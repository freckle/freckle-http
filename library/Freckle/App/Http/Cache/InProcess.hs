-- | An in-process, size-bounded cache of upstream HTTP responses
--
-- 'cacheGet' checks only the entry being looked up: if its own TTL has
-- already elapsed, it is evicted and treated as a miss. 'cacheSet' evicts
-- only when the total size is over budget, preferring an already-expired
-- entry over the least-recently-used one. Neither walks the whole cache on
-- every call. Call 'cacheReap' yourself (e.g. from a periodic background
-- thread) for more proactive reclamation than that.
module Freckle.App.Http.Cache.InProcess
  ( InProcessHttpCache
  , InProcessHttpCacheSettings (..)
  , defaultSettings
  , newInProcessHttpCache
  , inProcessHttpCache
  , inProcessHttpCacheSettings
  , cacheGet
  , Reap (..)
  , cacheGetReap
  , cacheSet
  , cacheDelete
  , cacheReap
  ) where

import Prelude

import Blammo.Logging (MonadLogger, logDebugNS, logWarnNS)
import Control.Exception.Annotated.UnliftIO (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.HashPSQ (HashPSQ)
import Data.HashPSQ qualified as HashPSQ
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Database.Memcache.Types (Key, Value)
import Freckle.App.Http.Cache
import Freckle.App.Http.Cache.Memcached (memcachedHttpCodec)
import Freckle.App.Memcached.CacheKey (fromCacheKey)
import Freckle.App.Memcached.CacheTTL (CacheTTL)
import UnliftIO (MonadUnliftIO)

-- | A monotonically increasing counter used as a recency priority
--
-- Smaller means less recently used.
type Tick = Int

data Entry = Entry
  { value :: Value
  , expiresAt :: UTCTime
  }

data CacheState = CacheState
  { byRecency :: HashPSQ Key Tick Entry
  , byExpiry :: HashPSQ Key UTCTime ()
  , totalBytes :: Int
  , nextTick :: Tick
  }

data InProcessHttpCache = InProcessHttpCache
  { ref :: IORef CacheState
  , maxBytes :: Int
  , clock :: IO UTCTime
  }

data InProcessHttpCacheSettings = InProcessHttpCacheSettings
  { maxBytes :: Int
  , clock :: IO UTCTime
  -- ^ What to use as "now". Tests can override this to simulate a TTL
  -- having elapsed without an actual delay.
  }

-- | 20MB, using the real clock
defaultSettings :: InProcessHttpCacheSettings
defaultSettings =
  InProcessHttpCacheSettings
    { maxBytes = 20 * 1024 * 1024
    , clock = getCurrentTime
    }

-- | Create an empty cache
newInProcessHttpCache
  :: MonadIO m => InProcessHttpCacheSettings -> m InProcessHttpCache
newInProcessHttpCache InProcessHttpCacheSettings {maxBytes, clock} = do
  ref <-
    liftIO $
      newIORef
        CacheState
          { byRecency = HashPSQ.empty
          , byExpiry = HashPSQ.empty
          , totalBytes = 0
          , nextTick = 0
          }
  pure InProcessHttpCache {ref, maxBytes, clock}

inProcessHttpCacheSettings
  :: (MonadLogger m, MonadUnliftIO m)
  => InProcessHttpCache
  -> CacheTTL
  -- ^ Default TTL, used when @max-age@ is not present
  -> HttpCacheSettings m Value
inProcessHttpCacheSettings cache defaultTTL =
  HttpCacheSettings
    { shared = True
    , cacheable = const True
    , cacheByHeaders = []
    , forceTTL = Nothing
    , defaultTTL
    , getCurrentTime = liftIO getCurrentTime
    , logDebug = logDebugNS "http.cache"
    , logWarn = logWarnNS "http.cache"
    , codec = memcachedHttpCodec
    , cache = inProcessHttpCache cache
    }

inProcessHttpCache :: MonadUnliftIO m => InProcessHttpCache -> HttpCache m Value
inProcessHttpCache cache =
  HttpCache
    { get = try . liftIO . cacheGet cache . fromCacheKey
    , set = \k v ttl -> try $ liftIO $ cacheSet cache (fromCacheKey k) v ttl
    , evict = try . liftIO . cacheDelete cache . fromCacheKey
    }

-- | Whether a 'cacheGetReap' call should evict an expired looked-up entry
data Reap = Reap | NoReap

cacheGet :: InProcessHttpCache -> Key -> IO (Maybe Value)
cacheGet = cacheGetReap Reap

-- | 'cacheGet', with the choice of whether it checks the looked-up entry's TTL
--
-- Real callers always want 'Reap' (that's what 'cacheGet' fixes it to);
-- 'NoReap' exists so tests can observe an expired entry's continued
-- presence, and 'cacheReap's effect on it, without 'cacheGet's own check
-- masking either.
cacheGetReap :: Reap -> InProcessHttpCache -> Key -> IO (Maybe Value)
cacheGetReap reap InProcessHttpCache {ref, clock} k = do
  now <- clock
  atomicModifyIORef' ref $ \state -> case HashPSQ.lookup k state.byRecency of
    Nothing -> (state, Nothing)
    Just (_tick, entry)
      | expired -> (removeKey k state, Nothing)
      | otherwise ->
          ( state
              { byRecency = HashPSQ.insert k state.nextTick entry state.byRecency
              , nextTick = state.nextTick + 1
              }
          , Just entry.value
          )
     where
      expired = case reap of
        Reap -> entry.expiresAt <= now
        NoReap -> False

cacheSet :: InProcessHttpCache -> Key -> Value -> CacheTTL -> IO ()
cacheSet InProcessHttpCache {ref, maxBytes, clock} k v ttl = do
  now <- clock
  let expiresAt = addUTCTime (fromIntegral ttl) now
  atomicModifyIORef' ref $ \state ->
    let
      oldSize = maybe 0 (BS.length . value . snd) $ HashPSQ.lookup k state.byRecency
      state1 =
        state
          { byRecency =
              HashPSQ.insert k state.nextTick Entry {value = v, expiresAt} state.byRecency
          , byExpiry = HashPSQ.insert k expiresAt () state.byExpiry
          , totalBytes = state.totalBytes - oldSize + BS.length v
          , nextTick = state.nextTick + 1
          }
    in
      (evictToFit maxBytes now state1, ())

cacheDelete :: InProcessHttpCache -> Key -> IO ()
cacheDelete InProcessHttpCache {ref} k =
  atomicModifyIORef' ref $ \state -> (removeKey k state, ())

-- | Remove every entry whose TTL has already elapsed
--
-- Unlike 'cacheGet' and 'cacheSet', which only ever look at what that one
-- call needs to, this walks the whole cache. Call it yourself, e.g.
-- periodically from your own background thread, for more proactive
-- reclamation than ordinary traffic gives you; this module does not run one
-- itself.
cacheReap :: InProcessHttpCache -> IO ()
cacheReap InProcessHttpCache {ref, clock} = do
  now <- clock
  atomicModifyIORef' ref $ \state -> (reapExpired now state, ())

-- | 'cacheReap's implementation: remove every entry whose TTL has elapsed
reapExpired :: UTCTime -> CacheState -> CacheState
reapExpired now = go
 where
  go state = case HashPSQ.findMin state.byExpiry of
    Just (k, expiresAt, ()) | expiresAt <= now -> go (removeKey k state)
    _ -> state

-- | Evict entries until under budget, preferring an already-expired one
evictToFit :: Int -> UTCTime -> CacheState -> CacheState
evictToFit maxBytes now = go
 where
  go state
    | state.totalBytes <= maxBytes = state
    | otherwise = case popExpired state of
        Just state' -> go state'
        Nothing -> case popLru state of
          Just state' -> go state'
          Nothing -> state

  popExpired state = case HashPSQ.findMin state.byExpiry of
    Just (k, expiresAt, ()) | expiresAt <= now -> Just $ removeKey k state
    _ -> Nothing

  popLru state = case HashPSQ.findMin state.byRecency of
    Just (k, _tick, _entry) -> Just $ removeKey k state
    Nothing -> Nothing

removeKey :: Key -> CacheState -> CacheState
removeKey k state =
  state
    { byRecency = HashPSQ.delete k state.byRecency
    , byExpiry = HashPSQ.delete k state.byExpiry
    , totalBytes = state.totalBytes - size
    }
 where
  size = maybe 0 (BS.length . value . snd) $ HashPSQ.lookup k state.byRecency
