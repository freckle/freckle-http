-- | An in-process, size-bounded cache of upstream HTTP responses
--
-- Once the total cached size exceeds the configured budget, entries are
-- evicted to make room: an already-expired entry is evicted ahead of any
-- fresh one, and otherwise the least-recently-used entry is evicted.
module Freckle.App.Http.Cache.InProcess
  ( InProcessHttpCache
  , newInProcessHttpCache
  , inProcessHttpCache
  , inProcessHttpCacheSettings
  , cacheGet
  , cacheSet
  , cacheDelete
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
  }

-- | Create an empty cache with the given byte budget
newInProcessHttpCache :: MonadIO m => Int -> m InProcessHttpCache
newInProcessHttpCache maxBytes = do
  ref <-
    liftIO $
      newIORef
        CacheState
          { byRecency = HashPSQ.empty
          , byExpiry = HashPSQ.empty
          , totalBytes = 0
          , nextTick = 0
          }
  pure InProcessHttpCache {ref, maxBytes}

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

cacheGet :: InProcessHttpCache -> Key -> IO (Maybe Value)
cacheGet InProcessHttpCache {ref} k =
  atomicModifyIORef' ref $ \state -> case HashPSQ.lookup k state.byRecency of
    Nothing -> (state, Nothing)
    Just (_tick, entry) ->
      ( state
          { byRecency = HashPSQ.insert k state.nextTick entry state.byRecency
          , nextTick = state.nextTick + 1
          }
      , Just entry.value
      )

cacheSet :: InProcessHttpCache -> Key -> Value -> CacheTTL -> IO ()
cacheSet InProcessHttpCache {ref, maxBytes} k v ttl = do
  now <- getCurrentTime
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

-- | Evict entries until under budget, preferring already-expired ones
evictToFit :: Int -> UTCTime -> CacheState -> CacheState
evictToFit maxBytes now = go
 where
  go state
    | state.totalBytes <= maxBytes =
        state
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
