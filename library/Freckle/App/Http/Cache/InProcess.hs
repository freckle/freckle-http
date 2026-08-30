-- | An in-process, size-bounded cache of upstream HTTP responses
--
-- Least-recently-used response bodies are evicted once their total size
-- exceeds the configured budget.
module Freckle.App.Http.Cache.InProcess
  ( InProcessHttpCache
  , newInProcessHttpCache
  , inProcessHttpCache
  , inProcessHttpCacheSettings
  ) where

import Prelude

import Blammo.Logging (MonadLogger, logDebugNS, logWarnNS)
import Control.Exception.Annotated.UnliftIO (try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS
import Data.Cache.LRU (LRU)
import Data.Cache.LRU qualified as LRU
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Time (getCurrentTime)
import Database.Memcache.Types (Key, Value)
import Freckle.App.Http.Cache
import Freckle.App.Http.Cache.Memcached (memcachedHttpCodec)
import Freckle.App.Memcached.CacheKey (fromCacheKey)
import Freckle.App.Memcached.CacheTTL (CacheTTL)
import UnliftIO (MonadUnliftIO)

data InProcessHttpCache = InProcessHttpCache
  { ref :: IORef (LRU Key Value, Int)
  , maxBytes :: Int
  }

-- | Create an empty cache with the given byte budget
newInProcessHttpCache :: MonadIO m => Int -> m InProcessHttpCache
newInProcessHttpCache maxBytes = do
  ref <- liftIO $ newIORef (LRU.newLRU Nothing, 0)
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
    , set = \k v _ttl -> try $ liftIO $ cacheSet cache (fromCacheKey k) v
    , evict = try . liftIO . cacheDelete cache . fromCacheKey
    }

cacheGet :: InProcessHttpCache -> Key -> IO (Maybe Value)
cacheGet InProcessHttpCache {ref} k =
  atomicModifyIORef' ref $ \(lru, total) ->
    let (lru', mv) = LRU.lookup k lru in ((lru', total), mv)

cacheSet :: InProcessHttpCache -> Key -> Value -> IO ()
cacheSet InProcessHttpCache {ref, maxBytes} k v =
  atomicModifyIORef' ref $ \(lru, total) ->
    let
      (lru0, mOld) = LRU.delete k lru
      total0 = total - maybe 0 BS.length mOld
      lru1 = LRU.insert k v lru0
      total1 = total0 + BS.length v
    in
      (evictToFit maxBytes lru1 total1, ())

cacheDelete :: InProcessHttpCache -> Key -> IO ()
cacheDelete InProcessHttpCache {ref} k =
  atomicModifyIORef' ref $ \(lru, total) ->
    let (lru', mOld) = LRU.delete k lru
    in  ((lru', total - maybe 0 BS.length mOld), ())

-- | Evict least-recently-used entries until under the given byte budget
evictToFit :: Int -> LRU Key Value -> Int -> (LRU Key Value, Int)
evictToFit maxBytes = go
 where
  go lru total
    | total <= maxBytes = (lru, total)
    | otherwise = case LRU.pop lru of
        (_, Nothing) -> (lru, total)
        (lru', Just (_, oldV)) -> go lru' (total - BS.length oldV)
