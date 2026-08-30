{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module Freckle.App.Http.Cache.InProcessSpec
  ( spec
  ) where

import Prelude

import Data.ByteString (ByteString)
import Data.Text (Text)
import Freckle.App.Http.Cache (HttpCache (..))
import Freckle.App.Http.Cache.InProcess
  ( inProcessHttpCache
  , newInProcessHttpCache
  )
import Freckle.App.Memcached.CacheKey (CacheKey, cacheKeyThrow)
import Freckle.App.Memcached.CacheTTL (cacheTTL)
import Test.Hspec

spec :: Spec
spec = describe "InProcessHttpCache" $ do
  it "misses on an empty cache" $ do
    c <- cacheOps 1024
    k <- key "a"
    Right mv <- c.get k
    mv `shouldBe` Nothing

  it "returns a value after it is set" $ do
    c <- cacheOps 1024
    k <- key "a"
    _ <- c.set k "hello" (cacheTTL 60)
    Right mv <- c.get k
    mv `shouldBe` Just "hello"

  it "no longer returns a value once evicted" $ do
    c <- cacheOps 1024
    k <- key "a"
    _ <- c.set k "hello" (cacheTTL 60)
    _ <- c.evict k
    Right mv <- c.get k
    mv `shouldBe` Nothing

  it "evicts the least-recently-used entry once over budget" $ do
    c <- cacheOps 10
    ka <- key "a"
    kb <- key "b"
    kc <- key "c"
    _ <- c.set ka "aaaaa" (cacheTTL 60) -- total: 5
    _ <- c.set kb "bbbbb" (cacheTTL 60) -- total: 10
    _ <- c.set kc "ccccc" (cacheTTL 60) -- total: 15, over budget; evicts "a"
    Right va <- c.get ka
    Right vb <- c.get kb
    Right vc <- c.get kc
    va `shouldBe` Nothing
    vb `shouldBe` Just "bbbbb"
    vc `shouldBe` Just "ccccc"

  it "treats a 'get' as refreshing an entry's recency" $ do
    c <- cacheOps 10
    ka <- key "a"
    kb <- key "b"
    kc <- key "c"
    _ <- c.set ka "aaaaa" (cacheTTL 60) -- total: 5
    _ <- c.set kb "bbbbb" (cacheTTL 60) -- total: 10
    _ <- c.get ka -- "a" is now more-recently-used than "b"
    _ <- c.set kc "ccccc" (cacheTTL 60) -- total: 15, over budget; evicts "b"
    Right va <- c.get ka
    Right vb <- c.get kb
    Right vc <- c.get kc
    va `shouldBe` Just "aaaaa"
    vb `shouldBe` Nothing
    vc `shouldBe` Just "ccccc"

  it "does not cache a single response larger than the whole budget" $ do
    c <- cacheOps 10
    k <- key "a"
    _ <- c.set k "this value is way over budget" (cacheTTL 60)
    Right mv <- c.get k
    mv `shouldBe` Nothing
 where
  key :: Text -> IO CacheKey
  key = cacheKeyThrow

  cacheOps :: Int -> IO (HttpCache IO ByteString)
  cacheOps maxBytes = inProcessHttpCache <$> newInProcessHttpCache maxBytes
