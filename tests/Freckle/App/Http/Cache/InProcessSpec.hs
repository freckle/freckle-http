module Freckle.App.Http.Cache.InProcessSpec
  ( spec
  ) where

import Prelude

import Freckle.App.Http.Cache.InProcess
  ( cacheDelete
  , cacheGet
  , cacheSet
  , newInProcessHttpCache
  )
import Test.Hspec

spec :: Spec
spec = describe "InProcessHttpCache" $ do
  it "misses on an empty cache" $ do
    c <- newInProcessHttpCache 1024
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "returns a value after it is set" $ do
    c <- newInProcessHttpCache 1024
    cacheSet c "a" "hello" 300
    mv <- cacheGet c "a"
    mv `shouldBe` Just "hello"

  it "no longer returns a value once evicted" $ do
    c <- newInProcessHttpCache 1024
    cacheSet c "a" "hello" 300
    cacheDelete c "a"
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "still returns a value once its ttl has elapsed" $ do
    -- Staleness is the caller's concern (it round-trips `inserted`/`ttl`
    -- through the cached payload itself); this cache tracks expiry only to
    -- prioritize eviction, not to hide values from `get`
    c <- newInProcessHttpCache 1024
    cacheSet c "a" "hello" 0
    mv <- cacheGet c "a"
    mv `shouldBe` Just "hello"

  it "evicts the least-recently-used entry once over budget" $ do
    c <- newInProcessHttpCache 10
    cacheSet c "a" "aaaaa" 300 -- total: 5
    cacheSet c "b" "bbbbb" 300 -- total: 10
    cacheSet c "c" "ccccc" 300 -- total: 15, over budget; evicts "a"
    va <- cacheGet c "a"
    vb <- cacheGet c "b"
    vc <- cacheGet c "c"
    va `shouldBe` Nothing
    vb `shouldBe` Just "bbbbb"
    vc `shouldBe` Just "ccccc"

  it "treats a 'get' as refreshing an entry's recency" $ do
    c <- newInProcessHttpCache 10
    cacheSet c "a" "aaaaa" 300 -- total: 5
    cacheSet c "b" "bbbbb" 300 -- total: 10
    _ <- cacheGet c "a" -- "a" is now more-recently-used than "b"
    cacheSet c "c" "ccccc" 300 -- total: 15, over budget; evicts "b"
    va <- cacheGet c "a"
    vb <- cacheGet c "b"
    vc <- cacheGet c "c"
    va `shouldBe` Just "aaaaa"
    vb `shouldBe` Nothing
    vc `shouldBe` Just "ccccc"

  it "does not cache a single response larger than the whole budget" $ do
    c <- newInProcessHttpCache 10
    cacheSet c "a" "this value is way over budget" 300
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "evicts an expired entry ahead of a fresh, less-recently-used one" $ do
    c <- newInProcessHttpCache 10
    cacheSet c "a" "aaaaa" 0 -- total: 5, already expired
    cacheSet c "b" "bbbbb" 300 -- total: 10, fresh
    _ <- cacheGet c "a" -- "a" is now more-recently-used than "b"
    cacheSet c "c" "ccccc" 300 -- total: 15, over budget; evicts "a", not "b"
    va <- cacheGet c "a"
    vb <- cacheGet c "b"
    vc <- cacheGet c "c"
    va `shouldBe` Nothing
    vb `shouldBe` Just "bbbbb"
    vc `shouldBe` Just "ccccc"

  it "falls back to least-recently-used once no expired entries remain" $ do
    c <- newInProcessHttpCache 10
    cacheSet c "a" "aaaaa" 0 -- total: 5, already expired
    cacheSet c "b" "bbbbb" 300 -- total: 10, fresh
    cacheSet c "c" "ccccc" 300 -- total: 15, over budget; evicts "a" (expired)
    cacheSet c "d" "ddddd" 300 -- total: 15, over budget; no expired entries left, evicts "b" (LRU)
    va <- cacheGet c "a"
    vb <- cacheGet c "b"
    vc <- cacheGet c "c"
    vd <- cacheGet c "d"
    va `shouldBe` Nothing
    vb `shouldBe` Nothing
    vc `shouldBe` Just "ccccc"
    vd `shouldBe` Just "ddddd"
