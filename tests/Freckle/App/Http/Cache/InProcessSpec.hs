module Freckle.App.Http.Cache.InProcessSpec
  ( spec
  ) where

import Prelude

import Freckle.App.Http.Cache.InProcess
  ( cacheDelete
  , cacheGet
  , cacheReap
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

  it "no longer returns a value once its ttl has elapsed" $ do
    c <- newInProcessHttpCache 1024
    cacheSet c "a" "hello" 0
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "reaps an expired entry when a different key is accessed, even under budget" $ do
    c <- newInProcessHttpCache 1024
    cacheSet c "a" "hello" 0 -- expires immediately
    cacheSet c "b" "world" 300 -- unrelated key, plenty of room; reaps "a" in passing
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "removes an expired entry when explicitly reaped" $ do
    c <- newInProcessHttpCache 1024
    cacheSet c "a" "hello" 0 -- expires immediately
    cacheReap c
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

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

  it "evicts an already-expired entry ahead of any fresh one, even the newest" $ do
    c <- newInProcessHttpCache 10
    cacheSet c "a" "aaaaa" 300 -- total: 5, fresh
    cacheSet c "b" "bbbbb" 300 -- total: 10, fresh
    cacheSet c "c" "ccccc" 0 -- total: 15, over budget; already expired on arrival, evicted despite being newest
    va <- cacheGet c "a"
    vb <- cacheGet c "b"
    vc <- cacheGet c "c"
    va `shouldBe` Just "aaaaa"
    vb `shouldBe` Just "bbbbb"
    vc `shouldBe` Nothing

  it "falls back to least-recently-used once no expired entries remain" $ do
    c <- newInProcessHttpCache 10
    cacheSet c "a" "aaaaa" 0 -- total: 5; reaped as soon as "b" is set below
    cacheSet c "b" "bbbbb" 300 -- total: 5 (after reaping "a"), fresh
    cacheSet c "c" "ccccc" 300 -- total: 10, fresh; fits exactly
    cacheSet c "d" "ddddd" 300 -- total: 15, over budget; nothing expired, evicts "b" (LRU)
    va <- cacheGet c "a"
    vb <- cacheGet c "b"
    vc <- cacheGet c "c"
    vd <- cacheGet c "d"
    va `shouldBe` Nothing
    vb `shouldBe` Nothing
    vc `shouldBe` Just "ccccc"
    vd `shouldBe` Just "ddddd"
