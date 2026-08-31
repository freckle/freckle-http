module Freckle.App.Http.Cache.InProcessSpec
  ( spec
  ) where

import Prelude

import Control.Concurrent (threadDelay)
import Freckle.App.Http.Cache.InProcess
  ( Reap (..)
  , cacheDelete
  , cacheGet
  , cacheGetReap
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

  it "never lets an entry that is already expired at insertion survive" $ do
    c <- newInProcessHttpCache 1024 -- plenty of headroom; budget pressure plays no part here
    cacheSet c "a" "aaaaa" 300 -- fresh
    cacheSet c "b" "bbbbb" 300 -- fresh
    cacheSet c "c" "ccccc" 0 -- already expired; reaped immediately as part of its own insertion
    va <- cacheGet c "a"
    vb <- cacheGet c "b"
    vc <- cacheGet c "c"
    va `shouldBe` Just "aaaaa"
    vb `shouldBe` Just "bbbbb"
    vc `shouldBe` Nothing

  it "reaps an entry that has expired once a different key is accessed" $ do
    c <- newInProcessHttpCache 1024
    cacheSet c "a" "hello" 1 -- expires in 1 second
    threadDelay 1_100_000 -- let it actually elapse, with no other access to the cache meanwhile
    cacheSet c "b" "world" 300 -- unrelated key, plenty of room; reaps "a" in passing
    mv <- cacheGetReap NoReap c "a" -- skip get's own reap, so only "b"'s set could have reaped it
    mv `shouldBe` Nothing

  it "leaves an entry that has expired since it was set until something reaps it" $ do
    c <- newInProcessHttpCache 1024
    cacheSet c "a" "hello" 1 -- expires in 1 second
    threadDelay 1_100_000 -- let it actually elapse, with no other access to the cache meanwhile
    stillThere <- cacheGetReap NoReap c "a" -- skip get's own reap, so its lingering presence shows
    stillThere `shouldBe` Just "hello"
    cacheReap c
    afterReap <- cacheGetReap NoReap c "a" -- skip get's own reap again, to isolate cacheReap's effect
    afterReap `shouldBe` Nothing

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

  it "reaps a stale entry, then still falls back to LRU for what's left" $ do
    c <- newInProcessHttpCache 10
    cacheSet c "a" "aaaaa" 0 -- total: 5; already expired, so it never survives its own insertion
    cacheSet c "b" "bbbbb" 300 -- total: 5 ("a" already gone), fresh
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
