module Freckle.App.Http.Cache.InProcessSpec
  ( spec
  ) where

import Prelude

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time (UTCTime, addUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Freckle.App.Http.Cache.InProcess
  ( InProcessHttpCacheSettings (..)
  , Reap (..)
  , cacheDelete
  , cacheGet
  , cacheGetReap
  , cacheReap
  , cacheSet
  , defaultSettings
  , newInProcessHttpCache
  )
import Test.Hspec

spec :: Spec
spec = describe "InProcessHttpCache" $ do
  it "misses on an empty cache" $ do
    c <- newInProcessHttpCache defaultSettings {maxBytes = 1024}
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "returns a value after it is set" $ do
    c <- newInProcessHttpCache defaultSettings {maxBytes = 1024}
    cacheSet c "a" "hello" 300
    mv <- cacheGet c "a"
    mv `shouldBe` Just "hello"

  it "no longer returns a value once evicted" $ do
    c <- newInProcessHttpCache defaultSettings {maxBytes = 1024}
    cacheSet c "a" "hello" 300
    cacheDelete c "a"
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "no longer returns a value once its own ttl has elapsed" $ do
    c <- newInProcessHttpCache defaultSettings {maxBytes = 1024}
    cacheSet c "a" "hello" 0
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "does not check any other key's ttl when getting one key" $ do
    c <- newInProcessHttpCache defaultSettings {maxBytes = 1024}
    cacheSet c "a" "hello" 0 -- already expired
    cacheSet c "b" "world" 300 -- fresh, unrelated
    _ <- cacheGet c "b" -- looks up "b" only
    mv <- cacheGetReap NoReap c "a" -- skip get's own check, to see whether "b"'s get touched "a"
    mv `shouldBe` Just "hello" -- untouched; still there, even though already expired

  it "leaves an entry that has expired since it was set until something reaps it" $ do
    clockRef <- newIORef epoch
    c <-
      newInProcessHttpCache
        defaultSettings {maxBytes = 1024, clock = readIORef clockRef}
    cacheSet c "a" "hello" 1 -- expires 1 second after epoch
    writeIORef clockRef (addUTCTime 2 epoch) -- simulate 2 seconds passing, no delay needed
    stillThere <- cacheGetReap NoReap c "a" -- skip get's own check, so its lingering presence shows
    stillThere `shouldBe` Just "hello"
    cacheReap c
    afterReap <- cacheGetReap NoReap c "a" -- skip get's own check again, to isolate cacheReap's effect
    afterReap `shouldBe` Nothing

  it "evicts the least-recently-used entry once over budget" $ do
    c <- newInProcessHttpCache defaultSettings {maxBytes = 10}
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
    c <- newInProcessHttpCache defaultSettings {maxBytes = 10}
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
    c <- newInProcessHttpCache defaultSettings {maxBytes = 10}
    cacheSet c "a" "this value is way over budget" 300
    mv <- cacheGet c "a"
    mv `shouldBe` Nothing

  it "evicts an already-expired entry ahead of the least-recently-used one" $ do
    c <- newInProcessHttpCache defaultSettings {maxBytes = 10}
    cacheSet c "a" "aaaaa" 300 -- total: 5, fresh, least-recently-touched
    cacheSet c "b" "bbbbb" 0 -- total: 10, already expired, but more-recently-touched than "a"
    cacheSet c "c" "ccccc" 300 -- total: 15, over budget; evicts "b" (expired), not "a" (LRU tail)
    va <- cacheGetReap NoReap c "a"
    vb <- cacheGetReap NoReap c "b" -- skip get's own check, to isolate the set-time eviction
    vc <- cacheGetReap NoReap c "c"
    va `shouldBe` Just "aaaaa"
    vb `shouldBe` Nothing
    vc `shouldBe` Just "ccccc"

  it "falls back to least-recently-used once no expired entries remain" $ do
    c <- newInProcessHttpCache defaultSettings {maxBytes = 10}
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

epoch :: UTCTime
epoch = posixSecondsToUTCTime 0
