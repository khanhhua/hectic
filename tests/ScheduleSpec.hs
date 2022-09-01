module ScheduleSpec where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Data

spec :: Spec
spec = do
  describe "Schedule as a monoid of free and busy days" $ do
    it "should join free schedules" $ do
      (free <> free) `shouldBe` free
    it "should replace free schedules" $ do
      let busyD = busyDay Monday (7, 0) (8, 30)
      (free <> busyD) `shouldBe` busyD

      let openD = openDay Monday (7, 0) (8, 30)
      (free <> openD) `shouldBe` openD
    it "should expand monthly busy schedule" $ do
      let
        busy1 = busyMonth January March
        busy2 = busyMonth March May
      (busy1 <> busy2) `shouldBe` busyMonth January May
    it "should expand weekly busy schedule" $ do
      let
        busy1 = busyDay Monday (7, 0) (8, 30)
        busy2 = busyDay Monday (8, 30) (9, 15)
      (busy1 <> busy2) `shouldBe` busyDay Monday (7, 0) (9, 15)
    it "should create series of busy days" $ do
      let
        busy1 = busyDay Monday (7, 0) (8, 0)
        busy2 = busyDay Monday (8, 30) (9, 15)
      (busy1 <> busy2) `shouldBe` Series [busy1, busy2]

  describe "Schedule as a monoid of open and busy days" $ do
    it "should create series of non-overlapping of open and busy days" $ do
      let
        open = openDay Monday (7, 0) (8, 0)
        busy = busyDay Monday (8, 30) (9, 15)
      (open <> busy) `shouldBe` Series [open, busy]

    it "should create series of open and busy months" $ do
      let
        open = openMonth January March
        busy = busyMonth March May
      (open <> busy) `shouldBe` Series [openMonth January February, busy]
