module DataSpec where

import Test.Hspec
import Data.List
import Data

spec :: Spec
spec = do
  describe "Month" $ do
    it "show January" $ do
      show January `shouldBe` "January"
    it "should correctly sort months" $ do
      sort [February, January] `shouldBe` [January, February]
    it "should be enum" $ do
      [January .. March] `shouldBe` [January, February, March]

  describe "Weekday" $ do
    it "show Sunday" $ do
      show Sunday `shouldBe` "Sunday"

  describe "DailyHourMinute" $ do
    it "show Sunday (09:00)" $ do
      show (DailyHourMinute Sunday 9 0) `shouldBe` "Sunday (09:00)"