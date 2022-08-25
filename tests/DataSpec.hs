module DataSpec where

import Test.Hspec
import Data.List
import Data
import Control.Monad (forM_)

spec :: Spec
spec = do
  describe "Month" $ do
    it "show January" $ do
      show January `shouldBe` "January"
    it "should correctly sort months" $ do
      sort [February, January] `shouldBe` [January, February]
    it "should be enum" $ do
      forM_ [January .. March] (\enum -> (toEnum (fromEnum enum) :: Month) `shouldBe` enum)

  describe "Weekday" $ do
    it "show Sunday" $ do
      show Sunday `shouldBe` "Sunday"

  describe "DailyHourMinute" $ do
    it "show Sunday (09:00)" $ do
      show (DailyHourMinute Sunday 9 0) `shouldBe` "Sunday (09:00)"
    it "is an enum" $ do
      let allEnums = take 10080 [DailyHourMinute Sunday 0 0 .. ]
      forM_ allEnums (\enum -> (toEnum (fromEnum enum) :: DailyHourMinute) `shouldBe` enum)