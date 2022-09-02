module QuerySpec where

import Test.Hspec ( describe, it, shouldBe, Spec )
import Data

spec :: Spec
spec = do
  describe "Flat Query" $ do
    it "should fetch busy days" $ do
      let
        schedule = free()
        q = query Montag
        result = runQuery q schedule 

