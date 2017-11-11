module ConstraintSpec (main, spec) where

import Test.Hspec

import Constraint

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Constraint" $ do
    it "can be created from number" $
      makeRangeFromNumber 10 `shouldBe` Constraint 10 10

    it "validate number" $
      10 `inRange` (Constraint 0 10) `shouldBe` True

    it "validate number" $
      10 `inRange` (Constraint 15 20) `shouldBe` False
