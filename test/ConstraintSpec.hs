module ConstraintSpec
  ( main
  , spec
  ) where

import           Constraint
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Constraint" $ do
    it "validate number" $ 10 `inRange` Constraint 0 10 `shouldBe` True
    it "validate number" $ 10 `inRange` Constraint 15 20 `shouldBe` False
