module FieldSpec (main, spec) where

import Test.Hspec
import Constraint
import Field

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Number" $ do
    it "can be parsed from string" $
      parseNumber "10" (Constraint 0 10) `shouldBe` Just 10

    it "can't be parsed from string" $
      parseNumber "10and10" (Constraint 0 10) `shouldBe` Nothing

    it "fails constraints" $
      parseNumber "10" (Constraint 0 5) `shouldBe` Nothing

  describe "Field can be created from" $ do
    it "asterisk" $
      parseField "*" (Constraint 0 0) `shouldBe` Just (Field All Every)

    it "number" $
      parseField "10" (Constraint 0 10) `shouldBe` Just (Field (Range 10 10) Every)

    it "range" $
      parseField "10-20" (Constraint 0 59) `shouldBe` Just (Field (Range 10 20) Every)

    it "sequence" $
      parseField "1,2,3" (Constraint 0 59) `shouldBe` Just (Field (Sequence [1, 2, 3]) Every)
