module FieldSpec (main, spec) where

import Foreign.Marshal.Utils (fromBool)
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

  -- Field validation

  describe "Field Range can be created from" $ do
    it "asterisk" $
      parseFieldRange "*" (Constraint 0 0) `shouldBe` Just All

    it "number" $
      parseFieldRange "10" (Constraint 0 10) `shouldBe` Just (Range 10 10)

    it "range" $
      parseFieldRange "10-20" (Constraint 0 59) `shouldBe` Just (Range 10 20)

    it "sequence" $
      parseFieldRange "1,2,3" (Constraint 0 59) `shouldBe` Just (Sequence [1, 2, 3])

  -- Field Step validation

  describe "Step can be created from" $ do
    it "empty string" $
      parseFieldStep "" `shouldBe` Just Every

    it "number" $
      parseFieldStep "5" `shouldBe` Just (Step 5)

  describe "Step cant'b created from" $ do
    it "word" $
      parseFieldStep "hello" `shouldBe` Nothing

  -- Field validation

  describe "Field can be created from" $ do
    it "asterisk" $
      parseField "*" (Constraint 0 59) `shouldBe` Just (Field All Every)

    it "asterisk with step" $ do
      parseField "*/5" (Constraint 0 59) `shouldBe` Just (Field All (Step 5))

    it "number with step" $ do
      parseField "10/5" (Constraint 0 59) `shouldBe` Just (Field (Range 10 10) (Step 5))

    it "range with step" $ do
      parseField "0-59/5" (Constraint 0 59) `shouldBe` Just (Field (Range 0 59) (Step 5))

    it "sequence with step" $ do
      parseField "1,3,4/5" (Constraint 0 59) `shouldBe` Just (Field (Sequence [1, 3, 4]) (Step 5))

  -- Field match

  describe "Field can match" $ do
    it "number" $
      count (Field All Every) [0..59] `shouldBe` 60

    it "range" $
      count (Field (Range 10 20) Every) [0..59] `shouldBe` 11

    it "range" $
      count (Field All (Step 10)) [0..59] `shouldBe` 6


count :: Field -> [Int] -> Int
count field values = sum $ map m values
  where m = fromBool . matchField field
