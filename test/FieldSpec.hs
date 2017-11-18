module FieldSpec
  ( main
  , spec
  ) where

import           Constraint
import           Field
import           Foreign.Marshal.Utils (fromBool)
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Field can be created from" $ do
    it "asterisk" $
      parseField "*" (Constraint 0 59) `shouldBe` Just (Range 0 59 1)
    it "asterisk" $
      parseField "*" (Constraint 0 0) `shouldBe` Just (Range 0 0 1)
    it "asterisk with step" $
      parseField "*/5" (Constraint 0 59) `shouldBe` Just (Range 0 59 5)
    it "number" $
      parseField "10" (Constraint 0 10) `shouldBe` Just (Sequence [10])
    it "range" $
      parseField "10-20" (Constraint 0 59) `shouldBe` Just (Range 10 20 1)
    it "range with step" $
      parseField "0-59/5" (Constraint 0 59) `shouldBe` Just (Range 0 59 5)
    it "sequence" $
      parseField "1,2,3" (Constraint 0 59) `shouldBe` Just (Sequence [1, 2, 3])
  -- Field negative cases
  describe "Field can't be created from" $ do
    it "number with step" $
      parseField "10/5" (Constraint 0 59) `shouldBe` Nothing
    it "sequence with step" $
      parseField "1,3,4/5" (Constraint 0 59) `shouldBe` Nothing
    it "can't be parsed from string" $
      parseField "10and10" (Constraint 0 10) `shouldBe` Nothing
    it "failed constraints" $
      parseField "10" (Constraint 0 5) `shouldBe` Nothing
  -- Field match
  describe "Field can match" $ do
    it "number" $ count (Range 0 59 1) [0 .. 59] `shouldBe` 60
    it "range" $ count (Range 10 20 1) [0 .. 59] `shouldBe` 11
    it "range" $ count (Range 0 59 10) [0 .. 59] `shouldBe` 6

count :: Field -> [Int] -> Int
count field values = sum $ map m values
  where
    m = fromBool . matchField field
