module HelperSpec
  ( main
  , spec
  ) where

import           Helper
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Splitting" $ do
    it "can process empty string" $ wordsWhen (== '-') "" `shouldBe` []
    it "can process only one word" $ wordsWhen (== '-') "10" `shouldBe` ["10"]
    it "can separated by '-'" $
      wordsWhen (== '-') "10-20" `shouldBe` ["10", "20"]
    it "can be separated by ','" $
      wordsWhen (== ',') "10,20,30" `shouldBe` ["10", "20", "30"]
