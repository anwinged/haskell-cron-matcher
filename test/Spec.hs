import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Data.Dates

import Lib (check)

main :: IO ()
main = hspec $ do
  describe "Cron ckecker" $ do
    it "matches asterisks" $ do
      check (DateTime 2017 10 11 0 0 0) "* * * * *" `shouldBe` (True :: Bool)

