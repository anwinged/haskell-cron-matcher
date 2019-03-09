module PatternSpec
  ( main
  , spec
  ) where

import           Data.Dates
import           Data.Maybe
import           Pattern
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Cron pattern" $ do
    it "matches fixed time" $
      let ptn = "* * * * *"
          date = DateTime 2017 10 11 0 0 0
       in match ptn date `shouldBe` Just True
    it "matches all minutes" $
      let ptn = "* * * * *"
          dates = [DateTime 2017 10 11 0 i 0 | i <- [0 .. 59]]
       in countMatches ptn dates `shouldBe` 60
    it "matches exactly moment" $
      let date = DateTime 2017 10 11 0 0 0
          ptn = "0 0 11 10 *"
       in match ptn date `shouldBe` Just True
    it "matches moment" $
      let date = DateTime 2017 10 10 12 10 0
          ptn = "* 12 * * *"
       in match ptn date `shouldBe` Just True

countMatches :: String -> [DateTime] -> Int
countMatches p xs = sum $ map (f p) xs
  where
    f x d =
      if isJust $ match x d
        then 1
        else 0
