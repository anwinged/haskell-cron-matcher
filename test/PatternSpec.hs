module PatternSpec (main, spec) where

import Test.Hspec
import Data.Dates

import Pattern

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Cron pattern" $ do

    it "createParts" $
      length (createParts "* * * * * *") `shouldBe` 6

    it "matches fixed time" $
      let
        pattern = "* * * * * *"
        date = DateTime 2017 10 11 0 0 0
      in
        match pattern date `shouldBe` True

    it "matches all minutes" $
      let
        pattern = "* * * * * *"
        dates = [DateTime 2017 10 11 0 i 0 | i <- [0..59]]
      in
        countMatches pattern dates `shouldBe` 60

    it "matches exactly moment" $
      let
        date = (DateTime 2017 10 11 0 0 0)
        pattern = "0 0 11 10 * 2017"
      in
        match pattern date `shouldBe` True


countMatches :: String -> [DateTime] -> Int
countMatches p xs = sum $ map (f p) xs
  where
    f p d = case match p d of
      True -> 1
      False -> 0
