module Field
  ( module Field
  ) where

import           Constraint
import           Data.Char  (isDigit)
import           Data.Maybe
import           Helper

data Field
  = Range Int
          Int
          Int
  | Sequence [Int]
  deriving (Eq, Show)

parseField :: String -> Constraint -> Maybe Field
parseField text constraint
  | isJust valueSequence = valueSequence
  | isJust valueRange = valueRange
  | otherwise = Nothing
  where
    valueRange = parseRange (splitRange text) constraint
    valueSequence = parseSequence text constraint

splitRange :: String -> [String]
splitRange text = take 2 $ wordsWhen (== '/') text ++ ["", ""]

isNumber :: String -> Bool
isNumber ""   = False
isNumber text = all isDigit text

parseRange :: [String] -> Constraint -> Maybe Field
parseRange [interval, step] constraint = do
  (from, to) <- parseInterval interval constraint
  step' <- parseStep step
  return (Range from to step')
parseRange _ _ = Nothing

parseInterval :: String -> Constraint -> Maybe (Int, Int)
parseInterval "*" (Constraint lo up) = Just (lo, up)
parseInterval text constraint
  | isValid = Just (start, end)
  | otherwise = Nothing
  where
    pieces = take 2 $ wordsWhen (== '-') text ++ ["", ""]
    isNumbers = all isNumber pieces
    [start, end] = map read pieces
    isValid = isNumbers && start <= end && (start, end) `inside` constraint

parseStep :: String -> Maybe Int
parseStep "" = Just 1
parseStep text
  | isNumber text = Just (read text)
parseStep _ = Nothing

parseSequence :: String -> Constraint -> Maybe Field
parseSequence text constraint
  | isValid = Just (Sequence numbers)
  | otherwise = Nothing
  where
    pieces = wordsWhen (== ',') text
    isNumbers = all isNumber pieces
    numbers = map read pieces
    allInRange = all (`inRange` constraint) numbers
    isValid = not (null pieces) && isNumbers && allInRange

matchField :: Field -> Int -> Bool
matchField (Range f t s) n = n >= f && n <= t && n `mod` s == 0
matchField (Sequence xs) n = n `elem` xs
