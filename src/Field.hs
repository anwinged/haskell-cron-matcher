module Field
  ( module Field
  ) where

import           Constraint
import           Data.Maybe
import           Helper
import           Text.Read  (readMaybe)

data Field
  = Range Int Int Int
  | Sequence [Int]
  deriving (Eq, Show)

parseField :: String -> Constraint -> Maybe Field
parseField text constraint
  | isJust valueSequence = valueSequence
  | isJust valueRange = valueRange
  | otherwise = Nothing
  where
    valueRange = parseRange (splitIntoTwoWords (== '/') text) constraint
    valueSequence = parseSequence text constraint

parseRange :: (String, String) -> Constraint -> Maybe Field
parseRange (intervalText, stepText) constraint = do
  (from, to) <- parseRangeInterval intervalText constraint
  step <- parseRangeStep stepText
  return (Range from to step)

parseRangeInterval :: String -> Constraint -> Maybe (Int, Int)
parseRangeInterval "*" (Constraint lo up) = Just (lo, up)
parseRangeInterval text constraint = do
  (from, to) <- tbind (parsedFrom, parsedTo)
  if validToConstraint (from, to)
    then return (from, to)
    else Nothing
  where
    (textFrom, textTo) = splitIntoTwoWords (== '-') text
    (parsedFrom, parsedTo) = (readMaybe textFrom, readMaybe textTo)
    validToConstraint (start, end) =
      start <= end && (start, end) `inside` constraint

parseRangeStep :: String -> Maybe Int
parseRangeStep ""   = Just 1
parseRangeStep text = readMaybe text

parseSequence :: String -> Constraint -> Maybe Field
parseSequence text constraint = do
  numbers <- sequence $ map readMaybe $ wordsWhen (== ',') text
  if validToConstraint numbers
    then return $ Sequence numbers
    else Nothing
  where
    validToConstraint = all (`inRange` constraint)

matchField :: Field -> Int -> Bool
matchField (Range f t s) n = n >= f && n <= t && n `mod` s == 0
matchField (Sequence xs) n = n `elem` xs
