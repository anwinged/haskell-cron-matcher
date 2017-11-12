module Field where

import           Constraint
import           Data.Char  (isDigit)
import           Data.Maybe
import           Helper

data Range
  = All
  | Range Int
          Int
  | Sequence [Int]
  deriving (Eq, Show)

data Step
  = Every
  | Step Int
  deriving (Eq, Show)

data Field =
  Field Range
        Step
  deriving (Eq, Show)

parseField :: String -> Constraint -> Maybe Field
parseField text = parseField' (wordsWhen (== '/') text)

parseField' :: [String] -> Constraint -> Maybe Field
parseField' [rangeText] constraint
  | isJust range = Just (Field (fromJust range) Every)
  | otherwise = Nothing
  where
    range = parseFieldRange rangeText constraint
parseField' [rangeText, stepText] constraint
  | isJust range && isJust step = Just (Field (fromJust range) (fromJust step))
  | otherwise = Nothing
  where
    range = parseFieldRange rangeText constraint
    step = parseFieldStep stepText
parseField' _ _ = Nothing

parseFieldRange :: String -> Constraint -> Maybe Range
parseFieldRange text constraint
  | text == "*" = Just All
  | isJust number = Just (Range (fromJust number) (fromJust number))
  | isJust range = Just (uncurry Range (fromJust range))
  | isJust sequence = fmap Sequence sequence
  where
    number = parseNumber text constraint
    range = parseRange text constraint
    sequence = parseSequence text constraint

isNumber :: String -> Bool
isNumber = all isDigit

parseNumber :: String -> Constraint -> Maybe Int
parseNumber text constraint
  | isValid = Just number
  | otherwise = Nothing
  where
    number = read text :: Int
    isValid = isNumber text && number `inRange` constraint

parseRange :: String -> Constraint -> Maybe (Int, Int)
parseRange text constraint
  | isValid = Just (start, end)
  | otherwise = Nothing
  where
    pieces = wordsWhen (== '-') text
    isTwo = length pieces == 2
    isAllNumbers = all isNumber pieces
    start = read (head pieces) :: Int
    end = read (pieces !! 1) :: Int
    isValid =
      isTwo &&
      isAllNumbers && start <= start && (start, end) `inside` constraint

parseSequence :: String -> Constraint -> Maybe [Int]
parseSequence text constraint
  | isValid = Just numbers
  | otherwise = Nothing
  where
    pieces = wordsWhen (== ',') text
    isAllNumbers = all isNumber pieces
    numbers = map read pieces
    allInRange = all (`inRange` constraint) numbers
    isValid = length pieces >= 2 && isAllNumbers && allInRange

parseFieldStep :: String -> Maybe Step
parseFieldStep "" = Just Every
parseFieldStep text
  | isNumber text = Just (Step (read text))
parseFieldStep _ = Nothing

matchField :: Field -> Int -> Bool
matchField (Field range step) n =
  matchFieldRange range n && matchFieldStep step n

matchFieldRange :: Range -> Int -> Bool
matchFieldRange All _           = True
matchFieldRange (Range x y) n   = n >= x && n <= y
matchFieldRange (Sequence xs) n = n `elem` xs

matchFieldStep :: Step -> Int -> Bool
matchFieldStep Every _    = True
matchFieldStep (Step x) n = n `mod` x == 0
