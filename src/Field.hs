module Field where

import Data.Char (isDigit)
import Data.Maybe

import Constraint
import Helper

data Range = All | Range Int Int | Sequence [Int]
  deriving (Eq, Show)

data Step = Every | Step Int
  deriving (Eq, Show)

data Field = Field Range Step
  deriving (Eq, Show)

parseField :: String -> Constraint -> Maybe Field
parseField text constraint
  | isAll = Just (Field All Every)
  | isNumber = Just (Field (Range number number) Every)
  | isRange = Just (Field (Range leftBound rightBound) Every)
  | otherwise = Nothing
  where
    -- All
    isAll = parseAll text
    -- Number
    numberParseResult = parseNumber text constraint
    isNumber = isJust $ numberParseResult
    number = fromJust numberParseResult
    -- Range
    rangeParseResult = parseRange text constraint
    isRange = isJust $ rangeParseResult
    rangeValues (Just p) = p
    leftBound = fst (rangeValues rangeParseResult)
    rightBound = snd (rangeValues rangeParseResult)
    -- -- Sequence
    -- matchSequence = matchRegex (mkRegex "(([0-9]+)[, ]?)+") s

parseAll :: String -> Bool
parseAll "*" = True
parseAll _ = False

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
    start = read (pieces !! 0) :: Int
    end = read (pieces !! 1) :: Int
    isValid = isTwo && isAllNumbers && start <= start && (start, end) `inside` constraint
