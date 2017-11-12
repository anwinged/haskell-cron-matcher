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
  | isJust range = Just (Field (fromJust range) Every)
  | otherwise = Nothing
  where
    range = parseFieldRange text constraint

parseFieldRange :: String -> Constraint -> Maybe Range
parseFieldRange text constraint
  | isAll = Just All
  | isJust number = Just (Range (fromJust number) (fromJust number))
  | isJust range = Just (Range (fst $ fromJust range) (snd $ fromJust range))
  | isJust sequence = fmap Sequence sequence
  where
    isAll = parseAll text
    number = parseNumber text constraint
    range = parseRange text constraint
    sequence = parseSequence text constraint

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

parseSequence :: String -> Constraint -> Maybe [Int]
parseSequence text constraint
  | isValid = Just numbers
  | otherwise = Nothing
  where
    pieces = wordsWhen (== ',') text
    isAllNumbers = all isNumber pieces
    numbers = map read pieces
    allInRange = all (\x -> x `inRange` constraint) numbers
    isValid = length pieces >= 2 && isAllNumbers && allInRange
