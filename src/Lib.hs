module Lib
    ( match,
      parse,
      check,
      createParts
    ) where

import Data.Bool
import Data.Dates
import Data.Char (isDigit)
import Data.Maybe (catMaybes, isNothing)

data Range = Any | Pair Int Int | Sequence [Int]

data Step = All | Value Int

data Field = Field Range Step

data Pattern = Pattern {
    cminute :: Field,
    chour :: Field,
    cday :: Field,
    cmonth :: Field,
    cweek :: Field,
    cyear :: Field
}

match :: String -> DateTime -> Bool
match s d =  case parse s of
    Just p -> check p d
    Nothing -> error ""

safeMatch :: String -> DateTime -> Maybe Bool
safeMatch s d = case parse s of
    Just p -> Just (check p d)
    Nothing -> Nothing

parse :: String -> Maybe Pattern
parse s
    | isInvalid = Nothing
    | otherwise = Just (createPattern $ catMaybes parts)
    where
        parts = createParts s
        isInvalid = checkParts parts == False
        createPattern xs = Pattern {
            cminute = xs !! 0,
            chour = xs !! 1,
            cday = xs !! 2,
            cmonth = xs !! 3,
            cweek = xs !! 4,
            cyear = xs !! 5
        }

parsers = [parseMinute, parseHour, parseDay, parseMonth, parseWeek, parseYear]

createParts s = map f $ zip parsers (words s)
    where
        f (g, s) = g s

checkParts :: [Maybe Field] -> Bool
checkParts xs
    | length xs /= 6 = False
    | any isNothing xs = False
    | otherwise = True

parseField :: (Int, Int) -> String -> Maybe Field
parseField (f, t) s
    | s == "*" = Just (Field Any All)
    | validNumber == True = Just (Field (Pair x x) All)
    | otherwise = Nothing
    where 
        x = read s :: Int
        validNumber = all isDigit s && x >= f && x <= t

parseMinute = parseField (0, 59)
parseHour = parseField (0, 59)
parseDay = parseField (1, 31)
parseMonth = parseField (1, 12)
parseWeek = parseField (1, 7)
parseYear = parseField (0, 9999)

check :: Pattern -> DateTime -> Bool
check pattern date = all isRight pairs
    where 
        pairs = [ (cminute pattern, minute date),
                  (chour pattern, hour date),
                  (cday pattern, day date),
                  (cmonth pattern, month date),
                  (cweek pattern, weekdayNumber $ dateWeekDay date),
                  (cyear pattern, year date)
                ]
        isRight (pattern, value) = matchField pattern value 

matchField :: Field -> Int -> Bool
matchField (Field Any All) _ = True
matchField (Field (Pair f t) All) x = x >= f && x <= t