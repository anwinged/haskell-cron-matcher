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

data CronItemPattern = CronItemPattern Range Step

data CronPattern = CronPattern {
    cminute :: CronItemPattern,
    chour :: CronItemPattern,
    cday :: CronItemPattern,
    cmonth :: CronItemPattern,
    cweek :: CronItemPattern,
    cyear :: CronItemPattern
}

match :: String -> DateTime -> Bool
match s d =  case parse s of
    Just p -> check p d
    Nothing -> error ""

safeMatch :: String -> DateTime -> Maybe Bool
safeMatch s d = case parse s of
    Just p -> Just (check p d)
    Nothing -> Nothing

parse :: String -> Maybe CronPattern
parse s
    | isInvalid = Nothing
    | otherwise = Just (createPattern $ catMaybes parts)
    where
        parts = createParts s
        isInvalid = checkParts parts == False
        createPattern xs = CronPattern {
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

checkParts :: [Maybe CronItemPattern] -> Bool
checkParts xs
    | length xs /= 6 = False
    | any isNothing xs = False
    | otherwise = True

parseCronItemPattern :: (Int, Int) -> String -> Maybe CronItemPattern
parseCronItemPattern (f, t) s
    | s == "*" = Just (CronItemPattern Any All)
    | validNumber == True = Just (CronItemPattern (Pair x x) All)
    | otherwise = Nothing
    where 
        x = read s :: Int
        validNumber = all isDigit s && x >= f && x <= t

parseMinute = parseCronItemPattern (0, 59)
parseHour = parseCronItemPattern (0, 59)
parseDay = parseCronItemPattern (1, 31)
parseMonth = parseCronItemPattern (1, 12)
parseWeek = parseCronItemPattern (1, 7)
parseYear = parseCronItemPattern (0, 9999)

check :: CronPattern -> DateTime -> Bool
check pattern date = all isRight pairs
    where 
        pairs = [ (cminute pattern, minute date),
                  (chour pattern, hour date),
                  (cday pattern, day date),
                  (cmonth pattern, month date),
                  (cweek pattern, weekdayNumber $ dateWeekDay date),
                  (cyear pattern, year date)
                ]
        isRight (pattern, value) = matchItemPattern pattern value 

matchItemPattern :: CronItemPattern -> Int -> Bool
matchItemPattern (CronItemPattern Any All) _ = True
matchItemPattern (CronItemPattern (Pair f t) All) x = x >= f && x <= t