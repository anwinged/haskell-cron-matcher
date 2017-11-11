module Pattern
    ( Pattern(..),
      match,
      parse,
      check,
      createParts,
      parseField
    ) where

import Data.Dates
import Data.Maybe

import Field
import Constraint

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

createParts s = map f $ zip parsers (words s)
    where
        f (g, s) = g s

checkParts :: [Maybe Field] -> Bool
checkParts xs
    | length xs /= 6 = False
    | any isNothing xs = False
    | otherwise = True

parseFieldAdapter :: Constraint -> String -> Maybe Field
parseFieldAdapter c t = parseField t c

parseMinute = parseFieldAdapter (Constraint 0 59)
parseHour = parseFieldAdapter (Constraint 0 59)
parseDay = parseFieldAdapter (Constraint 1 31)
parseMonth = parseFieldAdapter (Constraint 1 12)
parseWeek = parseFieldAdapter (Constraint 1 7)
parseYear = parseFieldAdapter (Constraint 0 9999)

parsers = [parseMinute, parseHour, parseDay, parseMonth, parseWeek, parseYear]

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
matchField (Field All Every) _ = True
matchField (Field (Range f t) Every) x = x >= f && x <= t
