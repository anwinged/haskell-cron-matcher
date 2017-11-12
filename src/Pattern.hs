module Pattern
  ( Pattern(..)
  , match
  , safeMatch
  , parse
  , check
  , createParts
  , parseField
  ) where

import           Constraint
import           Data.Dates
import           Data.Maybe
import           Field

data Pattern = Pattern
  { cminute :: Field
  , chour   :: Field
  , cday    :: Field
  , cmonth  :: Field
  , cweek   :: Field
  , cyear   :: Field
  }

match :: String -> DateTime -> Bool
match s d =
  case parse s of
    Just p  -> check p d
    Nothing -> error "Parse error"

safeMatch :: String -> DateTime -> Maybe Bool
safeMatch s d =
  case parse s of
    Just p  -> Just (check p d)
    Nothing -> Nothing

parse :: String -> Maybe Pattern
parse s
  | isInvalid = Nothing
  | otherwise = Just (createPattern $ catMaybes parts)
  where
    parts = createParts s
    isInvalid = not (checkParts parts)
    createPattern xs =
      Pattern
      { cminute = head xs
      , chour = xs !! 1
      , cday = xs !! 2
      , cmonth = xs !! 3
      , cweek = xs !! 4
      , cyear = xs !! 5
      }

createParts s = zipWith (curry f) parsers (words s)
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
check ptn date = all isRight pairs
  where
    pairs =
      [ (cminute ptn, minute date)
      , (chour ptn, hour date)
      , (cday ptn, day date)
      , (cmonth ptn, month date)
      , (cweek ptn, weekdayNumber $ dateWeekDay date)
      , (cyear ptn, year date)
      ]
    isRight (p, value) = matchField p value
