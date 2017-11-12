module Pattern
  ( Pattern(..)
  , match
  , parse
  , check
  , createFields
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
  } deriving (Show)

match :: String -> DateTime -> Maybe Bool
match s d =
  case parse s of
    Just p  -> Just (check p d)
    Nothing -> Nothing

parse :: String -> Maybe Pattern
parse s
  | isInvalid = Nothing
  | otherwise = Just (createPattern $ catMaybes parts)
  where
    parts = createFields s
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

createFields :: String -> [Maybe Field]
createFields text = zipWith (curry f) parsers (words text)
  where
    f (parser, s) = parser s

checkParts :: [Maybe Field] -> Bool
checkParts xs
  | length xs /= 6 = False
  | any isNothing xs = False
  | otherwise = True

parseFieldAdapter :: Constraint -> String -> Maybe Field
parseFieldAdapter c t = parseField t c

parseMinute :: String -> Maybe Field
parseMinute = parseFieldAdapter (Constraint 0 59)

parseHour :: String -> Maybe Field
parseHour = parseFieldAdapter (Constraint 0 23)

parseDay :: String -> Maybe Field
parseDay = parseFieldAdapter (Constraint 1 31)

parseMonth :: String -> Maybe Field
parseMonth = parseFieldAdapter (Constraint 1 12)

parseWeek :: String -> Maybe Field
parseWeek = parseFieldAdapter (Constraint 1 7)

parseYear :: String -> Maybe Field
parseYear = parseFieldAdapter (Constraint 0 9999)

parsers :: [String -> Maybe Field]
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
