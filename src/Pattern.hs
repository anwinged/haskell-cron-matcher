module Pattern
  ( Pattern(..)
  , match
  , parse
  , check
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
parse text
  | isValid = Just (createPattern $ catMaybes fields)
  | otherwise = Nothing
  where
    fields = zipWith parseField (words text) constraints
    isValid = checkFields fields
    createPattern xs =
      Pattern
      { cminute = head xs
      , chour = xs !! 1
      , cday = xs !! 2
      , cmonth = xs !! 3
      , cweek = xs !! 4
      , cyear = xs !! 5
      }

checkFields :: [Maybe Field] -> Bool
checkFields xs
  | length xs /= 6 = False
  | any isNothing xs = False
  | otherwise = True

constrainMinute :: Constraint
constrainMinute = Constraint 0 59

constrainHour :: Constraint
constrainHour = Constraint 0 23

constrainDay :: Constraint
constrainDay = Constraint 1 31

constrainMonth :: Constraint
constrainMonth = Constraint 1 12

constrainWeek :: Constraint
constrainWeek = Constraint 1 7

constrainYear :: Constraint
constrainYear = Constraint 0 9999

constraints :: [Constraint]
constraints =
  [ constrainMinute
  , constrainHour
  , constrainDay
  , constrainMonth
  , constrainWeek
  , constrainYear
  ]

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
