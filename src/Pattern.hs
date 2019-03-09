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
        }

checkFields :: [Maybe Field] -> Bool
checkFields xs
  | length xs /= 5 = False
  | any isNothing xs = False
  | otherwise = True

constraints :: [Constraint]
constraints =
  [ Constraint 0 59
  , Constraint 0 23
  , Constraint 1 31
  , Constraint 1 12
  , Constraint 1 7
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
      ]
    isRight (p, value) = matchField p value
