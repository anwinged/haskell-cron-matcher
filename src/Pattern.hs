module Pattern
  ( Pattern(..)
  , match
  , parse
  , check
  ) where

import           Constraint
import           Data.Dates
import           Field

data Pattern =
  Pattern
    { cminute :: Field
    , chour   :: Field
    , cday    :: Field
    , cmonth  :: Field
    , cweek   :: Field
    }
  deriving (Show)

match :: String -> DateTime -> Maybe Bool
match text datetime = fmap isMatched $ parse text
  where
    isMatched ptrn = check ptrn datetime

parse :: String -> Maybe Pattern
parse text = do
  xs <- sequence $ zipWith parseField (words text) constraints
  if length xs /= 5
    then Nothing
    else Just $
         Pattern
           { cminute = head xs
           , chour = xs !! 1
           , cday = xs !! 2
           , cmonth = xs !! 3
           , cweek = xs !! 4
           }

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
    isRight (patternField, value) = matchField patternField value
