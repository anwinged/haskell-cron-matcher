module Constraint where

data Constraint = Constraint
  { lower :: Int
  , upper :: Int
  } deriving (Show, Eq)

makeRangeFromNumber :: Int -> Constraint
makeRangeFromNumber x = Constraint x x

inside :: (Int, Int) -> Constraint -> Bool
inside (x, y) (Constraint lower upper) = x >= lower && y <= upper

inRange :: Int -> Constraint -> Bool
inRange x = inside (x, x)
