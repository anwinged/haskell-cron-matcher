module Constraint
  ( Constraint(..)
  , inRange
  , inside
  ) where

data Constraint =
  Constraint
    { lower :: Int
    , upper :: Int
    }
  deriving (Show, Eq)

inside :: (Int, Int) -> Constraint -> Bool
inside (x, y) (Constraint lw up) = x >= lw && y <= up

inRange :: Int -> Constraint -> Bool
inRange x = inside (x, x)
