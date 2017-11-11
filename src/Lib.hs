module Lib
    ( match
    ) where

import Data.Bool
import Data.Dates

data Range = Any | Pair Int Int | Sequence [Int]

data Step = All | Value Int

data CronItemPattern = CronItemPattern Range Step

data CronPattern = CronPattern [CronItemPattern]

match :: String -> DateTime -> Bool
match s d = matchPattern (parseCronPattern s) d

parseCronPattern :: String -> CronPattern
parseCronPattern s = CronPattern $ map parseCronItemPattern $ words s

parseCronItemPattern :: String -> CronItemPattern
parseCronItemPattern "*" = CronItemPattern Any All

matchPattern :: CronPattern -> DateTime -> Bool
matchPattern (CronPattern (x:xs)) d = matchItemPattern x (year d)

matchItemPattern :: CronItemPattern -> Int -> Bool
matchItemPattern (CronItemPattern Any All) _ = True