module Lib
    ( check
    ) where

import Data.Bool
import Data.Dates

data Range = Any | Pair Int Int | Sequence [Int]

data Step = All | Value Int

data CronItemPattern = CronItemPattern Range Step

data CronPattern = CronPattern [CronItemPattern]

check :: DateTime -> String -> Bool
check d s = checkPattern d (parseCronPattern s)

parseCronPattern :: String -> CronPattern
parseCronPattern s = CronPattern $ map parseCronItemPattern $ words s

parseCronItemPattern :: String -> CronItemPattern
parseCronItemPattern "*" = CronItemPattern Any All

checkPattern :: DateTime -> CronPattern -> Bool
checkPattern d (CronPattern (x:xs)) = checkItemPattern (year d) x

checkItemPattern :: Int -> CronItemPattern -> Bool
checkItemPattern n (CronItemPattern Any All) = True