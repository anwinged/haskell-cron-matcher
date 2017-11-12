module Main
  ( main
  ) where

import           Data.Dates
import           Pattern            (match)
import           System.Environment (getArgs)
import           System.Exit

main :: IO ()
main = do
  args <- getArgs
  currentDateTime <- getCurrentDateTime
  exitWith $
    case processArgs args currentDateTime of
      Just True  -> ExitSuccess
      Just False -> ExitFailure 1
      Nothing    -> ExitFailure 2

processArgs :: [String] -> DateTime -> Maybe Bool
processArgs [ptn] dt = match ptn dt
processArgs _ _      = Nothing
