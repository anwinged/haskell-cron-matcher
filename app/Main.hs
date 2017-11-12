module Main where

import           System.Environment (getArgs)
import           System.Exit
import           Text.Parsec.Error  (ParseError)

import           Data.Dates
import           Pattern

main :: IO ()
main = do
  args <- getArgs
  dt <- getCurrentDateTime
  exitWith $
    case processArgs args dt of
      Just True  -> ExitSuccess
      Just False -> ExitFailure 1
      Nothing    -> ExitFailure 2

processArgs :: [String] -> DateTime -> Maybe Bool
processArgs [ptn] dt       = safeMatch ptn dt
processArgs [ptn, time] dt = matchGivenTime ptn (parseDate dt time)
processArgs _ _            = Nothing

matchGivenTime :: String -> Either ParseError DateTime -> Maybe Bool
matchGivenTime _ (Left _)     = Nothing
matchGivenTime ptn (Right dt) = safeMatch ptn dt
