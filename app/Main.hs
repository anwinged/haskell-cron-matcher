module Main where

import System.Environment (getArgs)
import Text.Parsec.Error (ParseError)
import System.Exit

import Pattern
import Data.Dates

main :: IO ()
main = do
  args <- getArgs
  dt <- getCurrentDateTime
  case processArgs args dt of
    Just True -> exitWith ExitSuccess
    Just False -> exitWith (ExitFailure 1)
    Nothing -> exitWith (ExitFailure 2)

processArgs :: [String] -> DateTime -> Maybe Bool
processArgs [pattern] dt = safeMatch pattern dt
processArgs [pattern, time] dt = matchGivenTime pattern (parseDate dt time)
processArgs _ _ = Nothing

matchGivenTime :: String -> Either ParseError DateTime -> Maybe Bool
matchGivenTime _ (Left _) = Nothing
matchGivenTime pattern (Right dt) = safeMatch pattern dt
