module Main where

import Data.Default
import System.Environment
import System.Exit

import Text.ICalendar

err :: String -> IO ()
err m = putStrLn m >> exitFailure

main :: IO ()
main = do
    a <- getArgs
    case a of
        [filename] -> do
            r <- parseICalendarFile def filename
            case r of
                Left e              -> err e
                Right (_, [])       -> exitSuccess
                Right (_, warnings) -> err $ unlines warnings
        _          -> do
            p <- getProgName
            err $ "Usage: " ++ p ++ " <file>"
