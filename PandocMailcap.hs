module Main where

import Prelude hiding (readFile)

import Data.ByteString.Lazy (readFile)
import Data.Encoding (encodingFromString, decodeLazyByteString, DynEncoding)
import Text.Pandoc (writePlain, readHtml, def, pandocVersion)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)

defaultEncoding :: DynEncoding
defaultEncoding = encodingFromString "UTF-8"

parseArgs :: [String] -> Maybe (DynEncoding, FilePath)
parseArgs [     ] = Nothing
parseArgs [f    ] = Just (defaultEncoding, f)
parseArgs [f, ""] = Just (defaultEncoding, f)
parseArgs [f,  e] = Just (encodingFromString e, f)
parseArgs _       = Nothing

convert :: String -> String
convert = writePlain def . readHtml def

printHelp :: IO ()
printHelp = do
    p <- getProgName
    putStrLn $ "Usage: " ++ p ++ " FILE [ENCODING] \nPandoc version: " ++ pandocVersion

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Nothing -> printHelp >> exitFailure
        Just (e, f) -> do
                c <- readFile f
                putStr $ convert $ decodeLazyByteString e c
                exitSuccess
