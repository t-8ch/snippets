module Main where

import Prelude hiding (readFile)

import Data.ByteString.Lazy (readFile)
import Data.Encoding (encodingFromString, decodeLazyByteString, DynEncoding)
import Text.Pandoc (writePlain, readHtml, def)
import System.Environment (getArgs)

defaultEncoding :: DynEncoding
defaultEncoding = encodingFromString "UTF-8"

parseArgs :: [String] -> (DynEncoding, FilePath)
parseArgs [     ] = error "missing argument"
parseArgs [f    ] = (defaultEncoding, f)
parseArgs [f, ""] = (defaultEncoding, f)
parseArgs [f,  e] = (encodingFromString e, f)
parseArgs _      = error "too many arguments"

convert :: String -> String
convert = writePlain def . readHtml def

main :: IO ()
main = do
        args <- getArgs
        let (e, f) = parseArgs args
        c <- readFile f
        putStrLn . convert $ decodeLazyByteString e c
