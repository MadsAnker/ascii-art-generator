module Main where

import           Lib                (writeAsciiImage)
import           System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
      Just imagePath -> writeAsciiImage imagePath
      Nothing        -> putStrLn "Usage:"

parseArgs :: [String] -> Maybe String
parseArgs [x] = Just x
parseArgs _   = Nothing
