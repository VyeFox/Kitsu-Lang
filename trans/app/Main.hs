module Main where

import Lib
import qualified Text.Megaparsec as MP

main :: IO ()
main = do
    result <- (MP.runParser (curleyBracketTree parseName) "NO-FILE") <$> getLine
    case result of
        Left err -> putStrLn $ MP.errorBundlePretty err
        Right abc -> putStrLn $ show abc
