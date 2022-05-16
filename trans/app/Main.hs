module Main where

import Lib
import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)

main :: IO ()
main = do
    result <- MP.runParser (curleyBracketTree parseName <* optionalWhiteSpace <* MP.eof) "NO-FILE" <$> getLine
    case result of
        Left err -> putStrLn $ MP.errorBundlePretty err
        Right res -> putStrLn $ show res
