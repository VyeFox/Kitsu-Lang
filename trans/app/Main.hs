module Main where

import Lib

main :: IO ()
main = do
    result <- parseStr <$> getLine
    case result of
        Nothing -> putStrLn "invalid text"
        Just abc -> putStrLn $ show abc
