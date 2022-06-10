module Main where

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import Commons (rawpath, strpath)

main :: IO ()
main = getLine >>= (MP.parseTest (MP.try rawpath <|> strpath:: MP.Parsec Void String String))
