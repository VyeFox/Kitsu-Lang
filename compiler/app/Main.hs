module Main where

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

import Lib (example)

main :: IO ()
main = getLine >>= MP.parseTest (example <* MP.eof)
