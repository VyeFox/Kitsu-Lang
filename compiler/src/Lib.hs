module Lib where

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

type Parser a = MP.Parsec Void String a

example :: Parser String
example = MP.string "abc"
