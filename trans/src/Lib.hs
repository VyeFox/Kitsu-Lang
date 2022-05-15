module Lib where

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$))
import qualified Text.Megaparsec.Char as MP

type Parser a = MP.Parsec String String a

data ABC = ABC deriving (Show)
parseABC :: Parser ABC
parseABC = ABC <$ MP.string "abc"

parseStr :: String -> Maybe ABC
parseStr = MP.parseMaybe parseABC