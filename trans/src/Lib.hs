module Lib where

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void ( Void )

type Parser a = MP.Parsec Void String a



newtype Name = Name String deriving (Show)

nameStart :: Parser String
nameStart = (<$>) (\c -> "_" ++ [c]) $ MP.oneOf (['a'..'z'] ++ ['A'..'Z'])

nameBodyStandard :: Parser String
nameBodyStandard = MP.label "namebody" $
    (<$>) (\c -> "_" ++ [c]) $ MP.oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

nameBodySymbolic :: Parser String
nameBodySymbolic = MP.label "namesymbols" $
    MP.try ("Su" <$ MP.string "_")
    <|> MP.try ("Am" <$ MP.string "-")
    <|> MP.try ("Ap" <$ MP.string "+")
    <|> ("As" <$ MP.string "*")

parseName :: Parser Name
parseName = MP.label "name" $
    (<$>) Name $
    (<$>) join $
    (:) <$> nameStart <*> MP.many (MP.try nameBodyStandard <|> nameBodySymbolic)



data Tree a = Leaf a | Node [Tree a] deriving (Show)

spaceChars :: [Char]
spaceChars = [' ', '\t', '\n']

optionalWhiteSpace :: Parser String
optionalWhiteSpace = MP.many (MP.oneOf spaceChars)

whiteSpace :: Parser String
whiteSpace = (:) <$> MP.oneOf spaceChars <*> optionalWhiteSpace

curleyBracketSet :: Parser a -> Parser [a]
curleyBracketSet p = MP.label "{}" (MP.try ([] <$ MP.string "{" <* optionalWhiteSpace <* MP.string "}"))
    <|> MP.label "{elems,... elem}" (MP.string "{" *> optionalWhiteSpace *>
    ((++) <$> MP.many (MP.try (p <* MP.string "," <* optionalWhiteSpace)) <*> (pure <$> p))
    <* optionalWhiteSpace <* MP.string "}")


curleyBracketTree :: Parser a -> Parser (Tree a)
curleyBracketTree p = MP.try (Leaf <$> p)
    <|> (Node <$> curleyBracketSet (curleyBracketTree p))
