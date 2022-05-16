module Lib where

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import Data.Void

type Parser a = MP.Parsec Void String a



data Name = Name String deriving (Show)

nameStart :: Parser String
nameStart = (<$>) (\c -> "_" ++ [c]) $ MP.oneOf (['a'..'z'] ++ ['A'..'Z'])

nameBodyStandard :: Parser String
nameBodyStandard = (<$>) (\c -> "_" ++ [c]) $ MP.oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

nameBodySymbolic :: Parser String
nameBodySymbolic = ("Su" <$ MP.string "_")
               <|> ("Am" <$ MP.string "-")
               <|> ("Ap" <$ MP.string "+")
               <|> ("As" <$ MP.string "*")

parseName :: Parser Name
parseName = MP.label "name" $ (<$>) Name $ do
    start <- nameStart
    body <- MP.many $ nameBodyStandard <|> nameBodySymbolic
    return $ (<>) "userdef" $ foldl (<>) start body 



data Tree a = Leaf a | Node [Tree a] deriving (Show)

spaceChars :: [Char]
spaceChars = [' ', '\t', '\n']

optionalWhiteSpace :: Parser String
optionalWhiteSpace = MP.many (MP.oneOf spaceChars)

whiteSpace :: Parser String
whiteSpace = do
    head <- MP.oneOf spaceChars
    tail <- optionalWhiteSpace
    return (head:tail)

curleyBracketSet :: Parser a -> Parser [a]
curleyBracketSet p = (MP.label "{}" $ MP.try ([] <$ (MP.string "{") <* optionalWhiteSpace <* (MP.string "}"))) <|> (MP.label "{elems,... elem}" $ (MP.string "{") *> optionalWhiteSpace *> (do
    body <- MP.many (MP.try (p <* (MP.string ",") <* optionalWhiteSpace))
    head <- p
    return (body ++ [head])) <* (optionalWhiteSpace) <* (MP.string "}"))

curleyBracketTree :: Parser a -> Parser (Tree a)
curleyBracketTree p = MP.try (Leaf <$> p) <|> (Node <$> curleyBracketSet (curleyBracketTree p))
