{-# LANGUAGE LambdaCase #-}
module KitsuComponents (parseLiteral, parseObjNotationLiteral, parsePrimitive, parseExpression) where

import KitsuByteCode

import qualified Text.Megaparsec as MP
import Text.Megaparsec ((<?>))
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Void ( Void )
import Data.Ratio


data TypeDefAttached a = TypeDefAttached [ClosureTypeDef] ClosureHashLookup a

instance Functor TypeDefAttached where
  fmap f (TypeDefAttached xs h a) = TypeDefAttached xs h (f a)

instance Applicative TypeDefAttached where
  pure = TypeDefAttached [] mempty
  TypeDefAttached xs h f <*> TypeDefAttached ys i a = TypeDefAttached (xs ++ ys) (h <> i) (f a)

instance Monad TypeDefAttached where
  return = pure
  m >>= f = (\case TypeDefAttached ys i (TypeDefAttached xs h a) -> TypeDefAttached (xs ++ ys) (h <> i) a) (f <$> m)

parseIntegral :: (Num a, Ord e) => MP.ParsecT e String m a
parseIntegral = foldl (\ s dig -> s*10 + dig) 0 <$> MP.some digit

digit :: (Num a, Ord e) => MP.ParsecT e String m a
digit = MP.label "0..9" $
  0 <$ MP.char '0' <|>
  1 <$ MP.char '1' <|>
  2 <$ MP.char '2' <|>
  3 <$ MP.char '3' <|>
  4 <$ MP.char '4' <|>
  5 <$ MP.char '5' <|>
  6 <$ MP.char '6' <|>
  7 <$ MP.char '7' <|>
  8 <$ MP.char '8' <|>
  9 <$ MP.char '9'

hex :: (Num a, Ord e) => MP.ParsecT e String m a
hex = MP.label "0x.." $
  0 <$ MP.char '0' <|>
  1 <$ MP.char '1' <|>
  2 <$ MP.char '2' <|>
  3 <$ MP.char '3' <|>
  4 <$ MP.char '4' <|>
  5 <$ MP.char '5' <|>
  6 <$ MP.char '6' <|>
  7 <$ MP.char '7' <|>
  8 <$ MP.char '8' <|>
  9 <$ MP.char '9' <|>
  10 <$ MP.oneOf "aA" <|>
  11 <$ MP.oneOf "bB" <|>
  12 <$ MP.oneOf "cC" <|>
  13 <$ MP.oneOf "dD" <|>
  14 <$ MP.oneOf "eE" <|>
  15 <$ MP.oneOf "fF"

escapedChar :: (Ord e) => MP.ParsecT e String m Char
escapedChar = MP.label "char" $
  '\n' <$ MP.string "\\n" <|>
  '\t' <$ MP.string "\\t" <|>
  '\\' <$ MP.string "\\\\" <|>
  '\'' <$ MP.string "\\'" <|>
  '\"' <$ MP.string "\\\"" <|>
  '\NUL' <$ MP.string "\\0" <|>
  MP.noneOf "\\\'\""

sign :: (Num a, Ord e) => MP.ParsecT e String m (a -> a)
sign = (id <$ MP.char '+') <|> (\x -> -x) <$ MP.char '-'

regularName :: (Ord e) => MP.ParsecT e String m String
regularName = MP.label "prop name" $ (:) <$> startChar <*> MP.many restChar
  where
    startChar = MP.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    restChar = MP.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

symbolicName :: (Ord e) => MP.ParsecT e String m String
symbolicName = MP.label "var name" $ MP.some $ MP.oneOf "$%^&*-+=<>|?!:,/~"

parseLiteral :: Ord e => MP.ParsecT e String m Literal
parseLiteral = MP.label "literal" $
  MP.try parseBool <|> -- raw-text
  MP.try parseByte <|> MP.try parseNat <|> -- unsigned-literal
  MP.try parseRat <|> MP.try parseInt <|> -- signed-literal
  parseChar -- string-literal
    where
      parseBool = (KitBool True <$ MP.string "true") <|> (KitBool False <$ MP.string "false")
      parseNat = KitNat <$> parseIntegral
      parseInt = KitInt <$> (sign <*> parseIntegral)
      parseRat = KitRat <$> (sign <*> ((%) <$> parseIntegral <*> (MP.space *> MP.char '%' *> MP.space *> parseIntegral)))
      parseByte = KitByte <$> ((\x' x -> 16*x' + x) <$> (MP.string "0x" *> hex) <*> hex)
      parseChar = KitChar <$> (MP.char '\'' *> escapedChar <* MP.char '\'')

parseObjNotationLiteral :: Ord e => MP.ParsecT e String m Literal
parseObjNotationLiteral = MP.label "object-notation-literal" $
  MP.try parseLiteral <|>
  parseRef -- reference for managing recursion in object notation
    where
      parseRef = KitClosureAddress <$> (MP.char '&' *> parseIntegral)

parsePrimitive :: Ord e => MP.ParsecT e String m Primitive
parsePrimitive = MP.label "primitive" $
  MP.try (KitAsync <$> (MP.string "async" *> MP.space1 *> parseExpression)) <|>
  KitAtomic <$> (MP.string "atomic" *> MP.space1 *> parseExpression)

-- *: Expression parser includes bracketed expressions.
-- TODO: FUTURE: parse `$` syntax for formatted expressions.
-- TODO: FUTURE: parse `do{...}` syntax for procedural logic.
parseExpression :: Ord e => MP.ParsecT e String m Expression
parseExpression = Lit (KitChar '\NUL') <$ MP.eof -- TODO: define this
