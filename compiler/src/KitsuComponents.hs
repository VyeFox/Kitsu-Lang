{-# LANGUAGE LambdaCase #-}
module KitsuComponents (
  KitParseMonad,
  TypeDefAttached (TypeDefAttached), liftTypeDefAttached,
  parseLiteral,
  parseObjNotationLiteral,
  parsePrimitive,
  parseClosure,
  parseExpression
) where

import KitsuByteCode
import KitsuPreludeConnection (inline)

import qualified Text.Megaparsec as MP
import Text.Megaparsec ((<?>))
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Void ( Void )
import Data.Ratio ( (%) )
import Data.Functor.Compose (Compose (Compose), getCompose)

data TypeDefAttached a = TypeDefAttached [ClosureTypeDef] [ClosureTypeHash] a deriving (Show)

-- trivial derivations of functor typeclasses
instance Functor TypeDefAttached where
  fmap f (TypeDefAttached xs hs a) = TypeDefAttached xs hs (f a)
instance Applicative TypeDefAttached where
  pure = TypeDefAttached [] mempty
  TypeDefAttached xs hs f <*> TypeDefAttached ys is a = TypeDefAttached (xs ++ ys) (hs ++ is) (f a)
instance Monad TypeDefAttached where
  m >>= f = (\case TypeDefAttached ys is (TypeDefAttached xs hs a) -> TypeDefAttached (xs ++ ys) (hs ++ is) a) (f <$> m)
instance Semigroup a => Semigroup (TypeDefAttached a) where
  ma <> mb = (<>) <$> ma <*> mb
instance Foldable TypeDefAttached where
  foldMap f (TypeDefAttached _ _ a) = f a
instance Traversable TypeDefAttached where
  traverse f (TypeDefAttached xs hs a) = TypeDefAttached xs hs <$> f a

-- custom side effect interface
class Monad m => KitParseMonad m where
  liftTypeDefAttached :: TypeDefAttached a -> m a

instance KitParseMonad TypeDefAttached where
  liftTypeDefAttached = id

-- === SIMPLE PARSERS ===

parseIntegral :: (Num a, Ord e) => MP.Parsec e String a
parseIntegral = foldl (\ s dig -> s*10 + dig) 0 <$> MP.some digit

digit :: (Num a, Ord e) => MP.Parsec e String a
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

hex :: (Num a, Ord e) => MP.Parsec e String a
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

escapedChar :: (Ord e) => MP.Parsec e String Char
escapedChar = MP.label "char" $
  '\n' <$ MP.string "\\n" <|>
  '\t' <$ MP.string "\\t" <|>
  '\\' <$ MP.string "\\\\" <|>
  '\'' <$ MP.string "\\'" <|>
  '\"' <$ MP.string "\\\"" <|>
  '\NUL' <$ MP.string "\\0" <|>
  MP.noneOf "\\\'\""

sign :: (Num a, Ord e) => MP.Parsec e String (a -> a)
sign = (id <$ MP.char '+') <|> (\x -> -x) <$ MP.char '-'

textName :: (Ord e) => MP.Parsec e String String
textName = MP.label "prop name" $ (:) <$> startChar <*> MP.many restChar
  where
    startChar = MP.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
    restChar = MP.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

symbolicName :: (Ord e) => MP.Parsec e String String
symbolicName = MP.label "var name" $
  MP.notFollowedBy (MP.string "=>") *>
  MP.some (MP.oneOf "$%^&*-+=<>|?!:,/~")

-- === KITPARSEMONAD PARSERS ===

parseLiteral :: (Ord e, KitParseMonad m) => MP.Parsec e String (m Literal)
parseLiteral = MP.label "literal" $ (<$>) pure $
  MP.try parseBool <|>
  MP.try parseByte <|>
  MP.try parseNat <|>
  MP.try parseRat <|>
  MP.try parseInt <|>
  parseChar
    where
      parseBool = (KitBool True <$ MP.string "true") <|> (KitBool False <$ MP.string "false")
      parseNat = KitNat <$> parseIntegral
      parseInt = KitInt <$> (sign <*> parseIntegral)
      parseRat = KitRat <$> (sign <*> ((%) <$> parseIntegral <*> (MP.space *> MP.char '%' *> MP.space *> parseIntegral)))
      parseByte = KitByte <$> ((\x' x -> 16*x' + x) <$> (MP.string "0x" *> hex) <*> hex)
      parseChar = KitChar <$> (MP.char '\'' *> escapedChar <* MP.char '\'')

parseObjNotationLiteral :: (Ord e, KitParseMonad m) => MP.Parsec e String (m Literal)
parseObjNotationLiteral = MP.label "object-notation-literal" $
  MP.try parseLiteral <|>
  (pure <$> parseRef) -- reference for managing recursion in object notation
    where
      parseRef = KitClosureAddress <$> (MP.char '&' *> parseIntegral)

-- === SIDE-EFFECT INDUCING PARSERS ===

parsePrimitive :: (Ord e, KitParseMonad m) => MP.Parsec e String (m Literal) -> MP.Parsec e String (m Primitive)
parsePrimitive lit = MP.label "primitive" $
  MP.try ((<$>) KitAsync <$> (MP.string "async" *> MP.space1 *> parseExpression lit)) <|>
  (<$>) KitAtomic <$> (MP.string "atomic" *> MP.space1 *> parseExpression lit)

parseClosure :: (Ord e, KitParseMonad m) => MP.Parsec e String (m Literal) -> MP.Parsec e String (m Expression)
parseClosure lit = MP.label "closure" $
  MP.try constructed <|>
  MP.try initialTypeDef <|>
  jsobj
    where
      keyvalue = MP.label "key-value-pair" $ getCompose $ (,) <$> Compose ((<$>) pure $ textName <* MP.space) <*> Compose (MP.char ':' *> MP.space *> parseExpression lit)
      objectInner = getCompose $ (:) <$> Compose keyvalue <*> Compose (sequenceA <$> MP.many (MP.try $ MP.space *> MP.char ',' *> MP.space *> keyvalue))
      objectBody = MP.label "closure-body" $
        MP.try (MP.char '{' *> MP.space *> objectInner <* MP.space <* MP.char '}') <|>
        pure [] <$ MP.char '{' <* MP.space <* MP.char '}'
      function = MP.label "function-def" $ do
        typename <- textName
        MP.space1
        argname <- textName
        MP.space1
        MP.string "=>"
        MP.space1
        body <- parseExpression lit
        return $ join $ (<$>) liftTypeDefAttached $ TypeDefAttached <$> sequenceA [ClosureTypeDef typename "self" argname <$> body] <*> pure [ClosureTypeHash (typename, 0)] <*> pure typename
      constructed = getCompose $ Closure <$> Compose (pure <$> textName) <*> Compose objectBody
      initialTypeDef = do
        objbod <- objectBody
        MP.string "::"
        typename <- function
        return $ Closure <$> typename <*> objbod
      jsobj = (<$>) (Closure "Object") <$> objectBody

-- *: Expression parser includes bracketed expressions.
-- TODO: FUTURE: parse `$` syntax for formatted expressions.
-- TODO: FUTURE: parse `do{...}` syntax for procedural logic.
parseExpression :: (Ord e, KitParseMonad m) => MP.Parsec e String (m Literal) -> MP.Parsec e String (m Expression)
parseExpression lit =
  operatorfold
    where
      this = parseExpression lit -- expression parser recursively calls itself
      value =
        MP.try ((<$>) Lit <$> lit) <|>
        MP.try ((<$>) Prim <$> parsePrimitive lit)
      regularFunc =
        MP.try ((<$>) pure $ Name <$> textName) <|>
        MP.try (MP.char '(' *> (<$>) pure (Name <$> symbolicName) <* MP.char ')') <|>
        MP.char '(' *> MP.space *> this <* MP.space <* MP.char ')'
      inlineFunc = MP.label "operator" $
        MP.try ((<$>) pure $ Name <$> symbolicName) <|>
        MP.try (MP.char '`' *> (<$>) pure (Name <$> textName) <* MP.char '`') <|>
        MP.string "`(" *> MP.space *> this <* MP.space <* MP.string ")`"
      component = MP.label "value" $
        MP.try (parseClosure lit) <|>
        MP.try value <|>
        MP.try regularFunc
      currychain = do
        first <- component
        rest <- sequenceA <$> MP.many (MP.try $ MP.space1 *> component)
        return $ foldl Apply <$> first <*> rest
      operatorfold = do
        opfolds <- sequenceA <$> MP.many (MP.try $ getCompose $ (\lhs operator -> inline operator lhs) <$> Compose currychain <*> Compose (MP.space1 *> inlineFunc <* MP.space1))
        terminalchain <- currychain
        return $ foldr ($) <$> terminalchain <*> opfolds

