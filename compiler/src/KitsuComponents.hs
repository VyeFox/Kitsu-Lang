module KitsuComponents (
  textName,
  symbolicName,
  KitParseMonad (..),
  TypeDefAttached (..),
  parsePrimitive,
  parseClosure,
  parseExpression
) where

import KitsuByteCode
import KitsuPreludeConnection (inline)
import KitsuSeasoning (Seasoning(..), KitParseMonad(..), TypeDefAttached(..))

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Functor.Compose (Compose (Compose), getCompose)

-- === SIMPLE PARSERS ===

textName :: (Ord e) => MP.Parsec e String String
textName = MP.label "name" $ (:) <$> startChar <*> MP.many restChar
  where
    startChar = MP.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" -- underscore is reserved for internal use
    restChar = MP.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789"

symbolicName :: (Ord e) => MP.Parsec e String String
symbolicName = MP.label "symbol" $
  MP.notFollowedBy (MP.string "=>") *>
  MP.notFollowedBy (MP.string "::") *>
  MP.some (MP.oneOf "$%^&*-+=<>[]|?!:/~")

-- === SIDE-EFFECT INDUCING PARSERS ===

parsePrimitive :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String (m Primitive)
parsePrimitive seasoning = MP.label "primitive" $
  MP.try ((<$>) KitAsync <$> (MP.string "async" *> MP.space1 *> parseExpression seasoning)) <|>
  (<$>) KitAtomic <$> (MP.string "atomic" *> MP.space1 *> parseExpression seasoning)

-- TODO: ...Till refactor aware.
parseClosure :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String (m Expression)
parseClosure seasoning = MP.label "closure" $
  MP.try constructed <|>
  MP.try initialTypeDef <|>
  MP.try lambda <|>
  jsobj
    where
      keyvalue = MP.label "key-value-pair" $ getCompose $ (,) <$> Compose ((<$>) pure $ textName <* MP.space) <*> Compose (MP.char ':' *> MP.space *> parseExpression seasoning)
      objectInner = getCompose $ (:) <$> Compose keyvalue <*> Compose (sequenceA <$> MP.many (MP.try $ MP.space *> MP.char ',' *> MP.space *> keyvalue))
      objectBody = MP.label "closure-body" $
        MP.try (MP.char '{' *> MP.space *> objectInner <* MP.space <* MP.char '}') <|>
        pure [] <$ MP.char '{' <* MP.space <* MP.char '}'
      function tname_space = MP.label "function-def" $ do
        typename <- tname_space
        argname <-
          MP.try textName <|>
          MP.try symbolicName <|>
          ("" <$ MP.char '_') -- discard arg
        MP.space1
        selfalias <-
          MP.try ("self" <$ MP.string "=>") <|>
          (MP.char '[' *> (MP.try textName <|> symbolicName) <* MP.string "]=>")
        MP.space1
        body <- parseExpression seasoning
        return $ join $ (<$>) liftTypeDefAttached $ TypeDefAttached <$> sequenceA [ClosureTypeDef typename selfalias argname <$> body] <*> pure [ClosureTypeHash (typename, 0)] <*> pure typename
      constructed = getCompose $ Closure <$> Compose (pure <$> textName) <*> Compose objectBody
      initialTypeDef = do
        objbod <- objectBody
        MP.string "::"
        typename <- function (MP.try (textName <* MP.space1) <|> (MP.sourcePosPretty <$> MP.getSourcePos <* MP.space1))
        return $ Closure <$> typename <*> objbod
      lambda = do
        internalname <- function (MP.sourcePosPretty <$> MP.getSourcePos)
        return $ Closure <$> internalname <*> pure []
      jsobj = (<$>) (Closure "Object") <$> objectBody

-- TODO: ...Till refector target.
parseExpression :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String (m Expression)
parseExpression seasoning =
  MP.try codef <|>
  operatorfold
    where
      this = parseExpression seasoning -- expression parser recursively calls itself
      codef = MP.label "${...}" $ do
        MP.string "${" <* MP.space
        defs <- (<$>) sequenceA $ MP.many $ MP.try $ MP.label "declaration" $ getCompose $ (,)
          <$> Compose ((<$>) pure $ MP.try textName <|> symbolicName)
          <*> Compose (MP.space1 *> MP.char '=' *> MP.space1 *> this <* MP.space <* MP.char ';' <* MP.space)
        MP.char '}'
        val <- MP.space *> this
        return $ CoDef <$> defs <*> val
      value =
        MP.try ((<$>) Lit <$> salt seasoning) <|>
        MP.try ((<$>) Prim <$> parsePrimitive seasoning)
      regularFunc =
        MP.try ((<$>) pure $ Name <$> textName) <|>
        MP.try (MP.char '(' *> (<$>) pure (Name <$> symbolicName) <* MP.char ')') <|>
        MP.char '(' *> MP.space *> this <* MP.space <* MP.char ')'
      propdrill = do
        obj <- regularFunc
        props <- pure <$> MP.many (MP.try $ MP.space *> MP.char '.' *> MP.space *> textName)
        return $ foldl GetProp <$> obj <*> props
      inlineFunc = MP.label "operator" $
        MP.try ((<$>) pure $ Name <$> symbolicName) <|>
        MP.try (MP.char '`' *> (<$>) pure (Name <$> textName) <* MP.char '`') <|>
        MP.string "`(" *> MP.space *> this <* MP.space <* MP.string ")`"
      component = MP.label "value" $
        MP.try (sugar seasoning this) <|>
        MP.try (parseClosure seasoning) <|>
        MP.try value <|>
        propdrill
      currychain = do
        first <- component
        rest <- sequenceA <$> MP.many (MP.try $ MP.space1 *> component)
        return $ foldl Apply <$> first <*> rest
      operatorfold = do
        initialchain <- currychain
        opfolds <- sequenceA <$> MP.many (MP.try $ getCompose $ (\operator rhs lhs -> inline operator lhs rhs) <$> Compose (MP.space1 *> inlineFunc <* MP.space1) <*> Compose currychain)
        return $ foldl (\lhs op_rhs -> op_rhs lhs) <$> initialchain <*> opfolds

