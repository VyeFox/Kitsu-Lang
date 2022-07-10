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

parsePrimitive :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String () -> MP.Parsec e String (m Primitive)
parsePrimitive seasoning stop = MP.label "primitive" $
  MP.try ((<$>) KitAsync <$> (MP.string "async" *> MP.space1 *> parseExpression seasoning stop)) <|>
  (<$>) KitAtomic <$> (MP.string "atomic" *> MP.space1 *> parseExpression seasoning stop)

parseClosure :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String () -> MP.Parsec e String (m Expression)
parseClosure seasoning stop = MP.label "closure" $
  MP.try (constructed <* stop) <|>
  MP.try initialTypeDef <|>
  MP.try lambda <|>
  (jsobj <* stop)
    where
      keyvalue stop' = MP.label "key-value-pair" $ getCompose $ (,)
        <$> Compose ((<$>) pure $ textName <* MP.space)
        <*> Compose (MP.char ':' *> MP.space *> parseExpression seasoning stop')
      objectBody = MP.try (pure [] <$ MP.char '{' <* MP.space <* MP.char '}') <|> ((\(wkvs, wkv) -> do
          kvs <- sequenceA wkvs
          kv <- wkv
          return $ kvs ++ [kv]
        ) <$> (MP.char '{' *> MP.space *> MP.manyTill_ (keyvalue (MP.space *> MP.char ',' *> MP.space)) (keyvalue $ MP.space <* MP.char '}')))
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
        funcBody <- parseExpression seasoning stop
        return $ join $ (<$>) liftTypeDefAttached $ TypeDefAttached <$> sequenceA [ClosureTypeDef typename selfalias argname <$> funcBody] <*> pure [ClosureTypeHash (typename, 0)] <*> pure typename
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
parseExpression :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String () -> MP.Parsec e String (m Expression)
parseExpression seasoning stop =
  MP.try (codef stop) <|>
  operatorfold stop
    where
      this = parseExpression seasoning -- expression parser recursively calls itself
      codef stop' = MP.label "${...}" $ do
        MP.string "${" <* MP.space
        defs <- (<$>) sequenceA $ MP.manyTill (getCompose $ (,)
          <$> Compose ((<$>) pure $ MP.try textName <|> symbolicName)
          <*> Compose (MP.space1 *> MP.char '=' *> MP.space1 *> this (MP.space <* MP.char ';') <* MP.space)
          ) (MP.char '}')
        val <- MP.space *> this stop'
        return $ CoDef <$> defs <*> val
      value stop' =
        MP.try ((<$>) Lit <$> salt seasoning <* stop') <|>
        MP.try ((<$>) Prim <$> parsePrimitive seasoning stop')
      regularFunc =
        MP.try ((<$>) pure $ Name <$> textName) <|>
        MP.try (MP.char '(' *> (<$>) pure (Name <$> symbolicName) <* MP.char ')') <|>
        MP.char '(' *> MP.space *> this (MP.space <* MP.char ')')
      propdrill stop' = do
        obj <- regularFunc
        props <- pure <$> MP.manyTill (MP.space *> MP.char '.' *> MP.space *> textName) stop'
        return $ foldl GetProp <$> obj <*> props
      inlineFunc = MP.label "operator" $
        MP.try ((<$>) pure $ Name <$> symbolicName) <|>
        MP.try (MP.char '`' *> (<$>) pure (Name <$> textName) <* MP.char '`') <|>
        MP.string "`(" *> MP.space *> this (MP.space <* MP.string ")`")
      component stop' = MP.label "value" $
        MP.try (sugar seasoning this stop') <|>
        MP.try (parseClosure seasoning stop') <|>
        MP.try (value stop') <|>
        propdrill stop'
      currychain stop' = do
        terms <- (\(es, e) -> (\as a -> as ++ [a]) <$> sequenceA es <*> e) <$> MP.manyTill_ (component MP.space1) (component stop')
        let first = head <$> terms
        let rest = tail <$> terms
        return $ foldl Apply <$> first <*> rest
      operatorfold stop' = do
        opfold <- (\(ees, e) -> (sequenceA ((\(x, y) -> (,) <$> x <*> y) <$> ees), e)) <$> MP.manyTill_ ((,)
          <$> currychain (MP.lookAhead $ MP.space1 <* (MP.try (() <$ MP.char '`') <|> (() <$ symbolicName)))
          <*> (MP.space1 *> inlineFunc <* MP.space1)) (currychain stop')
        --  * TL;DR: by folding a function the natural order of the fold can be reversed
        {-
          The composition of opfold into a single expression can be thought of as an action upon the last element (call it `x`);
          in the case where there are no operators, this action on `x` is `id`.
          in the case where there is an operator `(y, op)` this action is `(id y) 'op' x`
          the already existing action is applied to `y` to ensure the fold direction is: `(x <> y) <> z`
          the resulting action of folding this composition through opfold is precisely the action needed on x
          ...to form the expression with the correct fold order.
        -}
        let (yops, x) = opfold
        return $ (foldl (\format (y, op) x' -> inline op y x') id <$> yops) <*> x

