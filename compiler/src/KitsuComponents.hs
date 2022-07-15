module KitsuComponents (
  textName,
  symbolicName,
  KitParseMonad (..),
  parsePrimitive,
  parseClosure,
  parseArrowFunc,
  parseExpression,
  applySugars,
  applyHerbs,
  variableName
) where

import KitsuByteCode
import KitsuPreludeConnection (inline)
import KitsuSeasoning (Seasoning(..), KitParseMonad(..))

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Functor.Compose (Compose (Compose), getCompose)

-- === HELPERS FOR COOKS ===

applySugars :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String () -> MP.Parsec e String (m Expression)
applySugars s = sugar s (reservations s) (parseExpression s)

applyHerbs :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String (m ())
applyHerbs s = herbs s (reservations s) (parseExpression s)

variableName :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String String
variableName s = MP.notFollowedBy (reservations s) *> (MP.try textName <|> symbolicName)

-- === SIMPLE PARSERS ===

textName :: (Ord e) => MP.Parsec e String String
textName = MP.label "name" $
  MP.notFollowedBy (MP.string "async") *>
  MP.notFollowedBy (MP.string "atomic") *>
  ((:) <$> startChar <*> MP.many restChar)
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

parseArrowFunc :: (Ord e, KitParseMonad m) => (Expression -> Expression) -> Seasoning m e -> MP.Parsec e String () -> MP.Parsec e String (String -> m a -> m a)
parseArrowFunc f seasoning stop = MP.label "... => ..." $ do
  argname <-
    MP.notFollowedBy (reservations seasoning) *>
    (MP.try textName <|>
    MP.try symbolicName)
  MP.space1 <* MP.string "=>" <* MP.space1
  expression <- parseExpression seasoning stop
  let compress = (\mf ma -> join $ mf <*> pure ma)
  return $ \tname -> compress $ (\expression' -> defineType $ ClosureTypeDef tname 0 (Just (argname, expression'))) <$> (f <$> expression)

parseObjectBody :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String (m [(String, Expression)])
parseObjectBody seasoning = MP.try (pure [] <$ MP.char '{' <* MP.space <* MP.char '}') <|> ((\(wkvs, wkv) -> do
    kvs <- sequenceA wkvs
    kv <- wkv
    return $ kvs ++ [kv]
  ) <$> (MP.char '{' *> MP.space *> MP.manyTill_ (keyvalue (MP.try $ MP.space *> MP.char ',' *> MP.space)) (MP.try $ keyvalue $ MP.try $ MP.space <* MP.char '}')))
    where
      keyvalue stop' = MP.label "key-value-pair" $ getCompose $ (,)
        <$> Compose ((<$>) pure $ MP.notFollowedBy (reservations seasoning) *> textName <* MP.space)
        <*> Compose (MP.char ':' *> MP.space *> parseExpression seasoning stop')

parseCapture :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String ([(String, Expression)], Expression -> Expression)
parseCapture seasoning = do
  MP.char '['
  elems <- (\(xs, x) -> xs ++ [x]) <$> MP.manyTill_ (MP.space *> variableName seasoning <* MP.space <* MP.char ',') (MP.try $ MP.space *> variableName seasoning <* MP.space <* MP.char ']')
  let indexes = (\x -> "x" ++ show x) <$> [0..(length elems - 1)]
  let body = [(i, Name cap) | (i, cap) <- zip indexes elems]
  let exprf = CoDef [(Just cap, GetProp (Name "self") i) | (cap, i) <- zip elems indexes]
  return (body, exprf)

parseClosure :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String () -> MP.Parsec e String (m Expression)
parseClosure seasoning stop = MP.label "closure" $
  MP.try (constructed <* stop) <|>
  MP.try (initialTypeDef stop) <|>
  MP.try (lambda stop) <|>
  MP.try (caplambda stop) <|>
  jsobj stop
    where
      constructed = getCompose $ Closure <$> Compose (pure <$> (MP.notFollowedBy (reservations seasoning) *> textName)) <*> Compose (parseObjectBody seasoning)
      initialTypeDef stop' = do
        objbod <- parseObjectBody seasoning
        MP.string "::"
        typename <-
          MP.try (MP.notFollowedBy (reservations seasoning) *> textName <* MP.space1) <|>
          (MP.sourcePosPretty <$> MP.getSourcePos <* MP.space1)
        func <- parseArrowFunc id seasoning stop'
        return $ func typename $ Closure typename <$> objbod
      lambda stop' = do
        internalname <- MP.sourcePosPretty <$> MP.getSourcePos
        func <- parseArrowFunc id seasoning stop'
        return $ func internalname $ pure $ Closure internalname []
      caplambda stop' = do
        (objbod, exprf) <- parseCapture seasoning
        typename <- MP.string "::" *> (MP.sourcePosPretty <$> MP.getSourcePos) <* MP.space1
        func <- parseArrowFunc exprf seasoning stop'
        return $ func typename $ pure (Closure typename objbod)
      jsobj stop' = ((<$>) (Closure "Object") <$> parseObjectBody seasoning) <* stop'

parseExpression :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String () -> MP.Parsec e String (m Expression)
parseExpression seasoning stop =
  MP.try (codef stop) <|>
  operatorfold stop
    where
      this = parseExpression seasoning -- expression parser recursively calls itself
      codef stop' = MP.label "${...}" $ do
        MP.string "${" <* MP.space
        defs <- (<$>) sequenceA $ MP.manyTill (getCompose $ (,)
          <$> Compose (MP.try ((<$>) pure $ MP.optional $ variableName seasoning <* MP.space1 <* MP.char '=' <* MP.space1))
          <*> Compose (this (MP.try $ MP.space <* MP.char ';') <* MP.space)
          ) (MP.char '}')
        val <- MP.space *> this stop'
        return $ CoDef <$> defs <*> val
      value stop' =
        MP.try ((<$>) Lit <$> salt seasoning <* stop') <|>
        MP.try ((<$>) Prim <$> parsePrimitive seasoning stop')
      regularFunc =
        MP.try ((<$>) pure $ Name <$> (MP.notFollowedBy (reservations seasoning) *> textName)) <|>
        MP.try (MP.char '(' *> (<$>) pure (Name <$> (MP.notFollowedBy (reservations seasoning) *> symbolicName)) <* MP.char ')') <|>
        MP.char '(' *> MP.space *> this (MP.try $ MP.space <* MP.char ')')
      propdrill stop' = do
        obj <- regularFunc
        props <- pure <$> MP.manyTill (MP.space *> MP.char '.' *> MP.space *> MP.notFollowedBy (reservations seasoning) *> textName) stop'
        return $ foldl GetProp <$> obj <*> props
      inlineFunc = MP.label "operator" $
        MP.try ((<$>) pure $ Name <$> (MP.notFollowedBy (reservations seasoning) *> symbolicName)) <|>
        MP.try (MP.char '`' *> (<$>) pure (Name <$> (MP.notFollowedBy (reservations seasoning) *> textName)) <* MP.char '`') <|>
        MP.string "`(" *> MP.space *> this (MP.try $ MP.space <* MP.string ")`")
      component stop' =
        MP.try (applySugars seasoning stop') <|>
        MP.try (codef stop') <|>
        MP.try (parseClosure seasoning stop') <|>
        MP.try (value stop') <|>
        propdrill stop'
      currychain stop' = do
        terms <- (\(es, e) -> (\as a -> as ++ [a]) <$> sequenceA es <*> e) <$> MP.manyTill_ (component $ MP.try MP.space1) (MP.try $ component stop')
        return $ foldl1 Apply <$> terms
      operatorfold stop' = do
        opfold <- (\(ees, e) -> (sequenceA ((\(x, y) -> (,) <$> x <*> y) <$> ees), e)) <$> MP.manyTill_ ((,)
          <$> currychain (MP.try $ MP.lookAhead $ MP.space1 <* (MP.try (() <$ MP.char '`') <|> (() <$ (MP.notFollowedBy (reservations seasoning) *> symbolicName))))
          <*> (MP.space1 *> inlineFunc <* MP.space1)) (MP.try $ currychain stop')
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
        return $ (foldl (\format (y, op) x' -> inline op (format y) x') id <$> yops) <*> x

