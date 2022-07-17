module KitsuComponents (
  reflect
) where

import KitsuByteCode
import KitsuPreludeConnection (inline)
import KitsuSyntaxBundling (SyntaxBundle(..), KitParseMonad(..), SyntaxReflection(..))

import Data.Hashable ( Hashable(hash, hashWithSalt) )

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Functor.Compose (Compose (Compose), getCompose)

-- === reflect ===

reflect :: (Ord e, KitParseMonad m) => SyntaxBundle m e -> SyntaxReflection m e
reflect s = res
  where
    res = SyntaxReflection {
      rTextName = textName s,
      rSymbolName = symbolName s,
      rExpression = parseExpression s,
      rArrowFunc = parseArrowFunc s,
      rStatic = statics s res
    }

-- === mirror ===

textName :: (Ord e, KitParseMonad m) => SyntaxBundle m e -> MP.Parsec e String String
textName s = MP.label "name" $ (*>) (MP.notFollowedBy $ keywords s) $ (:) <$> startChar <*> MP.many restChar
    where
      startChar = MP.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" -- underscore is reserved for internal use
      restChar = MP.oneOf "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789'"

symbolName :: (Ord e, KitParseMonad m) => SyntaxBundle m e -> MP.Parsec e String String
symbolName s = MP.label "symbol" $
  MP.notFollowedBy (MP.string "=>") *>
  MP.notFollowedBy (MP.string "::") *>
  MP.notFollowedBy (keywords s) *>
  MP.some (MP.oneOf "$%^&*-+=<>[]|?!:/~")

parseArrowFunc :: (Ord e, KitParseMonad m) => SyntaxBundle m e -> (Expression -> Expression) -> MP.Parsec e String () -> MP.Parsec e String (String -> m a -> m a)
parseArrowFunc s f stop = MP.label "... => ..." $ do
  argname <-
    MP.try (textName s) <|>
    MP.try (symbolName s)
  MP.space1 <* MP.string "=>" <* MP.space1
  expression <- (<$>) f <$> parseExpression s stop
  let compress = (\mf ma -> join $ mf <*> pure ma)
  return $ \tname -> compress $ (\expression' -> defineType $ ClosureTypeDef tname (hashWithSalt (hash argname) expression') (Just (argname, expression'))) <$> expression

parseObjectBody :: (Ord e, KitParseMonad m) => SyntaxBundle m e -> MP.Parsec e String (m [(String, Expression)])
parseObjectBody s = MP.try (pure [] <$ MP.char '{' <* MP.space <* MP.char '}') <|> ((\(wkvs, wkv) -> do
    kvs <- sequenceA wkvs
    kv <- wkv
    return $ kvs ++ [kv]
  ) <$> (MP.char '{' *> MP.space *> MP.manyTill_ (keyvalue (MP.try $ MP.space *> MP.char ',' *> MP.space)) (MP.try $ keyvalue $ MP.try $ MP.space <* MP.char '}')))
    where
      keyvalue stop' = MP.label "key-value-pair" $ getCompose $ (,)
        <$> Compose ((<$>) pure $ textName s <* MP.space)
        <*> Compose (MP.char ':' *> MP.space *> parseExpression s stop')

parseCapture :: (Ord e, KitParseMonad m) => SyntaxBundle m e -> MP.Parsec e String ([(String, Expression)], Expression -> Expression)
parseCapture s = do
  MP.char '['
  elems <- (\(xs, x) -> xs ++ [x]) <$> MP.manyTill_ (MP.space *> (MP.try (textName s) <|> symbolName s) <* MP.space <* MP.char ',') (MP.try $ MP.space *> (MP.try (textName s) <|> symbolName s) <* MP.space <* MP.char ']')
  let indexes = (\x -> "x" ++ show x) <$> [0..(length elems - 1)]
  let body = [(i, Name cap) | (i, cap) <- zip indexes elems]
  let exprf = CoDef [(Just cap, Apply (GetProp i) (Name "self")) | (cap, i) <- zip elems indexes]
  return (body, exprf)

parseClosure :: (Ord e, KitParseMonad m) => SyntaxBundle m e -> MP.Parsec e String () -> MP.Parsec e String (m Expression)
parseClosure s stop = MP.label "closure" $
  MP.try (constructed <* stop) <|>
  MP.try (initialTypeDef stop) <|>
  MP.try (lambda stop) <|>
  MP.try (caplambda stop) <|>
  jsobj stop
    where
      constructed = getCompose $ Closure <$> Compose (pure <$> textName s) <*> Compose (parseObjectBody s)
      initialTypeDef stop' = do
        objbod <- parseObjectBody s
        MP.string "::"
        typename <-
          MP.try (textName s <* MP.space1) <|>
          (MP.sourcePosPretty <$> MP.getSourcePos <* MP.space1)
        func <- parseArrowFunc s id stop'
        return $ func typename $ Closure typename <$> objbod
      lambda stop' = do
        internalname <- MP.sourcePosPretty <$> MP.getSourcePos
        func <- parseArrowFunc s id stop'
        return $ func internalname $ pure $ Closure internalname []
      caplambda stop' = do
        (objbod, exprf) <- parseCapture s
        typename <- MP.string "::" *> (MP.sourcePosPretty <$> MP.getSourcePos) <* MP.space1
        func <- parseArrowFunc s exprf stop'
        return $ func typename $ pure (Closure typename objbod)
      jsobj stop' = ((<$>) (Closure "Object") <$> parseObjectBody s) <* stop'

parseExpression :: (Ord e, KitParseMonad m) => SyntaxBundle m e -> MP.Parsec e String () -> MP.Parsec e String (m Expression)
parseExpression s stop =
  MP.try (codef stop) <|>
  operatorfold stop
    where
      this = parseExpression s -- expression parser recursively calls itself
      codef stop' = MP.label "${...}" $ do
        MP.string "${" <* MP.space
        defs <- (<$>) sequenceA $ MP.manyTill (getCompose $ (,)
          <$> Compose (MP.try ((<$>) pure $ MP.optional $ (MP.try (textName s) <|> symbolName s) <* MP.space1 <* MP.char '=' <* MP.space1))
          <*> Compose (this (MP.try $ MP.space <* MP.char ';') <* MP.space)
          ) (MP.char '}')
        val <- MP.space *> this stop'
        return $ CoDef <$> defs <*> val
      regularFunc =
        MP.try ((<$>) pure $ MP.char '?' *> (HasProp <$> textName s)) <|>
        MP.try ((<$>) pure $ MP.char '.' *> (GetProp <$> textName s)) <|>
        MP.try ((<$>) pure $ Name <$> textName s) <|>
        MP.try (MP.char '(' *> (<$>) pure (Name <$> symbolName s) <* MP.char ')') <|>
        MP.char '(' *> MP.space *> this (MP.try $ MP.space <* MP.char ')')
      propdrill stop' = do
        obj <- regularFunc
        props <- pure <$> MP.manyTill (MP.char '.' *> textName s) stop'
        return $ foldl (\obj' prop -> Apply (GetProp prop) obj') <$> obj <*> props
      inlineFunc = MP.label "operator" $
        MP.try ((<$>) pure $ Name <$> symbolName s) <|>
        MP.try (MP.char '`' *> (<$>) pure (Name <$> textName s) <* MP.char '`') <|>
        MP.string "`(" *> MP.space *> this (MP.try $ MP.space <* MP.string ")`")
      component stop' =
        MP.try (MP.char '$' *> MP.space1 *> this stop') <|>
        MP.try (extensions s (reflect s) stop') <|>
        MP.try (codef stop') <|>
        MP.try (parseClosure s stop') <|>
        propdrill stop'
      currychain stop' = do
        terms <- (\(es, e) -> (\as a -> as ++ [a]) <$> sequenceA es <*> e) <$> MP.manyTill_ (component $ MP.try MP.space1) (MP.try $ component stop')
        return $ foldl1 Apply <$> terms
      operatorfold stop' = do
        opfold <- (\(ees, e) -> (sequenceA ((\(x, y) -> (,) <$> x <*> y) <$> ees), e)) <$> MP.manyTill_ ((,)
          <$> currychain (MP.try $ MP.lookAhead $ MP.space1 <* (MP.try (() <$ MP.char '`') <|> (() <$ symbolName s)))
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

