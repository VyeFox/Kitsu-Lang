module KitsuSyntaxBundles (
  escapedChar,
  baseExpressions,
  stringLiteral,
  tupleLiteral,
  valueDefinition,
  typeDefinition
) where

import KitsuByteCode
import KitsuSyntaxBundling (SyntaxBundle(..), KitParseMonad(..), SyntaxReflection(..))
import KitsuRuntimeConnection (emptyTuple)

import Data.Hashable ( Hashable(hash, hashWithSalt) )

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Ratio ( (%) )

parseIntegral :: (Num a, Ord e) => MP.Parsec e String a
parseIntegral = foldl (\s dig -> s*10 + dig) 0 <$> MP.some digit

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

parseLiteral :: (Ord e, Monad m) => MP.Parsec e String (m Literal)
parseLiteral = MP.label "literal" $ (<$>) pure $
  MP.try parseBool <|>
  MP.try parseByte <|>
  MP.try parseNat <|>
  MP.try parseRat <|>
  MP.try parseInt <|>
  MP.try parseChar <|>
  parseErr
    where
      parseBool = (KitBool True <$ MP.string "true") <|> (KitBool False <$ MP.string "false")
      parseNat = KitNat <$> parseIntegral
      parseInt = KitInt <$> (sign <*> parseIntegral)
      parseRat = KitRat <$> (sign <*> ((%) <$> parseIntegral <*> (MP.space *> MP.char '%' *> MP.space *> parseIntegral)))
      parseByte = KitByte <$> ((\x' x -> 16*x' + x) <$> (MP.string "0x" *> hex) <*> hex)
      parseChar = KitChar <$> (MP.char '\'' *> escapedChar <* MP.char '\'')
      parseErr = KitErr <$> (MP.sourcePosPretty <$> MP.getSourcePos) <* MP.string "err"

baseExpressions :: (Ord e, KitParseMonad m) => SyntaxBundle m e
baseExpressions = SyntaxBundle {
    keywords = (<$) () $
      MP.try (MP.string "async") <|>
      MP.try (MP.string "lazy") <|>
      MP.try (MP.string "atomic") <|>
      MP.try (MP.string "true") <|>
      MP.try (MP.string "false") <|>
      MP.string "err",
    extensions = \r stop ->
      MP.try ((<$>) (Prim . KitAsync) <$> (MP.string "async" *> MP.space1 *> rExpression r stop)) <|>
      MP.try ((<$>) (Prim . KitLazy) <$> (MP.string "lazy" *> MP.space1 *> rExpression r stop)) <|>
      MP.try ((<$>) (Prim . KitAtomic) <$> (MP.string "atomic" *> MP.space1 *> rExpression r stop)) <|>
      ((<$>) Lit <$> parseLiteral <* stop),
    statics = const MP.empty
  }

stringLiteral :: (Ord e, KitParseMonad m) => SyntaxBundle m e
stringLiteral = SyntaxBundle {
    keywords = MP.empty,
    extensions = \r stop -> MP.label "string" $ (<$>) pure $ do
      MP.char '\"'
      chars <- MP.manyTill (Lit . KitChar <$> escapedChar) (MP.char '\"')
      stop
      return $ foldl Apply emptyTuple chars,
    statics = const MP.empty
  }

tupleLiteral :: (Ord e, KitParseMonad m) => SyntaxBundle m e
tupleLiteral = SyntaxBundle {
    keywords = MP.empty,
    extensions = \r stop ->
      MP.try ((<$>) pure $ emptyTuple <$ MP.char '(' <* MP.space <* MP.char ')' <* stop) <|>
      MP.try ((<$>) (Apply emptyTuple) <$> (MP.char '(' *> MP.space *> rExpression r (MP.try $ MP.space <* MP.char ',') <* MP.space <* MP.char ')' <* stop)) <|>
      (do
        MP.char '('
        terms <- (\(es, e) -> (\as a -> as ++ [a]) <$> sequenceA es <*> e) <$> MP.someTill_ (MP.space *> rExpression r (MP.try $ MP.space <* MP.char ',' <* MP.space)) (MP.try $ rExpression r (MP.try $ MP.space <* MP.char ')'))
        stop
        return $ foldl Apply emptyTuple <$> terms
      ),
    statics = const MP.empty
  }

valueDefinition :: (Ord e, KitParseMonad m) => SyntaxBundle m e
valueDefinition = SyntaxBundle {
    keywords = MP.empty,
    extensions = const $ const MP.empty,
    statics = \r -> MP.label "value-definition" $ do
      name <- MP.try (rTextName r) <|> rSymbolName r
      MP.space1 <* MP.char '=' <* MP.space1
      val <- rExpression r (MP.try $ MP.space <* MP.char ';')
      let compress = (\mf ma -> join $ mf <*> pure ma)
      return $ compress ((\val' -> defineVal (Just name, val')) <$> val) $ pure ()
  }

typeDefinition :: (Ord e, KitParseMonad m) => SyntaxBundle m e
typeDefinition = SyntaxBundle {
    keywords = MP.empty,
    extensions = const $ const MP.empty,
    statics = \r -> MP.label "type-definition" $ do
      MP.string "::"
      typename <- rTextName r
      MP.space1
      fbody <- rArrowFunc r id (MP.try $ MP.space <* MP.char ';')
      return $ fbody typename $ pure ()
  }

defaultBundle :: (Ord e, KitParseMonad m) => SyntaxBundle m e
defaultBundle = baseExpressions
  <> valueDefinition
  <> typeDefinition

        