module KitsuSpiceRack (
  escapedChar,
  simpleLiterals,
  stringLiteral,
  tupleLiteral,
  valueDefinition
) where

import KitsuByteCode
import KitsuSeasoning (Seasoning(..), KitParseMonad(..))
import KitsuPreludeConnection (emptyTuple)
import KitsuComponents (textName, symbolicName, parseExpression)

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Ratio ( (%) )

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

parseLiteral :: (Ord e, Monad m) => MP.Parsec e String (m Literal)
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

simpleLiterals :: (Ord e, KitParseMonad m) => Seasoning m e
simpleLiterals = Seasoning {
    reservations = (<$) () $ MP.try (MP.string "true") <|> MP.string "false",
    salt = parseLiteral,
    sugar = const $ const $ const MP.empty,
    herbs = const $ const MP.empty
  }

stringLiteral :: (Ord e, KitParseMonad m) => Seasoning m e
stringLiteral = Seasoning {
    reservations = MP.empty,
    salt = MP.empty,
    sugar = const $ const $ \stop -> (<$>) pure $ do
      MP.char '\"'
      chars <- MP.manyTill (Lit . KitChar <$> escapedChar) (MP.char '\"')
      stop
      return $ foldl Apply emptyTuple chars,
    herbs = const $ const MP.empty
  }

tupleLiteral :: (Ord e, KitParseMonad m) => Seasoning m e
tupleLiteral = Seasoning {
    reservations = MP.empty,
    salt = MP.empty,
    sugar = \res exp stop ->
      MP.try ((<$>) pure $ emptyTuple <$ MP.char '(' <* MP.space <* MP.char ')' <* stop) <|>
      MP.try ((<$>) (Apply emptyTuple) <$> (MP.char '(' *> MP.space *> exp (MP.try $ MP.space <* MP.char ',') <* MP.space <* MP.char ')' <* stop)) <|>
      (do
        MP.char '('
        terms <- (\(es, e) -> (\as a -> as ++ [a]) <$> sequenceA es <*> e) <$> MP.someTill_ (MP.space *> exp (MP.try $ MP.space <* MP.char ',' <* MP.space)) (MP.try $ exp (MP.try $ MP.space <* MP.char ')'))
        stop
        return $ foldl Apply emptyTuple <$> terms
      ),
    herbs = const $ const MP.empty
  }

valueDefinition :: (Ord e, KitParseMonad m) => Seasoning m e
valueDefinition = Seasoning {
    reservations = MP.empty,
    salt = MP.empty,
    sugar = const $ const $ const MP.empty,
    herbs = \res exp -> do
      name <- MP.notFollowedBy res *> (MP.try textName <|> symbolicName)
      MP.space1 <* MP.char '=' <* MP.space1
      val <- exp (MP.try $ MP.space <* MP.char ';')
      let compress = (\mf ma -> join $ mf <*> pure ma)
      return $ compress ((\val' -> defineVal (Just name, val')) <$> val) $ pure ()
  }

        