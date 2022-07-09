module KitsuSpiceRack (simpleLiterals, stringLiteral, tupleLiteral) where

import KitsuByteCode
import KitsuSeasoning (Seasoning(..), KitParseMonad(..), TypeDefAttached(..), AfterImportAction(..))
import KitsuPreludeConnection (emptyTuple)
import KitsuComponents (textName, symbolicName)

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

simpleLiterals :: (Ord e, Monad m) => Seasoning m e
simpleLiterals = Seasoning {
        salt = parseLiteral,
        sugar = const MP.empty,
        herbs = const MP.empty
    }

stringLiteral :: (Ord e, Monad m) => Seasoning m e
stringLiteral = Seasoning {
        salt = MP.empty,
        sugar = const $ (<$>) pure $ do
            MP.char '\"'
            chars <- MP.many (MP.try $ Lit . KitChar <$> escapedChar)
            MP.char '\"'
            return $ foldl Apply emptyTuple chars,
        herbs = const MP.empty
    }

tupleLiteral :: (Ord e, Monad m) => Seasoning m e
tupleLiteral = Seasoning {
        salt = MP.empty,
        sugar = \exp ->
            MP.try ((<$>) pure $ emptyTuple <$ MP.char '(' <* MP.space <* MP.char ')') <|>
            MP.try ((<$>) (Apply emptyTuple) <$> (MP.char '(' *> MP.space *> exp <* MP.space <* MP.char ',' <* MP.space <* MP.char ')')) <|>
            (do
                MP.char '('
                first <- MP.space *> exp <* MP.space
                rest <- (<$>) sequenceA $ MP.many $ MP.try $ MP.char ',' *> MP.space *> exp <* MP.space
                MP.char ')'
                return $ foldl Apply emptyTuple <$> ((:) <$> first <*> rest)
            ),
        herbs = const MP.empty
    }

typeDefinition :: (Ord e, KitParseMonad m) => Seasoning m e
typeDefinition = Seasoning {
        salt = MP.empty,
        sugar = const MP.empty,
        herbs = \exp -> do
            MP.string "::"
            typename <- textName
            MP.space1
            argname <-
                MP.try textName <|>
                MP.try symbolicName <|>
                ("" <$ MP.char '_') -- discard arg
            MP.space1
            selfalias <-
                MP.try ("self" <$ MP.string "=>") <|>
                MP.try ("" <$ MP.string "[]=>") <|> -- un-usable name, syntax for pure functions
                (MP.char '[' *> (MP.try textName <|> symbolicName) <* MP.string "]=>")
            MP.space1
            body <- exp
            MP.space1
            MP.char ';'
            return $ join $ (<$>) liftTypeDefAttached $ TypeDefAttached <$> sequenceA [ClosureTypeDef typename selfalias argname <$> body] <*> pure [ClosureTypeHash (typename, 0)] <*> pure ()
    }

        