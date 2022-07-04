{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances  #-}
module Commons ((<:>), selectUntil, selectUntil1, stringLiteral) where

import qualified Text.Megaparsec as MP
import Text.Megaparsec ((<?>))
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Void ( Void )
import Data.Bits (Bits(xor))


instance MP.ShowErrorComponent [Char] where
  showErrorComponent str = str


selectUntil :: (MP.Stream a) => MP.Parsec String a b -> MP.Parsec String a [MP.Token a]
selectUntil p = MP.many $ MP.try $ MP.notFollowedBy p *> MP.anySingle


selectUntil1 :: (MP.Stream a) => MP.Parsec String a b -> MP.Parsec String a [MP.Token a]
selectUntil1 p = MP.some $ MP.try $ MP.notFollowedBy p *> MP.anySingle


stringLiteral :: MP.Parsec String String String
stringLiteral = (MP.char '"' *> MP.many term <* MP.char '"') <?> "string literal"
    where
        term =
            MP.try ('\"' <$ MP.string "\\\"") <|>
            MP.try ('\t' <$ MP.string "\\t") <|>
            MP.try ('\n' <$ MP.string "\\n") <|>
            MP.try ('\r' <$ MP.string "\\r") <|>
            MP.try ('\NUL' <$ MP.string "\\0") <|>
            MP.try ('\\' <$ MP.string "\\\\") <|>
            MP.noneOf ['\\', '\"']

-- alternates between String and `a` 
formatStringLiteral :: MP.Parsec String String a -> MP.Parsec String String ([(String, a)], String)
formatStringLiteral p = MP.char '"' *> ((,) <$> MP.many (MP.try segment) <*> MP.many term) <* MP.char '"'
  where
    term =
      MP.try ('\"' <$ MP.string "\\\"") <|>
      MP.try ('\t' <$ MP.string "\\t") <|>
      MP.try ('\n' <$ MP.string "\\n") <|>
      MP.try ('\r' <$ MP.string "\\r") <|>
      MP.try ('\NUL' <$ MP.string "\\0") <|>
      MP.try ('\\' <$ MP.string "\\\\") <|>
      MP.try ('{' <$ MP.string "\\{") <|> -- '{' denotes start of format block
      MP.try ('}' <$ MP.string "\\}") <|> -- '}' denotes end of format block
      MP.noneOf ['\\', '\"', '{', '}']
    segment = (,) <$> MP.many term <*> (MP.char '{' *> p <* MP.char '}')


(<:>) :: (MP.Stream a, MP.Stream b, Show b, MP.VisualStream b, MP.TraversableStream b) => MP.Parsec String a b -> MP.Parsec String b c -> MP.Parsec String a c
(<:>) p1 p2 = do
  res <- p1
  case MP.parse p2 "" res of
    Right x -> pure x
    Left err -> MP.customFailure (show res <> " <:> ...\n" <> indent (MP.errorBundlePretty err))
      where
        indent = unlines . map ('\t' :) . lines


