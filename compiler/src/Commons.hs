{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase #-}
module Commons ((<:>), selectUntil, selectUntil1, stringLiteral) where

import qualified Text.Megaparsec as MP
import Text.Megaparsec ((<?>))
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Void ( Void )


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
            MP.noneOf "\\\""


(<:>) :: (MP.Stream a, MP.Stream b, MP.VisualStream b, MP.TraversableStream b) => MP.Parsec String a b -> MP.Parsec String b c -> MP.Parsec String a c
(<:>) p1 p2 = p1 >>= (\case
    Right x -> pure x
    Left err -> MP.customFailure (MP.errorBundlePretty err)) . MP.parse p2 ""


