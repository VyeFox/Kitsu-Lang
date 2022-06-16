{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Commons ((<:>), selectUntil, stringLiteral) where

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Void ( Void )
import Text.Megaparsec (pos1)


instance MP.ShowErrorComponent () where
  showErrorComponent _ = "()"


selectUntil :: (MP.Stream a) => MP.Parsec () a b -> MP.Parsec () a [MP.Token a]
selectUntil p = MP.many $ MP.try $ MP.notFollowedBy p *> MP.anySingle


stringLiteral :: MP.Parsec () String String
stringLiteral = MP.char '"' *> MP.many term <* MP.char '"'
    where
        term =
            MP.try ('\"' <$ MP.string "\\\"") <|>
            MP.try ('\t' <$ MP.string "\\t") <|>
            MP.try ('\n' <$ MP.string "\\n") <|>
            MP.try ('\r' <$ MP.string "\\r") <|>
            MP.try ('\NUL' <$ MP.string "\\0") <|>
            MP.try ('\\' <$ MP.string "\\\\") <|>
            MP.noneOf "\\\""


(<:>) :: (MP.Stream a, MP.Stream b) => MP.Parsec () a b -> MP.Parsec () b c -> MP.Parsec () a c
(<:>) p1 p2 = p1 >>= (\res -> case res of
    Right x -> pure x
    Left err -> MP.customFailure ()) . MP.parse p2 ""


