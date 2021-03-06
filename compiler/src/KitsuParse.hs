module KitsuParse (parseModule) where

import KitsuByteCode
import KitsuSyntaxBundling (SyntaxBundle(..), KitParseMonad(..), SyntaxReflection(..))
import KitsuSyntaxBundles (escapedChar)

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Functor.Compose (Compose (Compose), getCompose)

parseModule :: (Ord e, KitParseMonad m) => SyntaxReflection m e -> MP.Parsec e String KitsuModule
parseModule r = do
    MP.space *> MP.string "module" *> MP.space *> MP.char '{'
    imps <- MP.manyTill (MP.space *> imp) (MP.try $ MP.space *> MP.char '}')
    MP.space *> MP.char '('
    exps <-
        MP.try ([] <$ MP.space <* MP.char ')') <|>
        ((\(xs, x) -> xs ++ [x]) <$> MP.manyTill_ (MP.space *> (MP.try (rTextName r) <|> rSymbolName r) <* MP.space <* MP.char ',')
        (MP.try $ MP.space *> (MP.try (rTextName r) <|> rSymbolName r) <* MP.space <* MP.char ')'))
    global <- MP.manyTill (MP.space *> rStatic r) (MP.try $ MP.space *> MP.eof)
    return $ toModule $ foldl (<*) (pure (imps, exps)) global
    where
        imp = MP.char '\"' *> MP.manyTill escapedChar (MP.char '\"')

