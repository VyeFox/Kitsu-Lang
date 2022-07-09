{-# LANGUAGE LambdaCase #-}
module KitsuParse (parseModule) where

import KitsuByteCode
import KitsuComponents (parseExpression, textName, symbolicName)
import KitsuPrelude (preludeDef)
import KitsuSeasoning (Seasoning(..), KitParseMonad(..), TypeDefAttached(..), ExportNames(..), AfterImportAction(..))

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Functor.Compose (Compose (Compose), getCompose)

-- TODO: ...Till refacor aware.
parseModule :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String KitsuModule
parseModule seasoning = do
    imps <- imports
    names <- deflist
    MP.space <* MP.eof
    let asTDA = asTypeDefAttached names
    let asAIA = asAfterImportAction names
    let asExp = asExportNames names
    return $ KitsuModule
        (KitsuGlobal {
            kitsuDependencies = imps,
            kitsuTypeDefs = (\case TypeDefAttached ds hs a -> ds) asTDA,
            kitsuVarDefs = (\case TypeDefAttached ds hs a -> a) asTDA,
            kitsuAfterDefs = (\case AfterImportAction es a -> es) asAIA
        })
        ((\case TypeDefAttached ds hs a -> hs) asTDA)
        ((\case ExportNames ns a -> ns) asExp)
        where
            imports = MP.many $ MP.try $ MP.space *> MP.string "import" *> MP.space1 *> MP.string "***" <* MP.space <* MP.char ';'
            definition = do
                action <-
                    MP.try ((\str -> liftExportNames (ExportNames [str] str)) <$ MP.string "export" <* MP.space1) <|>
                    (pure <$ MP.notFollowedBy MP.empty)
                name <- MP.try textName <|> symbolicName
                MP.space1
                MP.char '='
                MP.space1
                def <- parseExpression seasoning
                MP.space
                MP.char ';'
                return $ (,) <$> action name <*> def
            defmor = getCompose $ (:) <$> Compose definition
            idmor = getCompose $ id <$ Compose (herbs seasoning (parseExpression seasoning))
            deflist = getCompose $ foldl (\ds f -> f ds) [] <$> Compose (sequenceA <$> MP.many (MP.try $ MP.space *> (MP.try idmor <|> defmor)))

