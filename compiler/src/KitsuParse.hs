module KitsuParse () where

import KitsuByteCode
import KitsuComponents (parseExpression, textName, symbolicName)
import KitsuPrelude (preludeDef)
import KitsuSeasoning (Seasoning(..), KitParseMonad(..), TypeDefAttached(..))

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Control.Monad (join, return)
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Error as MP
import Data.Functor.Compose (Compose (Compose), getCompose)

-- parseModule :: (Ord e, KitParseMonad m) => Seasoning m e -> MP.Parsec e String KitsuModule
-- parseModule seasoning =
--     _
--         where
--             definition = getCompose $ (,)
--                 <$> Compose ((<$>) pure $ MP.try textName <|> symbolicName)
--                 <*> Compose (MP.space1 *> MP.char '=' *> MP.space1 *> parseExpression seasoning)

