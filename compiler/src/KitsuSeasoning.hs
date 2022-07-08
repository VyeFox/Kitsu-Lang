module KitsuSeasoning (Seasoning(..)) where

import KitsuByteCode

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))

data Seasoning m e = Seasoning {
        salt :: MP.Parsec e String (m Literal), -- how to parse literals
        sugar :: MP.Parsec e String (m Expression) -> MP.Parsec e String (m Expression), -- inner expression component
        herbs :: MP.Parsec e String (m Expression) -> MP.Parsec e String (m ()) -- global line
    }

instance (Ord e) => Semigroup (Seasoning m e) where
    a <> b = Seasoning {
        salt = MP.try (salt a) <|> salt b,
        sugar = \exp -> MP.try (sugar a exp) <|> sugar b exp,
        herbs = \exp -> MP.try (herbs a exp) <|> herbs b exp
    }

instance (Ord e) => Monoid (Seasoning m e) where
    mempty = Seasoning {
        salt = MP.empty,
        sugar = const MP.empty,
        herbs = const MP.empty
    }