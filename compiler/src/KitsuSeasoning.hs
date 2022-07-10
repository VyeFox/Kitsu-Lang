{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module KitsuSeasoning (Seasoning(..), KitParseMonad(..), ParseKernel) where

import KitsuByteCode
import KitsuPreludeConnection (emptyTuple)

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Data.Functor.Compose (Compose(..))

-- custom side effect interface
class Monad m => KitParseMonad m where
  defineType :: ClosureTypeDef -> m a -> m a
  defineVal :: (Maybe String, Expression) -> m a -> m a
  toObject :: m Expression -> KitsuObjectNotation -- requires expression as subject
  toModule :: m ([String], [String]) -> KitsuModule -- requires imports and exports

newtype ParseKernel a = ParseKernel {getParseKernal :: ([ClosureTypeDef], [(Maybe String, Expression)], a)} deriving (Functor, Applicative, Monad)

instance KitParseMonad ParseKernel where
    defineType t = (*>) $ ParseKernel ([t], [], ())
    defineVal v = (*>) $ ParseKernel ([], [v], ())
    toObject (ParseKernel (ts, vs, e)) = KitsuObjectNotation ts $ CoDef vs e
    toModule (ParseKernel (ts, vs, (is, es))) = KitsuModule ts vs is es

instance (Show a) => Show (ParseKernel a) where
    show = show . getParseKernal

data Seasoning m e = Seasoning {
        reservations :: MP.Parsec e String (),
        salt :: MP.Parsec e String (m Literal), -- how to parse literals
        sugar :: MP.Parsec e String () -> (MP.Parsec e String () -> MP.Parsec e String (m Expression)) -> (MP.Parsec e String () -> MP.Parsec e String (m Expression)), -- inner expression component
        herbs :: MP.Parsec e String () -> (MP.Parsec e String () -> MP.Parsec e String (m Expression)) -> MP.Parsec e String (m ()) -- global line
    }

instance (Ord e) => Semigroup (Seasoning m e) where
    a <> b = Seasoning {
        reservations = MP.try (reservations a) <|> reservations b,
        salt = MP.try (salt a) <|> salt b,
        sugar = \res exp stop -> MP.try (sugar a res exp stop) <|> sugar b res exp stop,
        herbs = \res exp -> MP.try (herbs a res exp) <|> herbs b res exp
    }

instance (Ord e) => Monoid (Seasoning m e) where
    mempty = Seasoning {
        reservations = MP.empty,
        salt = MP.empty,
        sugar = const $ const $ const MP.empty,
        herbs = const $ const MP.empty
    }