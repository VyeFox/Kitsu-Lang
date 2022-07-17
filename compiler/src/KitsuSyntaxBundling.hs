{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module KitsuSyntaxBundling (SyntaxBundle(..), KitParseMonad(..), SyntaxReflection(..), ParseKernel) where

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

data SyntaxBundle m e = SyntaxBundle {
        keywords :: MP.Parsec e String (),
        extensions :: SyntaxReflection m e -> (MP.Parsec e String () -> MP.Parsec e String (m Expression)), -- inner expression component
        statics :: SyntaxReflection m e -> MP.Parsec e String (m ()) -- global line
    }

data SyntaxReflection m e = SyntaxReflection {
        rTextName :: MP.Parsec e String String,
        rSymbolName :: MP.Parsec e String String,
        rExpression :: MP.Parsec e String () -> MP.Parsec e String (m Expression), -- <exp>
        rArrowFunc :: forall a. (Expression -> Expression) -> MP.Parsec e String () -> MP.Parsec e String (String -> m a -> m a), -- <arg> => <body>
        rStatic :: MP.Parsec e String (m ()) -- <definition>;
    }

instance (Ord e) => Semigroup (SyntaxBundle m e) where
    a <> b = SyntaxBundle {
        keywords = MP.try (keywords a) <|> keywords b,
        extensions = \r stop -> MP.try (extensions a r stop) <|> extensions b r stop,
        statics = \r -> MP.try (statics a r) <|> statics b r
    }

instance (Ord e) => Monoid (SyntaxBundle m e) where
    mempty = SyntaxBundle {
        keywords = MP.empty,
        extensions = const $ const MP.empty,
        statics = const MP.empty
    }