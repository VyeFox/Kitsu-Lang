{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
module KitsuSeasoning (Seasoning(..), KitParseMonad(..), TypeDefAttached(..), AfterImportAction(..), ExportNames(..), ParseKernel) where

import KitsuByteCode
import AbstractInstances (Wrapper(..))
import KitsuPreludeConnection (emptyTuple)

import qualified Text.Megaparsec as MP
import Control.Applicative ((<$), (<$>), (<|>))
import Data.Functor.Compose (Compose(..))

data TypeDefAttached a = TypeDefAttached [ClosureTypeDef] [ClosureTypeHash] a deriving (Show)

-- trivial derivations of functor typeclasses
instance Functor TypeDefAttached where
    fmap f (TypeDefAttached xs hs a) = TypeDefAttached xs hs (f a)
instance Applicative TypeDefAttached where
    pure = TypeDefAttached [] mempty
    TypeDefAttached xs hs f <*> TypeDefAttached ys is a = TypeDefAttached (xs ++ ys) (hs ++ is) (f a)
instance Monad TypeDefAttached where
    m >>= f = (\case TypeDefAttached ys is (TypeDefAttached xs hs a) -> TypeDefAttached (xs ++ ys) (hs ++ is) a) (f <$> m)
instance (Semigroup a) => Semigroup (TypeDefAttached a) where
    ma <> mb = (<>) <$> ma <*> mb
instance (Monoid a) => Monoid (TypeDefAttached a) where
    mempty = TypeDefAttached [] [] mempty
instance Foldable TypeDefAttached where
    foldMap f (TypeDefAttached _ _ a) = f a
instance Traversable TypeDefAttached where
    traverse f (TypeDefAttached xs hs a) = TypeDefAttached xs hs <$> f a
instance Wrapper TypeDefAttached where
    unwrap (TypeDefAttached _ _ a) = a

data AfterImportAction a = AfterImportAction [Expression] a deriving (Show)

-- trivial derivations of functor typeclasses
instance Functor AfterImportAction where
    fmap f (AfterImportAction es a) = AfterImportAction es (f a)
instance Applicative AfterImportAction where
    pure = AfterImportAction []
    AfterImportAction es f <*> AfterImportAction fs a = AfterImportAction (es ++ fs) (f a)
instance Monad AfterImportAction where
    m >>= f = (\case AfterImportAction fs (AfterImportAction es a) -> AfterImportAction (es ++ fs) a) (f <$> m)
instance (Semigroup a) => Semigroup (AfterImportAction a) where
    ma <> mb = (<>) <$> ma <*> mb
instance (Monoid a) => Monoid (AfterImportAction a) where
    mempty = AfterImportAction [] mempty
instance Foldable AfterImportAction where
    foldMap f (AfterImportAction _ a) = f a
instance Traversable AfterImportAction where
    traverse f (AfterImportAction es a) = AfterImportAction es <$> f a
instance Wrapper AfterImportAction where
    unwrap (AfterImportAction _ a) = a

data ExportNames a = ExportNames [String] a deriving (Show)

-- trivial derivations of functor typeclasses
instance Functor ExportNames where
    fmap f (ExportNames ns a) = ExportNames ns (f a)
instance Applicative ExportNames where
    pure = ExportNames []
    ExportNames ns f <*> ExportNames ms a = ExportNames (ns ++ ms) (f a)
instance Monad ExportNames where
    m >>= f = (\case ExportNames ms (ExportNames ns a) -> ExportNames (ns ++ ms) a) (f <$> m)
instance (Semigroup a) => Semigroup (ExportNames a) where
    ma <> mb = (<>) <$> ma <*> mb
instance (Monoid a) => Monoid (ExportNames a) where
    mempty = ExportNames [] mempty
instance Foldable ExportNames where
    foldMap f (ExportNames _ a) = f a
instance Traversable ExportNames where
    traverse f (ExportNames ns a) = ExportNames ns <$> f a
instance Wrapper ExportNames where
    unwrap (ExportNames _ a) = a

-- custom side effect interface
class Monad m => KitParseMonad m where
  liftTypeDefAttached :: TypeDefAttached a -> m a
  asTypeDefAttached :: m a -> TypeDefAttached a
  liftAfterImportAction :: AfterImportAction a -> m a
  asAfterImportAction :: m a -> AfterImportAction a
  liftExportNames :: ExportNames a -> m a
  asExportNames :: m a -> ExportNames a

newtype ParseKernel a = ParseKernel {getParseKernel :: Compose TypeDefAttached (Compose ExportNames AfterImportAction) a}

-- wrapper derivations for functor typeclasses
instance Functor ParseKernel where
    fmap f x = ParseKernel $ f <$> getParseKernel x
instance Applicative ParseKernel where
    pure = ParseKernel . pure
    kerf <*> kera = ParseKernel $ getParseKernel kerf <*> getParseKernel kera
instance Monad ParseKernel where
    kera >>= func = ParseKernel $ getParseKernel kera >>= (getParseKernel . func)
instance (Semigroup a) => Semigroup (ParseKernel a) where
    kera <> kerb = ParseKernel $ getParseKernel kera <> getParseKernel kerb
instance (Monoid a) => Monoid (ParseKernel a) where
    mempty = ParseKernel mempty

-- Monad composition imported from Abstractinstances
instance KitParseMonad ParseKernel where
    liftTypeDefAttached tda = ParseKernel . Compose $ pure <$> tda
    asTypeDefAttached ker = unwrap <$> getCompose (getParseKernel ker)
    liftAfterImportAction aia = ParseKernel . Compose . pure . Compose . pure $ aia
    asAfterImportAction ker = unwrap . getCompose . unwrap . getCompose $ getParseKernel ker
    liftExportNames exp = ParseKernel . Compose $ pure . Compose $ pure <$> exp
    asExportNames ker = unwrap <$> getCompose ((unwrap . getCompose) (getParseKernel ker))

instance (Show a) => Show (ParseKernel a) where
    show ker = show $ getCompose <$> getCompose (getParseKernel ker)

-- TODO: ...Till refactor aware.
data Seasoning m e = Seasoning {
        salt :: MP.Parsec e String (m Literal), -- how to parse literals
        sugar :: (MP.Parsec e String () -> MP.Parsec e String (m Expression)) -> (MP.Parsec e String () -> MP.Parsec e String (m Expression)), -- inner expression component
        herbs :: (MP.Parsec e String () -> MP.Parsec e String (m Expression)) -> MP.Parsec e String (m ()) -- global line
    }

instance (Ord e) => Semigroup (Seasoning m e) where
    a <> b = Seasoning {
        salt = MP.try (salt a) <|> salt b,
        sugar = \exp stop -> MP.try (sugar a exp stop) <|> sugar b exp stop,
        herbs = \exp -> MP.try (herbs a exp) <|> herbs b exp
    }

instance (Ord e) => Monoid (Seasoning m e) where
    mempty = Seasoning {
        salt = MP.empty,
        sugar = const $ const MP.empty,
        herbs = const MP.empty
    }