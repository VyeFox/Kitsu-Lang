
module AbstractInstances (Wrapper(..)) where

import Data.Functor.Compose (Compose(..))
import Control.Monad (Monad(..), join)

-- because someone fucking had to

instance (Semigroup (f (g a))) => Semigroup (Compose f g a) where
    fga <> fgb = Compose $ getCompose fga <> getCompose fgb

instance (Monoid (f (g a))) => Monoid (Compose f g a) where
    mempty = Compose mempty

instance (Monad f, Monad g, Traversable g) => Monad (Compose f g) where
    fga >>= func = Compose $ join <$> join (sequenceA <$> getCompose (getCompose <$> (func <$> fga)))

class Wrapper f where
    unwrap :: f a -> a

instance (Wrapper f, Wrapper g) => Wrapper (Compose f g) where
    unwrap = unwrap . unwrap . getCompose
    

