module ComposeTools where

import Data.Functor.Compose (Compose (Compose), getCompose)

fCompose :: (f (g a) -> f (g b)) -> Compose f g a -> Compose f g b
fCompose fn = Compose . fn . getCompose

inner :: (Functor f) => (g a -> g b) -> Compose f g a -> Compose f g b
inner fn = Compose . (<$>) fn . getCompose 
