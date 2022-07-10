module KitsuPrelude (preludeDef) where

import KitsuByteCode
import KitsuSeasoning (KitParseMonad(..))

tupleDef :: (KitParseMonad m) => m a -> m a
tupleDef = defineType
    ClosureTypeDef {
        closureTypeName = "Tuple",
        closureTypeHash = 0,
        closureTypeBody = Just ("elem", Closure "Tuple" [
        ("rest", Name "self"),
        ("val", Name "elem")
        ])
    }



preludeDef :: (KitParseMonad m) => m a -> m a
preludeDef =
    tupleDef
