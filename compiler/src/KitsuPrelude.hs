module KitsuPrelude (preludeDef) where

import KitsuByteCode
import KitsuSeasoning (KitParseMonad(..), TypeDefAttached(..))

tupleDef :: (KitParseMonad m) => m ()
tupleDef = liftTypeDefAttached $
    TypeDefAttached [
    ClosureTypeDef {
        closureName = "Tuple",
        closureSelfAlias = "self",
        closureArgName = "elem",
        closureBody = Closure "Tuple" [
        ("rest", Name "self"),
        ("val", Name "elem")
        ]
    }
    ] [
    ClosureTypeHash {closureHash = ("Tuple", 0)}
    ]
    ()



preludeDef :: (KitParseMonad m) => m ()
preludeDef = do
    tupleDef
    return ()
