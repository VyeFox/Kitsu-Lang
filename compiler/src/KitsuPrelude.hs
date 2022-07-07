module KitsuPrelude where

import KitsuByteCode
import KitsuComponents (KitParseMonad, liftTypeDefAttached, TypeDefAttached (TypeDefAttached))

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