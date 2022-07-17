module KitsuStructure where

import KitsuByteCode
import KitsuPreludeConnection

data PropCheck = PropCheck {
        property :: String,
        valueread :: Maybe String,
        inner :: Maybe ObjCheck
    }

data ObjCheck = ObjCheck {
        typecheck :: Maybe String,
        propchecks :: [PropCheck]
    }

propPredicate :: PropCheck -> Expression
propPredicate check = maybe hasprop (\innercheck -> chooseFunc hasprop (pipeFuncs (objPredicate innercheck) (GetProp $ property check)) justfalse) (inner check)
    where
        hasprop = HasProp (property check)
        justfalse = Apply (Lit $ KitBool True) (Lit $ KitBool False) -- x => false

objPredicate :: ObjCheck -> Expression
objPredicate check = foldl andFunc tcheck (propPredicate <$> propchecks check)
    where
        justtrue = Apply (Lit $ KitBool True) (Lit $ KitBool True) -- x => true
        justfalse = Apply (Lit $ KitBool True) (Lit $ KitBool False) -- x => false
        andFunc f g = chooseFunc f g justfalse
        tcheck = maybe justtrue (isType . Name) (typecheck check)



