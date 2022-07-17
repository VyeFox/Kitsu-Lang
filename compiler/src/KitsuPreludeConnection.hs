module KitsuPreludeConnection where

import KitsuByteCode

{-
    REQUIRES PRELUDE DEFINED THE FOLLOWING

    ::Tuple other => Tuple{rest: self, val: other};
    id = x => x;
    run = f => f ();
    choose = pred => [pred]:: f => [pred, f]:: g => [pred, f, g]:: x => pred x f g x;
    pipe = f => [f]:: g => [f, g]:: x => f $ g x;

    REQUIRES RUNTIME TO DEFINE THE FOLLOWING

    Type
    eq

    IMPOSES STANDARDS

    x <> y represents: (<>) $ () x y
-}

-- identity function
kitsuId :: Expression
kitsuId = Name "id"

-- run f = f ()
kitsuRun :: Expression
kitsuRun = Name "run"

-- pipe f g x = f (g x)
pipeFuncs :: Expression -> Expression -> Expression
pipeFuncs f = Apply (Apply (Name "pipe") f)

-- tuple call operation links element to tuple in new tuple
emptyTuple :: Expression
emptyTuple = Closure "Tuple" []

-- a + b => (+) (() a b)
inline :: Expression -> Expression -> Expression -> Expression
inline operator lhs rhs = Apply operator (Apply (Apply emptyTuple lhs) rhs)

-- choose pred f g
chooseFunc :: Expression -> Expression -> Expression -> Expression
chooseFunc pred f = Apply (Apply (Apply (Name "choose") pred) f)

-- assert runtime provides primitive equality checker "eq"
equality :: Expression -> Expression -> Expression
equality a = Apply (Apply (Name "eq") a)

-- assert runtime provides reflective "type" object that provides `typeof` behaviour
getType :: Expression -> Expression
getType = Apply (Name "Type")

-- type check
isType :: Expression -> Expression
isType t = pipeFuncs (Apply (Name "eq") t) (Name "Type")