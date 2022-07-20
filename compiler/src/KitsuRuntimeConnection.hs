module KitsuRuntimeConnection where

import KitsuByteCode

{-
    REQUIRES PRELUDE DEFINED THE FOLLOWING

    ::Tuple other => Tuple{rest: self, val: other};
    id = x => x;

    REQUIRES RUNTIME TO DEFINE THE FOLLOWING

    Type
    eq

    IMPOSES STANDARDS

    x <> y represents: (<>) $ () x y
-}

-- identity function
kitsuId :: Expression
kitsuId = Name "id"

-- tuple call operation links element to tuple in new tuple
emptyTuple :: Expression
emptyTuple = Closure "Tuple" []

-- a + b => (+) (() a b)
inline :: Expression -> Expression -> Expression -> Expression
inline operator lhs rhs = Apply operator (Apply (Apply emptyTuple lhs) rhs)

-- assert runtime provides primitive equality checker "eq"
equality :: Expression -> Expression -> Expression
equality a = Apply (Apply (Name "eq") a)

-- assert runtime provides reflective "type" object that provides `typeof` behaviour
getType :: Expression -> Expression
getType = Apply (Name "Type")
