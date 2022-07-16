module KitsuPreludeConnection where

import KitsuByteCode

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