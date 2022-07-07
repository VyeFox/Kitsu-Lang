module KitsuPreludeConnection where

import KitsuByteCode

-- tuple call operation links element to tuple in new tuple
emptyTuple :: Expression
emptyTuple = Closure "Tuple" []

inline :: Expression -> Expression -> Expression -> Expression
inline operator lhs rhs = Apply operator (Apply (Apply emptyTuple lhs) rhs)
-- a + b => (+) (() a b)