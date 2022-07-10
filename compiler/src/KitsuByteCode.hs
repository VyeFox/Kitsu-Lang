module KitsuByteCode where

-- Simple Data Types
data Literal
    = KitBool Bool
    | KitNat Int -- assume that the number is positive
    | KitInt Int
    | KitRat Rational
    | KitByte Int -- assume that the number is natural and less than 256
    | KitChar Char
    deriving (Eq, Show)

-- Functors not Definable Within the Constrains of the Language
data Primitive
    = KitAsync Expression -- process expression asynchronously
    | KitAtomic Expression -- initial value of mutable variable
    deriving (Show)

-- General form of an expression
data Expression
    = Name String -- local variable
    | Lit Literal -- literal
    | Prim Primitive -- primitive
    | Apply Expression Expression -- function application
    | Closure String [(String, Expression)] -- closure
    | GetProp Expression String -- get property of a closure
    | CoDef [(Maybe String, Expression)] Expression -- simple procedural logic
    deriving (Show)

-- Defines the behabiour of a function call
data ClosureTypeDef = ClosureTypeDef {
    closureTypeName :: String,
    closureTypeHash :: Integer,
    closureTypeBody :: Maybe (String, Expression)
} deriving (Show)

-- Form of a module
data KitsuModule = KitsuModule
    [ClosureTypeDef] -- type definitions
    [(Maybe String, Expression)] -- assignments | actions
    [String] -- imported modules
    [String] -- exported names
    deriving (Show)

-- Object notation
data KitsuObjectNotation = KitsuObjectNotation
    [ClosureTypeDef] -- only `Just` for lambdas, otherwise defer definition to reciever
    Expression -- object
    deriving (Show)
