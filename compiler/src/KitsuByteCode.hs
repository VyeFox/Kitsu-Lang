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

-- Defines the behabiour of a function call
data ClosureTypeDef = ClosureTypeDef {
    closureName :: String,
    closureSelfAlias :: String,
    closureArgName :: String,
    closureBody :: Expression
} deriving (Show)

-- Stores the hash of a closure definition body for checking equality across different processes
newtype ClosureTypeHash = ClosureTypeHash {
    closureHash :: (String, Integer)
} deriving (Show)

-- General form of an expression
data Expression
    = Name String -- local variable
    | Lit Literal -- literal
    | Prim Primitive -- primitive
    | Apply Expression Expression -- function application
    | Closure String [(String, Expression)] -- closure
    | GetProp Expression String -- get property of a closure
    | CoDef [(String, Expression)] Expression -- define multiple values in paralelle for self / sister reference
    deriving (Show)

data KitsuGlobal = KitsuGlobal {
    kitsuDependencies :: [String], -- list of modules that this file depends on
    kitsuTypeDefs :: [ClosureTypeDef],
    kitsuVarDefs :: [(String, Expression)]
} deriving (Show)

-- Form of a module
data Module = Module
    KitsuGlobal
    [ClosureTypeHash]
    [String] -- exported names
    deriving (Show)

-- Form of process interface components
data ProcessCallDef = ProcessCallDef {
    procCallName :: String,
    procCallArg :: String,
    procCallExpr :: Expression
} deriving (Show)

-- Form of a program, variables 'cmdargs' and 'process' are automatically added to the environment
data Program = Program 
    KitsuGlobal
    [ClosureTypeHash]
    [ProcessCallDef] -- process interface
    deriving (Show)

-- Object notation
data KitsuObjectNotation = KitsuObjectNotation
    [ClosureTypeDef] -- only for lambdas
    [ClosureTypeHash]
    Expression -- object
    [Expression] -- context
    deriving (Show)
