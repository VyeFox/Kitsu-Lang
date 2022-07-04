
-- Simple Data Types
data Literal
    = KitBool Bool
    | KitNat Int -- assume that the number is positive
    | KitInt Int
    | KitRat Rational
    | KitByte Int -- assume that the number is natural and less than 256
    | KitChar Char
    | KitClosureAddress Integer -- only valid in the context of an object notation

-- Types not Definable Within the Constrains of the Language
data Primitives
    = KitAsync Expression -- process expression asynchronously
    | KitAtomic Expression -- initial value of mutable variable
    | KitTrait Expression -- default beheavior of overridable func

-- Defines the behabiour of a function call
data ClosureTypeDef = ClosureTypeDef {
    closureName :: String,
    closureSelfAlias :: String,
    closureArgName :: String,
    closureBody :: Expression
}

-- General form of an expression
data Expression
    = Name String -- local variable
    | Lit Literal -- literal
    | App Expression Expression -- function application
    | Closure String [(String, Expression)] -- closure
    | GetProp Expression String -- get property of a closure

data KitsuGlobal = KitsuGlobal {
    kitsuDependencies :: [String], -- list of modules that this file depends on
    kitsuTypeDefs :: [ClosureTypeDef],
    kitsuVarDefs :: [(String, Expression)]
}

-- Form of a module
data Module = Module
    KitsuGlobal
    [String] -- exported names

-- Form of process interface components
data ProcessCallDef = ProcessCallDef {
    procCallName :: String,
    procCallArg :: String,
    procCallExpr :: Expression
}

-- Form of a program, variables 'cmdargs' and 'process' are automatically added to the environment
data Program = Program 
    KitsuGlobal
    [ProcessCallDef]

-- Object notation, KitClosureAddress is valid here
data KitsuObjectNotation = KitsuObjectNotation
    Expression
    [Expression]
