# Procedures
This is a proposal for implimenting procedural logic utilising "frames", which act as a functional simplification to stack frames.

Syntactic objects:
- [Initialising procedure](#initialising-procedure): `{y = f x;} g y`
- [Inline procedure](#inline-procedure): `g |y = f x|{do y;}`
- [Terminatal procedure](#terminatal-procedure): `!{do x} g (f x)`

## Binding
Set to bind after operators (`<>`) and encompas as large an expression as possible e.g. a function and its arguments.
This lives in the "Applying" stage of the binding process.

## Usage
Example of procedure frames being used with an asynchonous function:
The following code sums
```
{f = async x;} !{await f;} $ op_get f <> op_get f <> op_get f
```

## Initialising procedure
The initialising procedure is set to run before the bound expression and allows usage of defined variables within the expression.
This allows values to be taken by reference and allows definitions of mutable variables.

## Inline procedure
The usefullness of this expression is dependent on the existance of mutable variables.
This is syntactic sugar for an Initialising procedure that evaluates to a pre-defined value; specifically:
`|y = f x|{do y;}` is equivalent to `{y = f x; do y;} y`

## Terminatal procedure
The usefullness of this expression is dependent on the existance of mutable variables.
This is syntactic sugar for an Initialising procedure that runs after an expression; specifically:
`!{do x;} expr` is equivalent to `const expr ({do x;} Void)`
This may potentially be extended to be able to use defined variables within any sub frames inside the expression.


