# Procedures
This is a proposal for implimenting procedural logic utilising "frames", which act as a functional simplification to stack frames.

Syntactic objects:
- [Initialising procedure](#initialising-procedure): `{y = f x;} g y`
- [Terminal procedure](#terminal-procedure): `{do y;} f y`

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

## Terminal procedure
The usefullness of this expression is dependent on the existance of mutable variables.
This is syntactic sugar for an Initialising procedure that runs after the bound expression; specifically: \
`!{do y;} g y` is equivalent to `((_) -> {do y;} _) g y`

This may potentially be extended to be able to use defined variables within any sub frames inside the expression.


