# Expressions
How functional code will be converted into an expression tree.

There are 3 stages of binding:
- [Grouping](#grouping): `(...)` and `$`
- [Chaining](#chaining): `<>`, `<$>`, `<*>`
- [Applying](#applying): `f x y`

## Grouping
Grouping consists of brackets and bracket sugar like `$`; specifically: \
`...$...` is equivalent to `... (...)`

## Chaining
After grouping, chaining is used to link together the remaining tokens between operators like `<>`; specifically: \
`...<$>...<*>...` is equivalent to `(...<$>...)<*>...` (written as non-operator functions: `(<*>) (...) ((<$>) (...) (...))`)

## Applying
Finally the remaining tokens are parsed left to right, and greedily take remaining tokens until the function is completely applied; specifically: \
`f x y` is equivalent to `(f x) y`
