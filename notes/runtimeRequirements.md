# Runtime Requirements

The runtime must provide:
* A type variable `Type` that is the type of all types that, when called, yields the type of the argument.
* Non type coercing mathematics functions `add` and `mul`
* The rational number function `div` which expects an `int` and a `nat` as arguments.
* The modular number function `rdiv` which expects an `int` and a `nat` as arguments and returns a tuple of the quotient (`Int`) and remainder (`Nat`).
* Non type coercing ordering functions `eq` and `lt`.
* All type names must be represented as closures with no properties, type `Type`, that when called create a copy of the argument but with the type set to the type object.
* The property accessor types `Get` and `Has` must be defined as the types of `.<prop>` and `?<prop>` respectively.
* All literal and primitive types `Bool`, `Nat`, `Int`, `Rat`, `Byte`, `Char`, `Async`, `Lazy`, `Atomic` and `Object` must also be defined.
* All anonymous types (for example the results of calling a `Bool` with one argument) must always be `false` when compared to any type (including themselves).