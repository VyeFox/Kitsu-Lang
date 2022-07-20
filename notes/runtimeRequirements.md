# Runtime Requirements

The runtime must provide:
* A type variable `Type` that is the type of all types that, when called, yields the type of the argument.
* A function `eq` that checks for equality between two values.
* All type names must be represented as closures with no properties, type `Type`, that when called create a copy of the argument but with the type set to the type object.
* All literal and primitive types `Bool`, `Nat`, `Int`, `Rat`, `Byte`, `Char`, `Async`, `Lazy`, `Atomic` and `Object` must also be defined.
* All anonymous types (for example the results of calling a `Bool` with one argument) must always be `false` when compared to any type (including themselves).