# Built-in and Standard Types
This section describes the built-in and standard types in Kitsu.

## Primitive Types
These types cannot be re-defined within the confines of the language:
* `Async` A type that represents a promise, the value is aquired by calling the instance with a throw-away value such as an empty tuple.
* `Atomic` A special wrapper type that allows unique access to a value.
* `Trait` A function with overloadable implementations.
* `Bool` Choses between two values.
* `Nat` A type that represents a unsigned integer.
* `Int` A type that represents a signed integer.
* `Rat` A type that represents a rational number.
* `Byte` A type that represents a byte.
* `Char` A type that represents a single character.
* `Type` A type that represents a type.

The signatures of these types are:
```js
Async: () => X;
Atomic: Maybe (X => X) => Maybe X; // "nothing" is returned if the action would result in a deadlock.
Trait: (X => Maybe Y) => (X => Bool); // returns condition to check if overload is reached for a given value.
Bool: * => * => *; // (x => y => x) if true, (x => y => y) if false.
Nat: (X => X) => (X => X); // iterate value `n` times, where `n` is value represented by the Natural number.
Type: * => Type; // returns the type of the argument
```

## Primitive operations
These operations cannot be re-defined within the confines of the language and are often represented by keywords:
* `async` A keyword that represents an asynchronous operation.
* `atomic` A keyword that represents an atomic value.
* `is` Checks for referential or primitive equality.
* `obj.prop` Accesses a property of an object, throws an error if the property does not exist.
* `obj?prop` Checks if a property of an object exists.

example:
```js
const id = {
    'count': atomic 0
}:: _ => self.count $ just $ x => x + 1;
```

## Built-in Types
These types are defined in the language and are available to all processes:
* `Maybe` A type that represents an optional value.
* `Either` A type that represents a value that can be either a left or right value.
* `Real` A type that represents a real number.
* `Fold` A tuple modeled as a linked list.
* `Match` A queryable predicate for deep type checking.

implimentation:
```js
const just = x => {
    'val': x
}::Maybe other => (self?val && other?val) Maybe{
    'val': self.val other.val
} Maybe{
    // empty
};

const nothing = Maybe{
    // empty
};

// castable to Maybe
const () = {}::Fold x => Fold{
    'val': x,
    'next': self
};

// castable to Maybe
const left = x => {
    'val': x
}::Either other => (self?val && other?val) Either{
    'val': self.val other.val
} $ (self?alt && other?alt) Either{
    'alt': self.alt other.alt
} self;

const right = x => Either{'alt': x};
```
