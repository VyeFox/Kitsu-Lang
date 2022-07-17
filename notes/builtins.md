# Built-in and Standard Types
This section describes the built-in and standard types in Kitsu.

## Primitive Types
These types cannot be re-defined within the confines of the language:
* `Object` A type that copies another object and ovverides its properties.
* `Async` A type that represents a promise, the value is aquired by calling the instance with a throw-away value such as an empty tuple.
* `Lazy` A type that represents a lazy value, the value is aquired by calling the instance with a throw-away value such as an empty tuple.
* `Atomic` A special wrapper type that allows unique access to a value.
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
Lazy: () => X;
Atomic: (X => Y) => Maybe Y; // "nothing" is returned if the action would result in a deadlock.
Bool: * => * => *; // (x => y => x) if true, (x => y => y) if false.
Nat: (X => X) => (X => X); // iterate value `n` times, where `n` is value represented by the Natural number.
Type: * => Type; // returns the type of the argument
```

## Primitive operations
These operations cannot be re-defined within the confines of the language and are often represented by keywords:
* `async` A keyword that represents an asynchronous operation.
* `lazy` A keyword that represents a lazy value.
* `atomic` A keyword that represents an atomic value.
* `eq` Checks for equality.
* `obj.prop` Accesses a property of an object, throws an error if the property does not exist.
* `?prop` Checks if a property of an object exists.

example:
```js
id = {
    'count': atomic 0
}:: _ => self.count $ just $ x => x + 1;
```

## Built-in Types
These types are defined in the language and are available to all modules:
* `Maybe` A type that represents an optional value.
* `Either` A type that represents a value that can be either a left or right value.
* `Tuple` A tuple modeled as a linked list.
* `TraitF` A function with overloadable implementation.
