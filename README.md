# Kitsu-Lang
Kitsu aims to apply the functional programming principles found in Haskell to an easy to use scripting language like JS.

GHC-8.10.7 is used for stable HLS-1.7.0.0 integration.

## Type System

A closure is an object paired with a function that is only dependent on its state and its argument.

**everything** is a closure.

Closures (of type `SomeType`) are written
* `{state...}::SomeType arg => result` defines type with behaviour
* `SomeType{state...}` uses type to define behaviour

There is also sugar for:
* pure functions with anonymous type: `x => result`
* closures with type `Object`: `{state...}`
* closures of anonymous type: `{state...}::arg => result`

