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

There is a comprehensive `Seasoning` system for injecting syntax into the language.

## Project structure

The most important files for understanding the project and its functionality are:

* [**Builtins**](builtins) - contains all of the `.kitsu` files that are built into the language, where `prelude` is imported automatically.
* [**Compiler**](compiler) - contains the parser code that consists of:
  * [*ByteCode*](compiler/src/KitsuByteCode.hs) - a module containing target for the parser.
  * [*RuntimeConnection*](compiler/src/KitsuRuntimeConnection.hs) - a module containing expression builder methods that rely on runtime provided closures and prelude functions.
  * [*SyntaxBundling*](compiler/src/KitsuSyntaxBundling.hs) - a module defining ways to extend the language with extra syntax.
  * [*SyntaxBundles*](compiler/src/KitsuSyntaxBundles.hs) - a module containing a collection of seasonings.
  * [*Components*](compiler/src/KitsuComponents.hs) - a module containing expression parsers that utilise seasoning.
* [**Notes**](notes) - contains notes on the language, specifically:
  * [*Closures*](notes/closures.md) - explains the closure system.
  * [*Expressions*](notes/expressions.md) - details how expressions are parsed.
  * [*Runtime*](notes/runtimeRequirements.md) - specifies the runtime requirements for the language so that *PreludeConnection* can properly function.
  * [*Prelude*](notes/preludeExports.md) - specifies the prelude types and closures that are available to the language.

## Future

* `matches` will be added to the `SyntaxBundle` type for dealing with destructuring.
* A transpiler will be written to convert module byte code into pyhton. 
* Eventually a custom interpreter will be written to parse the languages bytecode.

