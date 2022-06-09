# Type Definitions
How custom types will be defined and the extent of modifier implications over members.

## Example
The following example code demostraits a custom type definition with multiple patterns and modifiers.
```haskell
X:: type
= A {
    (pu puv => var) u: U; --member modifier
} | B {
    v: (pv puv => mutable) V; --pointer modifier
} | C {
    (pu puv => var) u: U; --member modifier
    v: (pv puv => mutable) V; --pointer modifier
} friend modify_u modify_v; --friend functions
```

Access modifiers can use member modifiers (`fix` and `var`) to define custom access to the object.
Access modifiers can also use access modifiers of the members to restrict access to contained objects.
Type `X` has 3 custom access modifiers `pu`, `pv` and `puv` along with the standard `const` and `mut` modifiers.

This creates an implicit inheritence structure where `const` <- (`pu`, `pv`) <- `puv` <- `mut`.

