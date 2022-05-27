# Type Definitions
How custom types will be defined and the extent of modifier implications over members.

## Example
The following example code demostraits a custom type definition with multiple patterns and modifiers.
```haskell
X:: type;
X = ~A {
    - (pu puv => var) u: U; --member modifier
} | +B {
    + v: (pv puv => mutable) V; --access modifier
} | ~C {
    - (pu puv => var) u: U; --member modifier
    + v: (pv puv => mutable) V; --access modifier
} friend modify_u; --friend function
```

Access modifiers can use member modifiers (`fix` and `var`) to define custom access to the object.
Access modifiers can also use access modifiers of the members to restrict access to contained objects.
Type `X` has 3 custom access modifiers `pu`, `pv` and `puv` along with the standard `const` and `mut` modifiers.

This creates an implicit inheritence structure where `const` <- (`pu`, `pv`) <- `puv` <- `mut`.

## Keywords

The `-` keyword on members specifies that the (member | accessor) modifier for that member is (`fix` | `const`) outside of friend functions. \
The `~` keyword on members specifies that the (member | accessor) modifier for that member is (`fix` | `const`) outside of the module. *(default)* \
The `+` keyword on members specifies that the (member | accessor) modifier for that member is as specified in all contexts.

The `-` keyword on patterns specifies that the pattern may be constructed only inside friend functions. \
The `~` keyword on patterns specifies that the pattern may be constructed only inside the module. *(default)* \
The `+` keyword on patterns specifies that the pattern may be constructed in all contexts.

