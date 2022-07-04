# Closures
Closures are JS-like objects equiped with an unchanging call operation with behaviour only dependent on its state and its argument.
This allows functors to have their applicative normal form expressed as regular function application.

**everything** is a closure.

Closures are defined with an object component `{...}` and a function component `x => ...`.
Either on their own will be syntactic sugar for a valid closure, where a lone object component has implicit closure type `Object`
and a lone function component has an anonymous closure type.

The syntax for a closure is:

```js
// regular object, function is Object
obj = {
    'x': 3/13,
    'y': 4/13,
    'z': 12/13
};

// anonymous pure function, state is {}
fn = o => (o.x * o.x) + (o.y * o.y) + (o.z * o.z);

// anonymous closure
clo = {
    'val': 1
}:: x => self.val * x;

// typed closure
zero = {
    'x': 0,
    'y': 0,
    'z': 0
}::Vect other => Vect{ //Vect is now a global name, and has been used as a type
    'x': self.x + other.x,
    'y': self.y + other.y,
    'z': self.z + other.z
};
```

...Where the default behaviour of `Object` is to create a copy of the argument with state overriden by the object's properties.

One of the key differences between closures and JS objects is that closures are immutable.