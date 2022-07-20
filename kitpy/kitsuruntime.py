""" Runtime support for the Kitsu programming language for Python. """

import math
from fractions import Fraction
from threading import Thread

# === EXCEPTION TYPES ===

class KitsuException(Exception):
    """ Base class for all Kitsu exceptions. """
    pass

class KitsuExplicitError(KitsuException):
    """ Base class for all errors raised from calling an error value. """
    pass

class KitsuKeyError(KitsuException):
    """ Raised when a key is not found in a closure. """
    pass

# === FORWARD DEFINE HELPER FUNCTIONS ===

to_bool = id
to_nat = id
to_int = id
to_rat = id
to_byte = id
to_char = id
to_getter = id
to_checker = id

wrap_as_maybe = id # connect to prelude
make_kit_tuple = lambda *xs: tuple(xs)

# === ABSTRACT TYPES ===

class Stateless:
    """ Base class for all stateless objects. """
    
    def __getitem__(self, key):
        raise KitsuKeyError("Object has no state")

    def __contains__(self, key):
        return False

# === TYPES ===

closuretype = None

class ClosureType(Stateless):
    """ A closure that represents a type of a closure. """
    def __init__(self, cast):
        self.cast = cast

    def __call__(self, args):
        return self.cast(args)

    def typeof(self):
        return closuretype

closuretype = ClosureType(lambda clo: clo.typeof()) # closuretype

class Closure:
    """ Lambda driven closure """

    def __init__(self, ctype, state, func):
        self.ctype = ctype
        self.state = state
        self.func = func

    def __call__(self, arg):
        return self.func(self.state, arg)

    def __getitem__(self, key):
        return self.state[key]

    def __contains__(self, key):
        return key in self.state

    def typeof(self):
        return self.ctype

# primitives

kitasync = None

class Async(Stateless):
    """ Asynchronous value. """

    def __init__(self, func):
        self.ret = None
        def callback(result):
            self.ret = result
        self.proc = Thread(target=func, args=(callback,))
        self.proc.start()

    def __call__(self, arg):
        self.proc.join()
        return self.ret

    def typeof(self):
        return kitasync

kitasync = ClosureType(lambda clo: Async(lambda cb: cb(clo)))

kitlazy = None

class Lazy(Stateless):
    """ Lazy value. """

    def __init__(self, func):
        self.ret = None
        self.func = func

    def __call__(self, arg):
        if self.ret is None:
            def callback(result):
                self.ret = result
            self.func(callback)
        return self.ret

    def typeof(self):
        return kitlazy

kitlazy = ClosureType(lambda clo: Lazy(lambda cb: cb(clo)))

class Atomic(Stateless):
    """ Mutable value. """

    def __init__(self, value):
        self.value = value

    # TODO: impliment deadlock resolution
    def __call__(self, update):
        self.value = update(self.value)
        return wrap_as_maybe(self.value)

    def typeof(self):
        return kitatomic

kitatomic = ClosureType(lambda clo: Atomic(clo))

# literals

kitbool = None
_kittruecurry = None
_kitfalsecurry = None

class Bool(Stateless):
    """ Boolean value. """

    class TrueCurry(Stateless):
        """ Result of calling a boolean value with true. """

        def __init__(self, value):
            self.value = value

        def __call__(self, arg):
            return self.value

        def typeof(self):
            return _kittruecurry

    class FalseCurry(Stateless):
        """ Result of calling a boolean value with false. """

        def __init__(self, value):
            pass

        def __call__(self, arg):
            return arg

        def typeof(self):
            return _kitfalsecurry

    def __init__(self, value):
        self.value = value

    def __call__(self, arg):
        if self.value:
            return Bool.TrueCurry(arg)
        else:
            return Bool.FalseCurry(arg)

    def typeof(self):
        return kitbool

kitbool = ClosureType(to_bool)
_kittruecurry = ClosureType(Bool.TrueCurry)
_kitfalsecurry = ClosureType(Bool.FalseCurry)

kitnat = None
_kitnatcurry = None

class Nat(Stateless):
    """ Natural number. """

    class NatCurry(Stateless):
        """ Result of calling a natural number. """

        def __init__(self, n, clo):
            self.n = n
            self.clo = clo

        def __call__(self, arg):
            for _ in [None] * self.n:
                arg = self.clo(arg)
            return arg

        def typeof(self):
            return _kitnatcurry

    def __init__(self, value):
        self.value = value

    def __call__(self, arg):
        return Nat.NatCurry(self.value, arg)

    def typeof(self):
        return kitnat

kitnat = ClosureType(to_nat)
_kitnatcurry = ClosureType(lambda clo: Nat.NatCurry(1, clo))

kitint = None
_kitintcurry = None
__kitintcurrycurry = None

class Int(Stateless):
    """ Integer. """

    class IntCurry(Stateless):
        """ Result of calling an integer. """

        class IntCurryCurry(Stateless):
            """ Result of calling a called integer. """

            def __init__(self, value, clo, inv):
                self.value = value
                self.clo = clo
                self.inv = inv

            def __call__(self, arg):
                if self.value == 0:
                    return arg
                elif self.value > 0:
                    for _ in [None] * self.value:
                        arg = self.clo(arg)
                    return arg
                else:
                    for _ in [None] * (- self.value):
                        arg = self.inv(arg)
                    return arg

            def typeof(self):
                return __kitintcurrycurry

        def __init__(self, value, clo):
            self.value = value
            self.clo = clo

        def __call__(self, arg):
            return Int.IntCurry.IntCurryCurry(self.value, self.clo, arg)

        def typeof(self):
            return _kitintcurry


    def __init__(self, value):
        self.value = value

    def __call__(self, arg):
        return Int.IntCurry(self.value, arg)

    def typeof(self):
        return kitint

kitint = ClosureType(to_int)
_kitintcurry = ClosureType(lambda clo: Int.IntCurry(1, clo))
__kitintcurrycurry = ClosureType(lambda inv: Int.IntCurry.IntCurryCurry(1, id, inv))

kitrat = None

class Rat(Stateless):
    """ Rational number. """

    def __init__(self, value):
        self.value = value

    def __call__(self, arg):
        return arg(make_kit_tuple(Int(self.value.numerator), Nat(self.value.denominator)))

    def typeof(self):
        return kitrat

kitrat = ClosureType(to_rat)

kitbyte = None

class Byte(Stateless):
    """ Byte. """

    def __init__(self, value):
        self.value = value

    def __call__(self, arg):
        return arg(make_kit_tuple(*[Bool(bool(self.value & (128 >> i))) for i in range(8)]))

    def typeof(self):
        return kitbyte

kitbyte = ClosureType(to_byte)

kitchar = None

class Char(Stateless):
    """ Character. """

    def __init__(self, char):
        self.value = char

    def __call__(self, arg):
        raise KitsuException("Char has no associated action.")

    def typeof(self):
        return kitchar

kitchar = ClosureType(to_char)

class Error:
    """ Error value. """

    def __init__(self, message):
        self.message = message

    def __call__(self, arg):
        raise KitsuExplicitError(self.message)

    def typeof(self):
        return KitsuExplicitError(self.message)

# accessors

kitget = None

class Get(Stateless):
    """ Accessor. """

    def __init__(self, propname):
        self.propname = propname

    def __call__(self, arg):
        return arg[self.propname]

    def typeof(self):
        return kitget

kitget = ClosureType(to_getter)

kithas = None

class Has(Stateless):
    """ Checker. """

    def __init__(self, propname):
        self.propname = propname

    def __call__(self, arg):
        return self.propname in arg

    def typeof(self):
        return kithas

kithas = ClosureType(to_checker)

# === HELPERS ===

accessor_types = {Get, Has}
logic_literals = {Bool, Nat, Int, Rat, Byte}

def to_bool(clo):
    """ Convert a logical literal to a boolean value. """
    if clo.typeof() in logic_literals:
        return Bool(bool(clo.value))
    else:
        raise KitsuException("Convertion to boolean failed.")

def to_nat(clo):
    """ Convert a logical literal to a natural number. """
    if clo.typeof() in logic_literals:
        return Nat(max(math.floor(clo.value), 0))
    else:
        raise KitsuException("Convertion to natural number failed.")

def to_int(clo):
    """ Convert a logical literal to an integer. """
    if clo.typeof() in logic_literals:
        return Int(math.floor(clo.value))
    else:
        raise KitsuException("Convertion to integer failed.")

def to_rat(clo):
    """ Convert a logical literal to a rational number. """
    if clo.typeof() in logic_literals:
        return Rat(Fraction(clo.value))
    else:
        raise KitsuException("Convertion to rational number failed.")

def to_byte(clo):
    """ Convert a logical literal to a byte. """
    if clo.typeof() in logic_literals:
        return Byte(math.floor(clo.value) % 256)
    else:
        raise KitsuException("Convertion to byte failed.")

def to_char(clo):
    """ assert literal is a character """
    if clo.typeof() == kitchar:
        return clo
    else:
        raise KitsuException("Convertion to character failed.")

def to_getter(clo):
    """ Convert an accessor to an accessor. """
    if clo.typeof() in accessor_types:
        return Get(clo.propname)
    else:
        raise KitsuException("Convertion to accessor failed.")

def to_checker(clo):
    """ Convert a accessor to a accessor. """
    if clo.typeof() in accessor_types:
        return Has(clo.propname)
    else:
        raise KitsuException("Convertion to checker failed.")

# TODO: Define this through a link to prelude
def wrap_as_maybe(x):
    """ Wrap a value in a maybe. """
    return x

# TODO: Define this through a link to prelude
def make_kit_tuple(*args):
    """ Make a tuple. """
    return tuple(args)
