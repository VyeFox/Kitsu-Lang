from threading import Thread

# TODO: link this to the prelude
def wrap_as_maybe(obj):
    return obj

class Closure:

    # constructor

    def __init__(self, ctype, state, func):
        self.ctype = ctype
        self.state = state
        self.func = func

    # define closure as action

    def __call__(self, arg):
        return self.func(self.state, arg)

    # define access to state

    def __getitem__(self, key):
        return self.state[key]

    def __contains__(self, key):
        return key in self.state

    # define access to type

    def typeof(self):
        return self.ctype

# stateless primitives

class Async:

    # constructor

    def __init__(self, func):
        self.ret = None
        def callback(result):
            self.ret = result
        self.proc = Thread(target=func, args=(callback,))
        self.proc.start()

    # define closure as action

    def __call__(self, arg):
        self.proc.join()
        return self.ret

    # define access to state

    def __getitem__(self, key):
        raise KeyError("Async has no state")

    def __contains__(self, key):
        return False

class Lazy:

    # constructor

    def __init__(self, func):
        self.ret = None
        self.func = func

    # define closure as action

    def __call__(self, arg):
        if self.ret is None:
            def callback(result):
                self.ret = result
            self.func(callback)
        return self.ret

    # define access to state

    def __getitem__(self, key):
        raise KeyError("Lazy has no state")

    def __contains__(self, key):
        return False


class Atomic:

    # constructor

    def __init__(self, value):
        self.value = value

    # define closure as action
    # TODO: impliment deadlock resolution
    def __call__(self, update):
        self.value = update(self.value)
        return wrap_as_maybe(self.value)

    # define access to state

    def __getitem__(self, key):
        raise KeyError("Atomic has no state")

    def __contains__(self, key):
        return False    




