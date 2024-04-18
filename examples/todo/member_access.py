def print_err(a):
    try:
        print(a())
    except AttributeError as e:
        print(type(e))
    except Exception as e:
        print(e)

class Foo:
    class_attr = 6

    def __init__(self):
        self.attr = 4

    def func(self):
        return self.attr

foo = Foo()
print(foo.attr)
print(foo.func)
print_err(lambda: Foo.attr)
print(Foo.func)
print(foo.class_attr)
print(Foo.class_attr)

#print(type([].__doc__))
print([].append)
#print(type(list.__doc__))
print(list.append)

print(type(list.__dict__))
print(type(type.__dict__))
