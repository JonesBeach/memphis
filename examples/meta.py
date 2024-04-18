# This test is supported, but I'm not testing it as part of integration tests
# because there are some internal properties as part of the namespace that
# we do not yet support.
# tldr the test works but the stdout doesn't match exactly
class InterfaceMeta(type):
    def __new__(mcls, name, bases, namespace, **kwargs):
        print('------------')
        print('mcls', mcls)
        print('name', name)
        print('bases', bases)
        print('namespace', namespace)
        new_cls = super().__new__(mcls, name, bases, namespace)
        return new_cls

    def run(cls):
        return 5

class BaseInterface(metaclass=InterfaceMeta):
    pass

class ConcreteImplementation(BaseInterface):
    pass

class IncompleteImplementation(BaseInterface):
    pass

a = ConcreteImplementation.run()
print(a)
