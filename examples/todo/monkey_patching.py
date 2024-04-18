class MyClass:
    pass

def my_method(self):
    return "Hello from the method"

# Binding a method to an instance at runtime
instance = MyClass()
import types
instance.my_method = types.MethodType(my_method, instance)
print(instance.my_method())  # Now 'my_method' is bound to 'instance'
