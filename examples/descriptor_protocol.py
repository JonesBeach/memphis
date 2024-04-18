class MyNonDataDescriptor:
    def __get__(self, instance, _):
        return 4 * instance.val

    # TODO test a data descriptor here
    #def __set__(self, instance, value):
    #    ...

    #def __delete__(self, instance):
    #    ...

class MyClass:
    non_data = MyNonDataDescriptor()

    def __init__(self):
        self.val = 11

a = MyClass()
print(a.non_data)

# Because this is a non-data descriptor, this will remove the descriptor entirely.
a.non_data = 33
print(a.non_data)
#
# del a.attribute
# print(a.attribute)
# del a.attribute
# print(a.attribute)
