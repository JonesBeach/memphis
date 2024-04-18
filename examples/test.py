print("Hello, World!")

def add(a, b):
    return a + b

print(add(add(4, 5), add(4*4, 5+5)))

x = False
print(4 >= 4)
print(x or True)
print(True and x)

y = 6
if y > 5:
    print("y is greater than 5")

z = 0
while z <= 10:
    print(z)
    z = z + 1

class Person:
    def __init__(self, name):
        self.name = name
        self.age = 0

    def greet(self):
        print("Hello, my name is:", self.name)

p = Person("John")
p.greet()

import other
# import it a second time to visual confirm module caching is working
# TODO find a better way to test this
import other
other.something()

# Things to test on iterables:
# 1) literal
# 2) builtin
# 3) builtin from different type
# 4) index access
# 5) for-in loop
# 6) type builtin
# 7) equality
# 8) list/set comprehension
print([1,2.1])
print(list([2,3,4,5]))
a = [1,2,3]
print(a)
print(a[0])
for i in a:
    print(i)
print(list(range(5,10)))

# Sets
b = {1,2}
print(b)
b = set({2,3})
for i in b:
    print(i)

# Tuples
c = (1, 2)
print(c)
for i in c:
    print(i)
print(c[0])
print(tuple(range(4)))

# Ranges
d = range(5)
print(d)
e = range(4, 10)
print(e)
f = range(2,12,2)
print(f)

try:
    print(4/0)
except:
    print("Caught an error")
finally:
    print("After the error")

def test_kwargs(**kwargs):
    print(kwargs['a'])
    print(kwargs['b'])

test_kwargs(a=1, b=2)
test_kwargs(**{'a': 1, 'b': 2})
args = {'a': 1, 'b': 2}
test_kwargs(**args)

for k, v in args.items():
    print(k, v)

print(slice(2))
print(slice(2,3))
print(slice(2,3,4))

print(complex())
print(complex(0, 1))
print(complex(1, 1))
print(complex(1.1, 1.1))
