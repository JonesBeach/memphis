# This class shares a single instance under the hood and the __init__ method will only run
# for the first time.
class SingletonA:
    _instance = None
    _initialized = False

    def __new__(cls, *args, **kwargs):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self, data):
        if not self._initialized:
            self.data = data
            self._initialized = True

# This class shares a single instance under the hood, but the __init__ method will run for
# each instance.
class SingletonB:
    _instance = None

    def __new__(cls, *args, **kwargs):
        if cls._instance is None:
            cls._instance = super().__new__(cls)
        return cls._instance

    def __init__(self, data):
        self.data = data

singleton1 = SingletonA("First")
singleton2 = SingletonA("Second")

print(singleton1.data)  # Output: First
print(singleton2.data)  # Output: First
print(singleton1 is singleton2)  # Output: True

singleton1 = SingletonB("First")
singleton2 = SingletonB("Second")

print(singleton1.data)  # Output: Second
print(singleton2.data)  # Output: Second
print(singleton1 is singleton2)  # Output: True
