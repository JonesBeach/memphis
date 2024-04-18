class MyContextManager:
    def __init__(self):
        self.a = 0

    def __enter__(self):
        self.a = self.a + 1
        print("Enter the context")
        return self

    def call(self):
        self.a = self.a + 1
        print("In the context")

    def __exit__(self, exc_type, exc_value, traceback):
        self.a = self.a + 1
        print("Exit the context")

with MyContextManager() as cm:
    cm.call()

print("FINAL VALUE", cm.a)
