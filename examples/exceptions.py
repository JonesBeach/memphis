try:
    # Attempting to open a file that does not exist, which should raise an IOError
    #file = open('examples/test.py', 'r')

    # If the file existed, the following line would try to divide by zero, which would raise a ZeroDivisionError
    result = 10 / 0
except IOError:
    print("An IOError occurred. File not found!")
except ZeroDivisionError:
    print("A ZeroDivisionError occurred. Cannot divide by zero!")
except Exception as e:
    # This will catch any other exceptions that are not caught by the specific except blocks above.
    print(f"An unexpected error occurred: {e}")
else:
    # This block will execute only if no exceptions are raised in the try block.
    print("Operation successful.")
finally:
    # This block will execute no matter what, even if exceptions are raised.
    print("The 'try except' block is finished.")
