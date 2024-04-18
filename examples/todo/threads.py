import threading
import time

def print_numbers(thread_name, count):
    for i in range(1, count + 1):
        print(f"{thread_name} prints {i}")
        time.sleep(1)

thread1 = threading.Thread(target=print_numbers, args=("Thread-1", 5,))
thread2 = threading.Thread(target=print_numbers, args=("Thread-2", 5,))

thread1.start()
thread2.start()

thread1.join()
thread2.join()

print("Both threads have finished execution.")
