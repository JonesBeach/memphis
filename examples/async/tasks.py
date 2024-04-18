import asyncio

async def task1(n):
    i = 0
    while i < n:
        if i == 2:
            print("Iteration 2: Task 1 - Step", i + 1)
            await asyncio.sleep(0.03)
            print("END Iteration 2: Task 1")
        else:
            print("Task 1 - Step", i + 1)
            await asyncio.sleep(0.01)
            print("End Task 1")
        i += 1

    return "Task 1 Completed"

async def task2(n):
    i = 0
    while i < n:
        print("Task 2 - Step", i + 1)
        await asyncio.sleep(0.01)
        i += 1

    return "Task 2 Completed"
