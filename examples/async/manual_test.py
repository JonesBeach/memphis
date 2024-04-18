import asyncio

async def task1(n):
    i = 0
    while i < n:
        if i == 2:
            print("TWO Task 1 - Step", i + 1)
            await asyncio.sleep(3)
            print("two")
        else:
            print("Task 1 - Step", i + 1)
            await asyncio.sleep(1)
            print("one")
        i += 1
    return True

async def task2(n):
    i = 0
    while i < n:
        print("Task 2 - Step", i + 1)
        await asyncio.sleep(1)
        i += 1
    return True

async def main():
    task_1 = asyncio.create_task(task1(5))
    task_2 = asyncio.create_task(task2(5))

    await task_1
    await task_2

asyncio.run(main())
