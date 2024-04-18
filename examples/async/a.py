import asyncio

from tasks import task1, task2

async def main():
    task_1 = task1(5)
    task_2 = task2(5)

    print(await task_1)
    print(await task_2)

    return "Main Completed"

print(asyncio.run(main()))
