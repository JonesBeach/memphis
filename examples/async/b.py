import asyncio

from tasks import task1, task2

async def main():
    task_1 = asyncio.create_task(task1(5))
    task_2 = asyncio.create_task(task2(5))

    result_1 = await task_1
    print(result_1)
    await task_2

asyncio.run(main())
