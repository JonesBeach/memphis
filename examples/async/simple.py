import asyncio

async def simple_task(id, n):
    for i in range(n):
        print("Task", id, "Step", i + 1)
        await asyncio.sleep(0.05)

async def main():
    task_1 = asyncio.create_task(simple_task(1, 5))
    task_2 = asyncio.create_task(simple_task(2, 5))

    await task_1
    await task_2

asyncio.run(main())
