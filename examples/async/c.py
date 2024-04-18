import asyncio

from tasks import task1, task2

async def main():
    asyncio.create_task(task1(4))
    asyncio.create_task(task2(4))

asyncio.run(main())
