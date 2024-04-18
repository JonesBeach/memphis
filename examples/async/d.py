import asyncio

from tasks import task1, task2

async def main(**kwargs):
    task_1 = task1(kwargs['count'])
    task_2 = task2(kwargs['count'])

    print(await task_1)
    print(await task_2)

    return "Main Completed"

args = {'count': 5}
print(asyncio.run(main(**args)))
