import random

with open("big-example.txt", "w+") as f:
    n = random.randrange(0, 10000)
    f.write(f"{n}")

    ops = "+-*/"
    for i in range(0, 1000000):
        op = random.randrange(0, 4)
        f.write(ops[op])
        n = random.randrange(0, 10000)
        f.write(f"{n}")
