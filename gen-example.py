import random

with open("big-example.txt", "w+") as f:
    n = random.randrange(0, 10000)
    f.write(f"{n}")
    depth = 0

    ops = "+-*/"
    for i in range(0, 1000000):
        op = random.randrange(0, 4)
        f.write(ops[op])
        if random.randrange(0, 10) < 2:
            if random.randrange(0, 1) == 0:
                f.write('(')
                depth += 1
        n = random.randrange(0, 10000)
        f.write(f"{n}")
        if random.randrange(0, 2) == 0 and depth > 0:
            f.write(')')
            depth -= 1


    for i in range(depth):
        f.write(')')
