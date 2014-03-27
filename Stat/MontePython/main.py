import random
import math

N = 2000000


def integral(f, a, b):
    return (b - a) / N * sum(f(random.uniform(a, b)) for _ in range(0, N))


if __name__ == "__main__":
    f1 = lambda x: math.sin(x + math.exp(-x))
    f3 = lambda x: math.pow(math.sin(x), -2 / 3.0)

    f2_1 = lambda x: math.sin(x) * math.exp(-x * x)
    f2_2 = lambda x: 1 / (x * x) * math.sin(1 / x) * math.exp(-1 / (x * x))

    print("task1 at [0..1] = ", integral(f1, 0, 1))
    print("task2 at [0..infinity] = ", integral(f2_1, 0, 1) + integral(f2_2, 0, 1))
    print("task3 at [0..1] = ", integral(f3, 0, 1))
