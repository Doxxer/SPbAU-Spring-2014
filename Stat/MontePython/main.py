import random
import math

N = 1000000

def integral(f, a, b):
    return (b - a) / N * sum(f(random.uniform(a, b)) for _ in range(0, N))

if __name__ == "__main__":
    function = lambda x: math.sin(x + math.exp(-x))
    print(integral(function, 0, 1))
