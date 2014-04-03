import numpy as np
from numpy.ma import sqrt
from numpy.random.mtrand import uniform

THETA = 42
SERIES_SIZE = 1000
MIN_SAMPLE_SIZE = 1
MAX_SAMPLE_SIZE = 500


def MSE(statistic, param=THETA):
    return sqrt(np.mean((statistic - param) ** 2))

print("".ljust(28) + "{0:20} {1:16} {2:20}".format("2*[MEAN]", "[MAX]", "[N+1 / N * MAX]"))
for sample_size in range(MIN_SAMPLE_SIZE, MAX_SAMPLE_SIZE, 10):
    stat1 = stat2 = stat3 = np.empty((1, 0), dtype=np.float64)
    for test in range(SERIES_SIZE):
        sample = uniform(high=THETA, size=sample_size)

        stat1 = np.append(stat1, 2 * sample.mean())
        stat2 = np.append(stat2, sample.max())
        stat3 = np.append(stat3, (sample_size + 1.0) / sample_size * sample.max())

    printFormat = 'SampleSize = {0:3} {1:20.8f} {2:20.8f} {3:20.8f}'
    print(printFormat.format(sample_size, MSE(stat1), MSE(stat2), MSE(stat3), end=""))