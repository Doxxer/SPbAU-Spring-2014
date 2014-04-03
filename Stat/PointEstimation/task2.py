import numpy as np
from numpy.ma import sqrt
from numpy.random.mtrand import standard_cauchy

THETA = 42
SERIES_SIZE = 500
MIN_SAMPLE_SIZE = 20
MAX_SAMPLE_SIZE = 800


def MSE(statistic, knownParameter=THETA):
    return sqrt(np.mean((statistic - knownParameter) ** 2))


def TrimmedMean(sample, slice=0.05):
    lim = round(sample.size * slice)
    return np.mean(np.sort(sample)[lim:-lim])


print("".ljust(28) + "{0:20} {1:16} {2:22} {3:20}".format("[MEAN]", "[MEDIAN]", "MEAN after slice", "Trimmed mean"))
for sample_size in range(MIN_SAMPLE_SIZE, MAX_SAMPLE_SIZE, 10):
    stat1 = stat2 = stat3 = stat4 = np.empty((1, 0), dtype=np.float64)
    for _ in range(SERIES_SIZE):
        sample = standard_cauchy(size=sample_size) + THETA

        stat1 = np.append(stat1, np.mean(sample))
        stat2 = np.append(stat2, np.median(sample))
        stat3 = np.append(stat3, np.mean(np.sort(sample)[1:-1]))
        stat4 = np.append(stat4, TrimmedMean(sample))

    printFormat = 'SampleSize = {0:3} {1:20.8f} {2:20.8f} {3:20.8f} {4:20.8f}'
    print(printFormat.format(sample_size, MSE(stat1), MSE(stat2), MSE(stat3), MSE(stat4), end=""))