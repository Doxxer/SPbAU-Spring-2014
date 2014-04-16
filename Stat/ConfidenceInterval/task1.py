#!/usr/local/bin/python
# coding=utf-8

from matplotlib.pyplot import plot, axhline, grid, legend, show
import numpy as np
from scipy.integrate import quad
from scipy.stats import beta, norm

# #--------- SETUP
f = lambda x: np.sin(x) ** (-2.0 / 3)  # подынтегральная функция
ACTUAL_VALUE = quad(f, 0, 1)[0]  # реальное значение интеграла
N = 10 ** 7  # число оценок
lDataStrip = 5  # какую часть усекать чтобы избежать разлетания графика
xPlot = list(range(N))[N / lDataStrip:]

# ------ Setup Statistics
# Реальная дисперсия оценки Be(1/3,1)
pdf1 = lambda x: beta.pdf(x, 1.0 / 3, 1)
var1 = quad(lambda x: f(x) ** 2 / pdf1(x), 0, 1)[0] - ACTUAL_VALUE ** 2

# Реальная дисперсия оценки Be(1/2,1/2)
pdf2 = lambda x: beta.pdf(x, 0.5, 0.5)
var2 = quad(lambda x: f(x) ** 2 / pdf2(x), 0, 1)[0] - ACTUAL_VALUE ** 2
xGamma = norm.ppf(0.95)
#---------- END SETUP

def generate_samples():
    rv1 = beta(1.0 / 3, 1.0)
    rv2 = beta(0.5, 0.5)

    sample1 = rv1.rvs(size=N)
    np.save('rv1_sample', sample1)
    np.save('rv1_pdf', rv1.pdf(sample1))

    sample2 = rv2.rvs(size=N)
    np.save('rv2_sample', sample2)
    np.save('rv2_pdf', rv2.pdf(sample2))


def cumulative_mean(a):
    return np.cumsum(a) / np.arange(1, np.size(a) + 1, dtype=float)


def cumulative_variance(a):
    return cumulative_mean(a ** 2) - cumulative_mean(a) ** 2


def calc_ci(a, var=None):
    index = np.arange(1, np.size(a) + 1, dtype=float) ** 0.5
    if var:
        variance = np.zeros(np.size(a)) + var
    else:
        variance = cumulative_variance(a)

    cummeanData = cumulative_mean(a)
    q = xGamma / index * np.sqrt(variance)

    return cummeanData - q, cummeanData + q


def solve(sample, pdf, label, var):
    result = f(sample) / pdf

    cumulativeMean = cumulative_mean(result)
    size = np.size(cumulativeMean)

    ci1, ci2 = calc_ci(result)
    known_var_CI1, known_var_CI2 = calc_ci(result, var)

    cumulativeMean = cumulativeMean[size / lDataStrip:]
    ci1 = ci1[size / lDataStrip:]
    ci2 = ci2[size / lDataStrip:]
    known_var_CI1 = known_var_CI1[size / lDataStrip:]
    known_var_CI2 = known_var_CI2[size / lDataStrip:]

    print "start plotting..."

    plot(xPlot, cumulativeMean, label=label)
    plot(xPlot, ci1, label="cumulative-variance CI", color='g')
    plot(xPlot, ci2, label="cumulative-variance CI", color='g')

    plot(xPlot, known_var_CI1, label="known-variance CI", c='m')
    plot(xPlot, known_var_CI2, label="known-variance CI", c='m')
    axhline(y=ACTUAL_VALUE, label="Actual value", c='r')

    #ylim(3.03, 3.07)
    #ylim(np.min(cumulativeMean), np.max(cumulativeMean))

    grid(True)
    legend(loc="lower right")
    show()


if __name__ == "__main__":
    # generate_samples()

    #solve(sample=(np.load('rv1_sample.npy')), pdf=(np.load('rv1_pdf.npy')), label="Be(1/3, 1)", var=var1)
    solve(sample=(np.load('rv2_sample.npy')), pdf=(np.load('rv2_pdf.npy')), label="Be(1/2, 1/2)", var=var2)