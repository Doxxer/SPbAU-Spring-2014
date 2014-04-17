#!/usr/local/bin/python
# coding=utf-8

from scipy.stats import binom, beta, norm
import numpy as np

# #--------- SETUP
M = 1000
N = 10 ** np.arange(1, 5)
alpha = 0.05
z = norm.ppf(1 - 0.5 * alpha)
#---------- END SETUP

def clopper_pearson(x, n):
    right = beta.ppf(1 - alpha / 2, x + 1, n - x)
    left = beta.ppf(alpha / 2, x, n - x + 1)
    return right - left, (left, right)


def arcsine_transformation(x, n):
    p = (x + 0.0) / n
    left = np.sin(np.arcsin(np.sqrt(p)) - z / (2 * np.sqrt(n))) ** 2
    right = np.sin(np.arcsin(np.sqrt(p)) + z / (2 * np.sqrt(n))) ** 2
    return right - left, (left, right)


def wilson(x, n):
    a = 1 / (1 + 1 / n * z ** 2)
    p = (x + 0.0) / n
    q = z * np.sqrt(p * (1 - p) / n + z ** 2 / (4 * n ** 2))
    left = a * (p + z ** 2 / (2 * n) - q)
    right = a * (p + z ** 2 / (2 * n) + q)
    return right - left, (left, right)


def normal_approximation(x, n):
    p = (x + 0.0) / n
    right = p + np.sqrt(p * (1 - p) / n)
    left = p - np.sqrt(p * (1 - p) / n)
    return right - left, (left, right)


def solve(n, p):
    res = np.zeros((4, M))
    emp = np.zeros((4, M))
    for k, x in enumerate(binom.rvs(n, p, size=M).tolist()):
        cp = clopper_pearson(x, n)
        at = arcsine_transformation(x, n)
        wl = wilson(x, n)
        nr = normal_approximation(x, n)

        res[0][k], emp[0][k] = cp[0], cp[1][0] < p < cp[1][1]
        res[1][k], emp[1][k] = at[0], at[1][0] < p < at[1][1]
        res[2][k], emp[2][k] = wl[0], wl[1][0] < p < wl[1][1]
        res[3][k], emp[3][k] = nr[0], nr[1][0] < p < nr[1][1]

    print np.mean(res, axis=1), np.var(res, axis=1), np.mean(emp, axis=1)


if __name__ == "__main__":
    for p in [0.5, 0.75, 0.99]:
        print p
        for n in N:
            solve(n, p)