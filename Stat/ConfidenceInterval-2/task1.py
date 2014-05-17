#!/usr/local/bin/python
# coding=utf-8

import numpy as np
from numpy.ma import mean, std, sqrt
from scipy.stats import norm, t
from scipy.stats.mstats import mquantiles

# --------- SETUP
M = 5000
N = 10 ** np.arange(1, 5)
alpha = 0.05
# ---------- END SETUP

def RMSE(actual, predicted):
    return sqrt(np.mean((actual - predicted) ** 2)) / sqrt(actual.size)


def ci_non_parametric(x, p):
    return mquantiles(x, prob=[(1 - p) / 2, (1 + p) / 2], alphap=1, betap=1)


def ci_parametric(x, p):
    return norm.ppf(q=[(1 - p) / 2, (1 + p) / 2], loc=mean(x), scale=std(x, ddof=1))


def solve(rv, N, p, rv_name):
    ci_par, ci_non_par = np.zeros((2, M)).tolist(), np.zeros((2, M)).tolist()
    width_par, width_non_par = np.zeros(M).tolist(), np.zeros(M).tolist()

    for step in range(M):
        x = rv.rvs(size=N)
        ci_par[0][step], ci_par[1][step] = ci_parametric(x, p)
        ci_non_par[0][step], ci_non_par[1][step] = ci_non_parametric(x, p)
        width_par[step] = ci_par[1][step] - ci_par[0][step]
        width_non_par[step] = ci_non_par[1][step] - ci_non_par[0][step]

    width_par, width_non_par = np.array(width_par), np.array(width_non_par)
    ci_par, ci_non_par = np.array(ci_par), np.array(ci_non_par)

    meanwidth_par, meanwidth_non_par = mean(width_par), mean(width_non_par)
    sd_meanwidth_par = RMSE(width_par, norm.ppf(q=(1 + p) / 2) - norm.ppf(q=(1 - p) / 2))
    sd_meanwidth_non_par = RMSE(width_non_par, norm.ppf(q=(1 + p) / 2) - norm.ppf(q=(1 - p) / 2))

    emp_cl_par = mean(rv.cdf(ci_par[1]) - rv.cdf(ci_par[0]))
    emp_cl_non_par = mean(rv.cdf(ci_non_par[1]) - rv.cdf(ci_non_par[0]))
    sd_emp_cl_par, sd_emp_cl_non_par = RMSE(emp_cl_par, p), RMSE(emp_cl_non_par, p)

    print("{0} (N = {1:5}); quantile={2}; Non-parametric: width = {3} ({4}); empirical CL = {5} ({6})".format(
        rv_name, N, p, meanwidth_non_par, sd_meanwidth_non_par, emp_cl_non_par, sd_emp_cl_non_par))
    print("{0} (N = {1:5}); quantile={2}; Parametric    : width = {3} ({4}); empirical CL = {5} ({6})".format(
        rv_name, N, p, meanwidth_par, sd_meanwidth_par, emp_cl_par, sd_emp_cl_par))


if __name__ == "__main__":
    p = 0.99
    for n in N:
        solve(norm(), n, p,  "norm")
    print('---------')
    for n in N:
        solve(t([5]), n, p, "t df=5")
    print('---------')
    for n in N:
        solve(t([20]), n, p, "t df=20")