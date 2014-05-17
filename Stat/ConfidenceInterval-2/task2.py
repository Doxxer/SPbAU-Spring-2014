#!/usr/local/bin/python
# coding=utf-8

import numpy as np
from numpy.ma import median, mean, std
from scipy.stats import cauchy, norm
from scipy.stats.mstats_basic import mquantiles

# --------- SETUP
a = 0
s = 1
M = 10000
N = 10 ** np.arange(1, 5)
gamma = 0.95
# ---------- END SETUP

def ci_non_parametric(x, p):
    return mquantiles(x, prob=[(1 - p) / 2, (1 + p) / 2], alphap=1, betap=1)


def ci_parametric(x, p):
    return norm.ppf(q=[(1 - p) / 2, (1 + p) / 2], loc=mean(x), scale=std(x, ddof=1))


def mad(arr):
    return median(np.abs(arr - median(arr)))


def a_ci(x, med, mad):
    return med + x[0] * mad, med + x[1] * mad


def s_ci(x, mad):
    return x[0] * mad, x[1] * mad


def test_ci(ci_non_par, ci_par, real_value):
    empirical_cl_par = mean((ci_par[0] < real_value) * (real_value < ci_par[1]))
    empirical_cl_non_par = mean((ci_non_par[0] < real_value) * (real_value < ci_non_par[1]))
    width_par, width_non_par = ci_par[1] - ci_par[0], ci_non_par[1] - ci_non_par[0]
    sd_width_par, sd_width_non_par, = std(width_par, ddof=1) / np.sqrt(M), std(width_non_par, ddof=1) / np.sqrt(M)
    return empirical_cl_non_par, empirical_cl_par, sd_width_non_par, sd_width_par, width_non_par, width_par


def solve(n):
    x_cauchy = cauchy.rvs(size=n * M, loc=a, scale=s).reshape(M, n)
    x_median = np.apply_along_axis(np.median, 1, x_cauchy)
    x_mad = np.apply_along_axis(mad, 1, x_cauchy)
    mu = -x_median / x_mad
    ka = 1 / x_mad

    ci_par = a_ci(ci_parametric(mu, gamma), x_median, x_mad)
    ci_non_par = a_ci(ci_non_parametric(mu, gamma), x_median, x_mad)

    x_med = mean(x_median)
    empirical_cl_non_par, empirical_cl_par, sd_width_non_par, sd_width_par, width_non_par, width_par = \
        test_ci(ci_non_par, ci_par, x_med)

    print("LOCATION | N = {0:5}; Non-parametric: width = {1} ({2}); empirical CL = {3}".format(
        n, mean(width_non_par), sd_width_non_par, empirical_cl_non_par))
    print("LOCATION | N = {0:5};     Parametric: width = {1} ({2}); empirical CL = {3}".format(
        n, mean(width_par), sd_width_par, empirical_cl_par))

    # ---------------------------------

    ci_par = s_ci(ci_parametric(ka, gamma), x_mad)
    ci_non_par = s_ci(ci_non_parametric(ka, gamma), x_mad)

    mad_ = np.exp(mean(np.log(np.abs(x_cauchy - x_med))))
    empirical_cl_non_par, empirical_cl_par, sd_width_non_par, sd_width_par, width_non_par, width_par = \
        test_ci(ci_non_par, ci_par, mad_)

    print("SCALE    | N = {0:5}; Non-parametric: width = {1} ({2}); empirical CL = {3}".format(
        n, mean(width_non_par), sd_width_non_par, empirical_cl_non_par))
    print("SCALE    | N = {0:5};     Parametric: width = {1} ({2}); empirical CL = {3}".format(
        n, mean(width_par), sd_width_par, empirical_cl_par))
    print
    print


if __name__ == "__main__":
    for n in N:
        solve(n)