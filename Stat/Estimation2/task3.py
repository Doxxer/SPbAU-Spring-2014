#!/usr/local/bin/python
# coding=utf-8
from matplotlib.pyplot import *
import numpy as np
from numpy.ma import sqrt
from scipy.stats import uniform, tmean, norm

#--------- SETUP
from scipy.stats.mstats_basic import winsorize
THETA = ACTUAL_VALUE = 5
SAMPLES_SIZES = np.arange(20, 10000, 100)
M = 200  # число выборок для оценки
N = np.size(SAMPLES_SIZES)  # число точек
rv = norm(THETA, 1)
#---------- END SETUP

def RMSE(actual, predicted):
    return sqrt(np.mean((actual - predicted) ** 2))

res_rmse = [np.zeros(N), np.zeros(N), np.zeros(N), np.zeros(N), np.zeros(N)]

for k_sampleSize, sample_size in enumerate(SAMPLES_SIZES):
    stat = [np.zeros(M), np.zeros(M), np.zeros(M), np.zeros(M), np.zeros(M)]
    for test in range(M):
        sample = rv.rvs(size=sample_size)
        perc = np.percentile(sample, [25, 75, 5, 95])
        stat[0][test] = np.mean(sample)
        stat[1][test] = np.median(sample)
        stat[2][test] = tmean(sample, (perc[0], perc[1]))
        stat[3][test] = tmean(sample, (perc[2], perc[3]))
        stat[4][test] = np.mean(winsorize(sample, (0.05, 0.05)))
    for k_stat in range(5):
        res_rmse[k_stat][k_sampleSize] = RMSE(ACTUAL_VALUE, stat[k_stat])
    print k_sampleSize, N

for i in range(N):
    print(res_rmse[0][i], res_rmse[1][i], res_rmse[2][i], res_rmse[3][i], res_rmse[4][i], )

ARE_0_1 = (res_rmse[1][-1] / res_rmse[0][-1]) ** 2

print ARE_0_1


loglog(SAMPLES_SIZES, res_rmse[0], label="mean")
loglog(SAMPLES_SIZES, res_rmse[1], label="median")
loglog(SAMPLES_SIZES, res_rmse[2], label="quartile mean [25-75]")
loglog(SAMPLES_SIZES, res_rmse[3], label="trimmed mean [5-95]")
loglog(SAMPLES_SIZES, res_rmse[4], label="winsorized mean")
grid(True)
legend(loc="lower left")
show()