#!/usr/local/bin/python
# coding=utf-8
from matplotlib.pyplot import *
import numpy as np
from numpy.ma import sqrt
from scipy.integrate import quad
from scipy.stats import beta

#--------- SETUP
f = lambda x: np.sin(x) ** (-2.0 / 3)
M = 50 # число выборок для оценки
N = 20 # число точек
SAMPLES_SIZES = (1.8)**np.arange(1, N + 1)
ACTUAL_VALUE = quad(f, 0, 1)[0]
#---------- END SETUP

def RMSE(actual, predicted):
    return sqrt(np.mean((actual - predicted) ** 2))

res = [np.zeros(M), np.zeros(M), np.zeros(M)]
res_rmse = [np.zeros(N), np.zeros(N), np.zeros(N)]

for k, sample_size in enumerate(SAMPLES_SIZES):
    for test in range(M):
        rv = [beta(1.0, 1.0), beta(1.0 / 3, 1.0), beta(0.5, 0.5)]
        sample = [[], [], []]
        for stat in range(3):
            sample[stat] = rv[stat].rvs(size=sample_size)
            res[stat][test] = np.mean(f(sample[stat]) / rv[stat].pdf(sample[stat]))

    for stat in range(3):
        res_rmse[stat][k] = RMSE(ACTUAL_VALUE, res[stat])
    print(sample_size, np.mean(res[0]), np.mean(res[1]), np.mean(res[2]))

print res_rmse[0]
print res_rmse[1]
print res_rmse[2]

ARE_1_2 = (res_rmse[2][-1] / res_rmse[1][-1]) ** 2

print ARE_1_2

loglog(SAMPLES_SIZES, res_rmse[0], label="B(1, 1)")
loglog(SAMPLES_SIZES, res_rmse[1], label="B(1/3, 1)")
loglog(SAMPLES_SIZES, res_rmse[2], label="B(1/2, 1/2)")
grid(True)
legend(loc="lower left")
show()