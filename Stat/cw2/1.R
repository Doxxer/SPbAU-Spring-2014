# Строим доверительный интервал для среднего по известной дисперсии
# ответ (122.8719, 133.1281)
len = 75
alpha = 0.1
mean = 128
sigma = 27
ans = qnorm(c(alpha/2, 1-alpha/2), mean = mean, sd = sigma / sqrt(len))
print(ans)