# Строим доверительный интервал для доли, normal approximation interval
# ответ (0.2747415, 0.5824014)
n = 28
alpha = 0.1
w = 12/28
ans = w + qnorm(c(alpha/2, 1-alpha/2)) * sqrt(w*(1-w))/sqrt(n)
print(ans)