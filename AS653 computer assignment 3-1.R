# enter Y
Y <- rep(0, 50)
Y[19:35] <- 1
Y[36:43] <- 2
Y[44:47] <- 3
Y[48:49] <- 4
Y[50] <- 5
# get n
n = length(Y)
# get Ybar
Ybar = mean(Y)
# get T
T = var(Y)

# 1.
# set bootstrap samples
nboot = 800
tmpdata1 <- sample(Y, n * nboot, TRUE)
bootstrap1 <- matrix(tmpdata1, n, nboot)

# (a)
# get Tstars and sum of (Tstar[i] - T) ^ 2
Tstar1 <- rep(0, nboot)
sum1 = 0
for (i in 1 : nboot) {
  Tstar1[i] = var(bootstrap1[(n * (i - 1) + 1) : (n * i)])
  sum1 = sum1 + (Tstar1[i] - T) ^ 2
}
# get MSE
MSE1 = sum1 / nboot
print(MSE1)

# (b)
# 95% CI
lb1 = quantile(Tstar1 - T, prob = 0.025)
ub1 = quantile(Tstar1 - T, prob = 0.975)
CI1 <- T - c(ub1, lb1)
print(CI1)

# 2.
# set bootstrap samples
N <- rpois(n, Ybar)
tmpdata2 <- rpois(n * nboot, mean(N))
bootstrap2 <- matrix(tmpdata2, n, nboot)
That = var(N)

# (a)
# get Tstars and sum of (Tstar[i] - T) ^ 2
Tstar2 <- rep(0, nboot)
sum2 = 0
for (i in 1 : nboot) {
  Tstar2[i] = var(bootstrap2[(n * (i - 1) + 1) : (n * i)])
  sum2 = sum2 + (Tstar2[i] - That) ^ 2
}
# get MSE
MSE2 = sum2 / nboot
print(MSE2)

# (b)
# 95% CI
lb2 = quantile(Tstar2 - That, prob = 0.025)
ub2 = quantile(Tstar2 - That, prob = 0.975)
CI2 <- That - c(ub2, lb2)
print(CI2)