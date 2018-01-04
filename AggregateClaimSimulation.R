
# Collective Risk Model: without coverage modifications
# S = Y_1 + \cdots + Y_N
# Assume N ~ Poison(lambda=2) and Y ~ Weibull(alpha=3,theta=5000)

# set parameters
lambda = 2
alpha = 3
theta = 5000
# show frequency and severity distributions
par(mfrow=c(1,2))
n <- 1:10
fn <- dpois(1:10,lambda)
plot(n,fn,ylim=c(0,0.3),main="Frequency: Poisson")
abline(h=0,lty=2)
y <- seq(1,25000,1)
fy <- alpha*theta^alpha/(y+theta)^(alpha+1)
plot(y,fy,type="l",main="Severity: Pareto")


# set sample size for the simulation
set.seed(123)
size = 5000
S <- rep(NA,size)
N <- rpois(size,lambda)
for (i in 1:size){
  uu <- runif(N[i])
  Y <- theta*((1-uu)^(-1/alpha)-1)
  S[i] <- sum(Y)
}

# show distribution of aggregate loss S
par(mfrow=c(1,2))
hist(S,freq=F,breaks=100)
plot(ecdf(S),xlab="S")

# applications
# 1) find descriptive statistics
mean(S)                             # sample mean
sd(S)                               # sample standard deviation
quantile(S,prob=c(0.05,0.5,0.95))   # percentiles

# 2) calculate cdf function
sum((S==0))/size                    # Pr(S=0)
sum(S<=mean(S))/size                # Pr(S<=E(S))
sum(S>mean(S))/size                 # Pr(S>E(S))

# 3) calculate risk measures
VaR = quantile(S,prob=0.99)         # significance level = 0.01
CTE = sum(S*(S>VaR))/sum((S>VaR))
rm = c(VaR,CTE)
names(rm) = c("VaR","CTE")
print(rm)

# 4) pricing stop-loss insurance
# set deductible
par(mfrow=c(1,1))
d = seq(0,120000,1000)
price = rep(NA,length(d))
for (i in 1:length(d)){
price[i] = sum((S-d[i])*(S>d[i]))/size
}
plot(d,price,xlab="Deductible",ylab="Stop-Loss Premium",type="b")



