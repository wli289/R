# set parameters
lambda = 5
theta = 1000
set.seed(123)
size = 1000
S <- rep(NA,size)
N <- rpois(size,lambda)
Npolicy <- rep(NA, size)
for (i in 1:size){
  uu <- runif(N[i])
  Y <- -theta*log((1-uu))
  S[i] <- sum(Y)
  Npolicy[i] <- sum(Y > 500)
}

#1.(a)
# show distribution of aggregate loss S
par(mfrow=c(1,2))
hist(S,freq=F,breaks=100)
plot(density(S),main = "Kernel Density", xlab="S")

#1.(b)
#simulated values
mean(S)
var(S)
#true values
E <- lambda * theta
E
V <- lambda * (2*theta^2 - theta^2) + (lambda * theta^2)
V
#accuracy of mean and variance
1-((E-mean(S))/E)
1-((V-var(S))/V)
#the ture mean and true variance are close to the estimate mean and variance.
#by law of large number, increase size can make the simulation more accurate

#1.(c)
#P(S>10000) by normal distribution
1-pnorm(10000, mean = mean(S), sd = (var(S))^0.5, lower.tail = TRUE, log.p = FALSE) 
#P(S>10000) by simulation
count = 0
for (i in 1:size){
  if (S[i] > 10000){
    count = count +1
  }
}
count/size
#normal approximation is better because the size of simulation is 1000, which is too small

#1.(d)
VaR = quantile(S,prob=0.95)         # significance level = 0.05
CTE = sum(S*(S>VaR))/sum((S>VaR))
rm = c(VaR,CTE)
names(rm) = c("VaR","CTE")
print(rm)


#2.(a)(i)
Nloss <- N
table(Nloss)
table(Npolicy)
plot(ecdf(Nloss), main = "empirical CDF of Nloss", xlab = "number of claims")
plot(ecdf(Npolicy), main = "empirical CDF of Nloss", xlab = "number of claims")
#the Numbers per policy is light tail than Numbers per loss

#2.(a)(ii)
lambda.new = lambda*exp(-500/theta)
lambda.new
n = 1000
tbl = cbind(n*(dpois(0,lambda.new)),
              n*(dpois(1,lambda.new)),
              n*(dpois(2,lambda.new)),
              n*(dpois(3,lambda.new)),
              n*(dpois(4,lambda.new)),
              n*(dpois(5,lambda.new)),
              n*(dpois(6,lambda.new)),
              n*(dpois(7,lambda.new)),
              n*(1-ppois(7,lambda.new)))

#  PEARSON GOODNESS-OF-FIT STATISTIC
actual = data.frame(table(Npolicy))[,2];
length(actual) = 9
diff = actual-tbl
(PearsonG = sum(diff*diff/tbl))
cbind(tbl,PearsonG)
#  p-value
1-pchisq(PearsonG, df=9-1)

#2.(b)(i)
YL <- pmax(0, (rexp(size, 1/theta) - 500)*0.8)
YP <- YL[YL>0]
plot(ecdf(YL), xlab = "Yloss")
plot(ecdf(YP), xlab = "Ypayment")

YPAY<-rexp(size,1/(.8*theta)) #memoryless property


mean(YPAY)
#Ypayment has less number of claims and it must be larger than zero

#2.(b)(ii)
library(goftest)
#Kolmogorov-Smirnov # the test statistic is "D"
ks.test(YP, "pexp",rate=1/(0.8*theta))

#3.
set.seed(123)
s.stop.loss <- rep(NA,size)
n.stop.loss <- rpois(size,lambda)
for (i in 1:size){
  u <- runif(n.stop.loss[i])
  X <- -theta*log(1-u)
  Xsl<- X-500
  Xsl[Xsl>2500] <- 2500
  Xsl[Xsl<0]<-0
  s.stop.loss[i] <-sum(Xsl)
}
s.stop.loss<-s.stop.loss-2500
s.stop.loss[s.stop.loss<0]<-0
mean(s.stop.loss)

