

#This assignment is related to the simulation of aggregate loss model. Consider an automobile
#insurance policy that covers three vehicles in a household, assume that the aggregate losses from
#the household follow the model
#S = Y1 +    + YN
#where N  Poisson( = 5), Y  Exponential( = 1; 000), and N and Y are independent. Perform
#the following analysis using Monte Carlo simulation.

#1. Assume there is no deductible and policy limit:
#(a) Generate 1,000 random realizations of S. Plot the kernel density of S. Comment on the
#normality of the density.
#1a.

size <- 1000
lambda <- 5
theta <- 1000

set.seed(123)

S <- rep(NA,size)
N <- rpois(size,lambda)
for (i in 1:size){
  uu <- runif(N[i])
  Y <- -theta*(log(1-uu))
  S[i] <- sum(Y)
}
# show distribution of aggregate loss S
par(mfrow=c(1,2))
hist(S,freq=F,breaks=100)
plot(ecdf(S),xlab="S")

#(b) Estimate mean and variance of S using the random sample. Comment on the accuracy
#by comparing with the true values. How can one improve the accuracy in the simulation?
# applications
# 1) find descriptive statistics
means <- mean(S)                             # sample mean
means
sds <- sd(S)                               # sample standard deviation
sds
quantile(S,prob=c(0.05,0.5,0.95))   # percentiles

truemean = theta*lambda
truemean
truevar= lambda*theta^2+lambda*theta^2
truevar^(1/2)
truevar

#(c) Estimate Pr(S > 10; 000) using the random sample and normal approximation. Which
#method, simulation or normal approximation, do you prefer? Why?
sum(S>10000)/size

zvalue <- (10000-means)/sds
zvalue
normapprox = 1- pnorm(zvalue)
normapprox

zvalue2 <- (10000-truemean)/(truevar^(1/2))
zvalue2
normapproxtrue = 1- pnorm(zvalue2)
normapproxtrue
#(d) Estimate risk measures VaR and CTE at signicance level  = 5%.
VaR = quantile(S,prob=0.95)         # significance level = 0.01
CTE = sum(S*(S>VaR))/sum((S>VaR))
rm = c(VaR,CTE)
names(rm) = c("VaR","CTE")
print(rm)


#2. Assume that the insurer introduces a per-occurrence deductible of $500 and a 80% coinsur-ance:
#(a) Let NL denote the number of claims, and NP the number of positive payments. Generate
#1,000 realizations of NL, and use these realizations to generate 1,000 realizations of NP .
#i. Show the empirical CDF of NL and NP . Comment on the features of the two
#distributions.
d <- 500
size <- 1000
lambda <- 5
theta <- 1000

set.seed(123)

S2 <- rep(NA,size)
N <- rpois(size,lambda)
for (i in 1:size){
  
  Y <- rexp(N[i], rate= 1/theta)
  S2[i] <- sum(Y)
}

NP <-rep(NA, size)
for (i in 1:size){
  Y <- rexp(N[i], rate= 1/theta)
  NP[i] <- sum(Y>d)
}
par(mfrow=c(1,2))
hist(S2,freq=F,breaks=100)
plot(ecdf(S2),xlab="S2")


#ii. Compare the theoretical distribution and the empirical distribution of NP . Speci-
#cally, perform a chi-square test.
numrow<-max(NP)+1
pois<- rep(0,numrow+1)
for(i in 1:numrow){
  pois[i]<-length(NP)*dpois(i-1, lambda*exp(-0.5))
}
pois[numrow+1]<- length(NP)-sum(pois)
pois

emp<-rep(0,numrow+1)
for(i in 1:(numrow+1)){
  emp[i]<-sum(NP==i-1)
}
emp
chipois<-sum((pois-emp)^2/pois)
chipois
#there is 9 columns and no paremeters so our df=9-1=8

#(b) Let Y L denote the payment per loss variable, and Y P the payment per payment variable.
#Generate 1,000 realizations of Y , and use these realizations to generate Y L and Y P .1
#i. Show the empirical CDF of Y L and Y P . Comment on the features of the two
#distributions.
set.seed(123)
YY <- rexp(size,1/theta)
head(YY)

YL <- pmax((0.2)*(YY-500),0)
sub <- subset(YL, YL > 0)
plot(ecdf(YL))
plot(ecdf(YY))

YP<-rexp(size,1/(.8*theta)) #memoryless property


mean(YP)
#ii. Compare the theoretical distribution and the empirical distribution of Y P . Speci-cally, perform a K-S test.
ks.test(YP, "pexp",rate=1/(0.8*theta))

#3. Assume that the insurer introduces a per-occurrence deductible of $500 and a policy limit of
#$2; 500. In addition, there is an aggregate deductible of $2; 500. Use simulation to determine
#the stop-loss premium.
set.seed(123)
s.stop.loss <- rep(NA,size)
n.stop.loss <- rpois(size,lambda)
for (i in 1:size){
  uu <- runif(n.stop.loss[i])
  Y <- -theta*log(1-uu)
  Y <- Y-500
  Y[Y>2500] <- 2500
  Y[Y<0]<-0
  s.stop.loss[i] <-sum(Y)
}
s.stop.loss<-s.stop.loss-2500
s.stop.loss[s.stop.loss<0]<-0
mean(s.stop.loss)

