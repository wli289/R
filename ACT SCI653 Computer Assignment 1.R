dat <- read.csv("datBCbal.csv", header = TRUE)
names(dat)
dat06 <- dat[which(dat$Year==2006),]

#1.a
dat06loss <- dat06$Claim1 + dat06$Claim2 + dat06$Claim3
summary(dat06loss)
mean(dat06loss)
sd(dat06loss)
quantile(dat06loss,c(0.05,0.5,0.95))
#data is heavy tailed

#1.b
library(statmod)
library(tweedie)

fit <- glm(dat06loss~1,family=tweedie(var.power=1.5,link.power=0))
summary(fit)
summary(fit)$coefficient
summary(fit)$dispersion

#mle 
loglik<-function(parms){ 
  p=parms[1]
  mu=exp(parms[2])
  phi=exp(parms[3])
  llk <- -sum(log(dtweedie(dat06loss, p, mu, phi)))
  llk
}
ini <- c(1.5,log(mean(dat06loss)),log(summary(fit)$dispersion))
zop <- nlminb(ini, loglik, lower =c(1+1e-6,-Inf,-Inf),upper =c(2-1e-6,Inf,Inf))
print(zop)

#obtain standard error
library(numDeriv)
est <- zop$par
names(est) <- c("p","mu","phi")
hess<-hessian(loglik,est)
se <-sqrt(diag(solve(hess)))
print(cbind(est,se))

#1.c
dtweedie(0,est[1],exp(est[2]),exp(est[3]))
sum(dat06loss == 0)/length(dat06loss)
# It is  a good fit.
###################################################################################################


#2.a.(1)

I <- (dat06loss>0)*1
table(I)


#2.a.(2)
library(dplyr)
freq.dat = dat %>% group_by(PolicyNum) %>% summarise(tLoss = sum(dat06loss),count = sum(dat06loss>0))
dim(freq.dat)

n  <-  nrow(dat06)
loglikBin <- function(parms){ 
  p = parms[1]
  llk <- -sum(log(dbinom(I,1,p)))
  llk
}
ini.Bin <- 0.5
zop.Bin <- nlminb(ini.Bin,loglikBin,lower=c(0.000000000000001),upper=c(0.9999999999999999))
print(zop.Bin)

library(numDeriv)
estBin <- zop.Bin$par
hess <- hessian(loglikBin, estBin, method.args=list(d = 0.025))
se <- sqrt(diag(solve(hess)))
print(cbind(estBin,se))



#2.a.(3)
#fit frequency table
I0 = dbinom(0,1,estBin[1])*length(dat06loss)
I0
I1 = dbinom(1,1,estBin[1])*length(dat06loss)
I1

#chi-square stat
observed = table(I)
estimated = c(I0,I1)
cbind(I0,I1)
#2.b.(1)
datpos <- dat06loss[which(dat06loss>0)]
hist(datpos, main = "Loss")

#2.b.(2)
plot(ecdf(datpos), main = "loss")

#2.b.(3)
#randon sample
y <- datpos
n <- length(y)
n

#Sample mean
xbar = mean(y)

#Set bootstrap sample size
nboot = 30
tmpdata = sample(y,n*nboot,replace=TRUE)
bootstrap.sample = matrix(tmpdata,nrow=n,ncol=nboot)

#Compute sample mean for each bootstrap sample
bsmeans = colMeans(bootstrap.sample)

#Calculate deviation from sample statistics
delta.star = bsmeans - xbar
delta.lb <- quantile(delta.star,prob=0.025)
delta.ub <- quantile(delta.star,prob=0.975)

#90%% confidence intervel
CI = xbar - c(delta.ub,delta.lb)
print(CI)

#2.b.(4)
library(VGAM)
fit.LN <- vglm(datpos ~ 1, family=lognormal, data = dat06)
summary(fit.LN)
coef(fit.LN)
logLik(fit.LN)
AIC(fit.LN)
BIC(fit.LN)
fit.pareto <- vglm(datpos ~ 1, paretoII, loc=0, data = dat06)
summary(fit.pareto)
coef(fit.pareto)
logLik(fit.pareto)
AIC(fit.pareto)
BIC(fit.pareto)

#2.b.(5)
mu = coef(fit.LN)[1]
sigma = exp(coef(fit.LN)[2])
theta = exp(coef(fit.pareto)[1])
alpha = exp(coef(fit.pareto)[2])

#Lognormal
par(mfrow=c(1,2))
pct <- seq(0.01,0.99,0.01)
plot(qlnorm(pct,mu,sigma),quantile(datpos,probs=pct),
     main="LN", xlab="Theoretical Quantile", ylab="Empirical Quantile",
     xlim=c(0,7.5e5),ylim=c(0,7.5e5))
abline(0,1)

#Pareto
qpareto <- function(p,theta,alpha){theta*((1-p)^(-1/alpha)-1)}

plot(qpareto(pct,theta,alpha),quantile(datpos,probs=pct),
     main="Pareto", xlab="Theoretical Quantile", ylab="Empirical Quantile",
     xlim=c(0,7.5e5),ylim=c(0,7.5e5))
abline(0,1)

