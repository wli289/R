# This Example shows how to fit basic distributions to a data set

ClaimLev <- read.csv("CLAIMLEVEL.csv", header=TRUE); nrow(ClaimLev); # 6258
#2010 subset 
ClaimData<-subset(ClaimLev,Year==2010); 
length(unique(ClaimData$PolicyNum))        #403 unique policyholders
NTot = nrow(ClaimData)                     #1377 individual claims

# As an alternative, you can simulate Claims
#NTot = 13770
#alphahat = 2
#thetahat =  100

#Claim = rgamma(NTot, shape = alphahat, scale = thetahat)
#Claim = rparetoII(NTot, loc = 0,  shape = alphahat, scale = thetahat)
# GB2
#Claim = thetahat*rgamma(NTot, shape = alphahat, scale = 1)/rgamma(NTot, shape = 1, scale =1) 
#ClaimData <- data.frame(Claim)

###################################################
# Summarizing the Data
summary(ClaimData$Claim);     sd(ClaimData$Claim)
summary(log(ClaimData$Claim));sd(log(ClaimData$Claim))

#histogram 
par(mfrow=c(1, 2))
hist(ClaimData$Claim, main="", xlab="Claims")
hist(log(ClaimData$Claim), main="", xlab="Logarithmic Claims")
#dev.off()
###################################################
#  Inference assuming a lognormal distribution
#  First, take the log of the data and assume normality
y = log(ClaimData$Claim)
summary(y);sd(y)
# confidence intervals and hypothesis test
t.test(y,mu=log(5000))   # H0: mu_o=log(5000)=8.517

#mean of the lognormal distribution
exp(mean(y)+sd(y)^2/2)
mean(ClaimData$Claim)

#Alternatively, assume that the data follow a lognormal distribution
#Use "VGAM" library for estimation of parameters 
library(VGAM)
fit.LN <- vglm(Claim ~ 1, family=lognormal, data = ClaimData)
summary(fit.LN)
coef(fit.LN)                 # coefficients
confint(fit.LN, level=0.95)  # confidence intervals for model parameters 
logLik(fit.LN)               #loglikelihood for lognormal
AIC(fit.LN)                  #AIC for lognormal
BIC(fit.LN)                  #BIC for lognormal
vcov(fit.LN)                 # covariance matrix for model parameters 

#mean of the lognormal distribution
exp(mean(y)+sd(y)^2/2)

exp(coef(fit.LN))

###################################################
#  Inference assuming a gamma distribution
#install.packages("VGAM")
library(VGAM)
fit.gamma <- vglm(Claim ~ 1, family=gamma2, data = ClaimData)
summary(fit.gamma)
coef(fit.gamma)                 # This uses a different parameterization 

(theta<-exp(coef(fit.gamma)[1])/exp(coef(fit.gamma)[2])) #mu=theta/alpha
(alpha<-exp(coef(fit.gamma)[2]))

plot(density(log(ClaimData$Claim)), main="", xlab="Log Expenditures")
x <- seq(0,15,by=0.01)
fgamma_ex = dgamma(exp(x), shape = alpha, scale=theta)*exp(x)
lines(x,fgamma_ex,col="blue")

confint(fit.gamma, level=0.95)  # confidence intervals for model parameters 
logLik(fit.gamma)               #loglikelihood for gamma
AIC(fit.gamma)                  #AIC for gamma
BIC(fit.gamma)                  #BIC for gamma
vcov(fit.gamma)                 # covariance matrix for model parameters 

# Here is a check on the formulas
#AIC using formula : -2*(loglik)+2(number of parameters)
-2*(logLik(fit.gamma))+2*(length(coef(fit.gamma)))
#BIC using formula : -2*(loglik)+(number of parameters)*(log(n))
-2*(logLik(fit.gamma))+length(coef(fit.gamma, matrix = TRUE))*log(nrow(ClaimData))

#Alternatively, we could a gamma distribution using glm
library(MASS)
fit.gamma2 <- glm(Claim~1, data=ClaimData,family=Gamma(link=log)) 
summary(fit.gamma2, dispersion = gamma.dispersion(fit.gamma2)) 

(theta<-exp(coef(fit.gamma2))*gamma.dispersion(fit.gamma2)) #mu=theta/alpha
(alpha<-1/gamma.dispersion(fit.gamma2) )

logLik(fit.gamma2)  #log - likelihood slightly different from vglm
AIC(fit.gamma2)     #AIC
BIC(fit.gamma2)     #BIC
###################################################
#  Inference assuming a Pareto Distribution
fit.pareto <- vglm(Claim ~ 1, paretoII, loc=0, data = ClaimData)
summary(fit.pareto)
head(fitted(fit.pareto))
coef(fit.pareto)
exp(coef(fit.pareto))

confint(fit.pareto, level=0.95)  # confidence intervals for model parameters 
logLik(fit.pareto)               #loglikelihood for pareto
AIC(fit.pareto)                  #AIC for pareto
BIC(fit.pareto)                  #BIC for pareto
vcov(fit.pareto)                 # covariance matrix for model parameters 
###################################################
#  Inference assuming an exponential distribution
fit.exp <- vglm(Claim ~ 1, exponential, data = ClaimData)
summary(fit.exp)
(theta = 1/exp(coef(fit.exp)))

# Can also fit using the "glm" package
fit.exp2 <- glm(Claim~1, data=ClaimData,family=Gamma(link=log)) 
summary(fit.exp2,dispersion=1)
(theta<-exp(coef(fit.exp2)))  

###################################################
#  Inference assuming a GB2 Distribution - this is more complicated
# The likelihood functon of GB2 distribution (negative for optimization)
likgb2 <- function(param) {
  a1 <- param[1]
  a2 <- param[2]
  mu <- param[3]
  sigma <- param[4]
  yt <- (log(ClaimData$Claim)-mu)/sigma
  logexpyt<-ifelse(yt>23,yt,log(1+exp(yt)))
  logdens <- a1*yt - log(sigma) - log(beta(a1,a2)) - (a1+a2)*logexpyt -log(ClaimData$Claim) 
  return(-sum(logdens))
}
#  "optim" is a general purpose minimization function
gb2bop <- optim(c(1,1,0,1),likgb2,method=c("L-BFGS-B"),
                lower=c(0.01,0.01,-500,0.01),upper=c(500,500,500,500),hessian=TRUE)

#Estimates
gb2bop$par
#standard error
sqrt(diag(solve(gb2bop$hessian)))
#t-statistics
(tstat = gb2bop$par/sqrt(diag(solve(gb2bop$hessian))) )

# density for GB II
gb2density <- function(x){
  a1 <- gb2bop$par[1]
  a2 <- gb2bop$par[2]
  mu <- gb2bop$par[3]
  sigma <- gb2bop$par[4]
  xt <- (log(x)-mu)/sigma
  logexpxt<-ifelse(xt>23,yt,log(1+exp(xt)))
  logdens <- a1*xt - log(sigma) - log(beta(a1,a2)) - (a1+a2)*logexpxt -log(x) 
  exp(logdens)
}

#AIC using formula : -2*(loglik)+2(number of parameters)
-2*(sum(log(gb2density(ClaimData$Claim))))+2*4
#BIC using formula : -2*(loglik)+(number of parameters)*(log(n))
-2*(sum(log(gb2density(ClaimData$Claim))))+4*log(nrow(ClaimData))


###################################################
# Plotting the fit using densities (on a logarithmic scale)
# None of these distributions is doing a great job....
plot(density(log(ClaimData$Claim)), main="", xlab="Log Expenditures")
x <- seq(0,15,by=0.01)
fexp_ex = dgamma(exp(x), scale = exp(-coef(fit.exp)), shape = 1)*exp(x)
lines(x,fexp_ex, col="red")
fgamma_ex = dgamma(exp(x), shape = alpha, scale=theta)*exp(x)
lines(x,fgamma_ex,col="blue")
fpareto_ex = dparetoII(exp(x),loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))*exp(x)
lines(x,fpareto_ex,col="purple")
flnorm_ex = dlnorm(exp(x), mean = coef(fit.LN)[1], sd = exp(coef(fit.LN)[2]))*exp(x)
lines(x,flnorm_ex, col="lightblue")
# density for GB II
gb2density <- function(x){
  a1 <- gb2bop$par[1]
  a2 <- gb2bop$par[2]
  mu <- gb2bop$par[3]
  sigma <- gb2bop$par[4]
  xt <- (log(x)-mu)/sigma
  logexpxt<-ifelse(xt>23,yt,log(1+exp(xt)))
  logdens <- a1*xt - log(sigma) - log(beta(a1,a2)) - (a1+a2)*logexpxt -log(x) 
  exp(logdens)
  }
fGB2_ex = gb2density(exp(x))*exp(x)
lines(x,fGB2_ex, col="green")

