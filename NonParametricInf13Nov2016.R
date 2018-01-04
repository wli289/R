# Nonparametric stuff
# Start with a simple example of ten points
(xExample = c(10,rep(15,3),20,rep(23,4),30))
PercentilesxExample <- ecdf(xExample)

###Empirical Distribution Function
plot(PercentilesxExample, main="",xlab="x")

##summary
summary(xExample);sd(xExample)^2

##density plot 
plot(density(xExample), main="", xlab="x")
plot(density(xExample, bw=.33), main="", xlab="x") # Change the bandwidth
plot(density(xExample, kernel = "triangular"), main="", xlab="x") # Change the kernel

##quantiles 
quantile(xExample)

#quantiles : set you own probabilities
quantile(xExample, probs = seq(0, 1, 0.333333))
#help(quantile)

ClaimLev <- read.csv("CLAIMLEVEL.csv", header=TRUE); nrow(ClaimLev); # 6258
ClaimData<-subset(ClaimLev,Year==2010);     #2010 subset 

##Empirical distribution function of Property fund
par(mfrow=c(1, 2))
Percentiles  <- ecdf(ClaimData$Claim)
LogPercentiles  <- ecdf(log(ClaimData$Claim))
plot(Percentiles,  main="", xlab="Claims")
plot(LogPercentiles, main="", xlab="Logarithmic Claims")
#dev.off()

#Density Comparison
hist(log(ClaimData$Claim), main="", ylim=c(0,.35),xlab="Log Expenditures", freq=FALSE, col="lightgray")
lines(density(log(ClaimData$Claim)), col="blue",lwd=2.5)
lines(density(log(ClaimData$Claim), bw=1), col="green")
lines(density(log(ClaimData$Claim), bw=.1), col="red", lty=3)
density(log(ClaimData$Claim))$bw   ##default bandwidth

#####Nonparametric Estimation Tools For Model Selection################

library(MASS)
library(VGAM)
fit.gamma2 <- glm(Claim~1, data=ClaimData,family=Gamma(link=log)) 
summary(fit.gamma2, dispersion = gamma.dispersion(fit.gamma2)) 

(theta<-exp(coef(fit.gamma2))*gamma.dispersion(fit.gamma2)) #mu=theta/alpha
(alpha<-1/gamma.dispersion(fit.gamma2) )

#  Inference assuming a Pareto Distribution
fit.pareto <- vglm(Claim ~ 1, paretoII, loc=0, data = ClaimData)
summary(fit.pareto)
head(fitted(fit.pareto))
exp(coef(fit.pareto))

# Plotting the fit using densities (on a logarithmic scale)
# None of these distributions is doing a great job....
x <- seq(0,15,by=0.01)

par(mfrow=c(1, 2))
LogPercentiles  <- ecdf(log(ClaimData$Claim))
plot(LogPercentiles,  main="", xlab="Claims", cex=0.4)
Fgamma_ex = pgamma(exp(x), shape = alpha, scale=theta)
lines(x,Fgamma_ex,col="blue")
Fpareto_ex = pparetoII(exp(x),loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))
lines(x,Fpareto_ex,col="purple")

plot(density(log(ClaimData$Claim)) ,main="", xlab="Log Expenditures")
fgamma_ex = dgamma(exp(x), shape = alpha, scale=theta)*exp(x)
lines(x,fgamma_ex,col="blue")
fpareto_ex = dparetoII(exp(x),loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))*exp(x)
lines(x,fpareto_ex,col="purple")

#dev.off()

#  PP Plot

par(mfrow=c(1, 2))
Fgamma_ex = pgamma(ClaimData$Claim, shape = alpha, scale=theta)
plot(Percentiles(ClaimData$Claim),Fgamma_ex, xlab="Empirical DF", ylab="Gamma DF",cex=0.4)
abline(0,1)
Fpareto_ex = pparetoII(ClaimData$Claim,loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))
plot(Percentiles(ClaimData$Claim),Fpareto_ex, xlab="Empirical DF", ylab="Pareto DF",cex=0.4)
abline(0,1)
#dev.off()


##q-q plot
par(mfrow=c(2, 2))
xseq = seq(0.0001, 0.9999, by=1/length(ClaimData$Claim))
empquant = quantile(ClaimData$Claim, xseq)
Gammaquant = qgamma(xseq, shape = alpha, scale=theta)
plot(empquant, Gammaquant, xlab="Empirical Quantile", ylab="Gamma Quantile")
abline(0,1)
plot(log(empquant), log(Gammaquant), xlab="Log Emp Quantile", ylab="Log Gamma Quantile")
abline(0,1)
Paretoquant = qparetoII(xseq,loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))
plot(empquant, Paretoquant, xlab="Empirical Quantile", ylab="Pareto Quantile")
abline(0,1)
plot(log(empquant), log(Paretoquant), xlab="Log Emp Quantile", ylab="Log Pareto Quantile")
abline(0,1)


############Goodness of Fit Statistics###########################
library(goftest)
#Kolmogorov-Smirnov # the test statistic is "D"
ks.test(ClaimData$Claim, "pgamma", shape = alpha, scale=theta)
ks.test(ClaimData$Claim, "pparetoII",loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))

#Cramer-von Mises # the test statistic is "omega2"
cvm.test(ClaimData$Claim, "pgamma", shape = alpha, scale=theta)
cvm.test(ClaimData$Claim, "pparetoII",loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))

#Anderson-Darling # the test statistic is "An"
ad.test(ClaimData$Claim, "pgamma", shape = alpha, scale=theta)
ad.test(ClaimData$Claim, "pparetoII",loc=0,shape = exp(coef(fit.pareto)[2]), scale = exp(coef(fit.pareto)[1]))



