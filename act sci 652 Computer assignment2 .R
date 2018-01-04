#1.a
library(VGAM)
alpha1 = 5; theta1 = 10 
alpha2 = 17; theta2 = 8
x = seq(0,400,by=1)
plot(x, dgamma(x, shape = alpha1, scale = theta1) * 0.5 + dgamma(x, shape = alpha2, scale = theta2) * 0.5, type = "l", ylab = "Mixture", xlim=c(0,400))

#1.b
plot(x, pgamma(x, shape = alpha1, scale = theta1) * 0.5 + pgamma(x, shape = alpha2, scale = theta2) * 0.5, type = "l", ylab = "Mixture", xlim=c(0,400))

#1.c
#mean
(alpha1*theta1)*1/2+(alpha2*theta2)*1/2
#variance
(theta1^2*(alpha1+1)*(alpha1)*0.5+theta2^2*(alpha2+1)*(alpha2)*0.5)- (alpha1*theta1*1/2+alpha2*theta2*1/2)^2


#2.a
MEPS1 = read.csv("HealthExpend.csv", header=TRUE) 
MEPS = subset(HealthExpend , EXPENDIP > 0)

#2.b
library(VGAM)
fit.gamma <- vglm(EXPENDIP ~ 1, family = gamma2, data = MEPS)
summary(fit.gamma)
coef(fit.gamma)

(theta <- exp(coef(fit.gamma)[1])/exp(coef(fit.gamma)[2]))
(alpha <- exp(coef(fit.gamma)[2]))

fit.pareto <- vglm(EXPENDIP ~ 1, paretoII, loc = 0, data = MEPS)
summary(fit.pareto)
head(fitted(fit.pareto))
coef(fit.pareto)
exp(coef(fit.pareto))

#2.c

plot(density(log(MEPS$EXPENDIP)), main = "", xlab = "Log Expenditures")
x <- seq(0,15,by = 0.01)
fgamma_ex1 = dgamma(exp(x),scale = theta - 10000, shape = alpha) * exp(x)
lines(x, fgamma_ex1, col = "red")
fgamma_ex2 = dgamma(exp(x),scale = theta, shape = alpha) * exp(x)
lines(x, fgamma_ex2, col = "yellow")
fgamma_ex3 = dgamma(exp(x),scale = theta + 10000, shape = alpha) * exp(x)
lines(x, fgamma_ex3, col = "green")



#2.d
fit.pareto <- vglm(EXPENDIP ~ 1, paretoII, loc=0, data = MEPS) 
theta2 <- exp(coef(fit.pareto)[1]) 
alpha2 <- exp(coef(fit.pareto)[2])
plot(density(log(MEPS$EXPENDIP)), main="", xlab="Log Expenditures")
x <- seq(0,15,by=0.01)
fpareto_ex1 = dparetoII(exp(x), shape = alpha2, scale=theta2)*exp(x) 
fpareto_ex2 = dparetoII(exp(x), shape = alpha2, scale=theta2+5000)*exp(x) 
fpareto_ex3 = dparetoII(exp(x), shape = alpha2, scale=theta2-5000)*exp(x) 
lines(x,fpareto_ex1,col="purple")
lines(x,fpareto_ex2,col="red")
lines(x,fpareto_ex3,col="purple")
legend("topright", c("Original", "Fitted Pareto1", "Fitted Pareto2","Fitted Pareto3"), lty=1, col = 1:4, cex=0.6)

#3.a
library(moments)
#Simulations
set.seed(2)
nTot <- 20000
alpha <- 3
theta <- 25000

Losses <- rparetoII(nTot, shape = alpha, scale = theta)
summary(Losses)
Claims <- pmax(Losses-5000,0)
hist(log(Claims))

#3.b
mean(Losses)
mean(Claims)
quantile(Losses,.8)
quantile(Claims,.8)

#3.c
#E(x)= theta/(alpha-1)=12500
abs(25000/2)
mean(Losses)
set.seed(1)
mean(replicate(10000,mean(rparetoII(1000, loc=0, shape = 3, scale = 25000))))

#3.d
(mean(Losses)-mean(Claims))/mean(Losses)