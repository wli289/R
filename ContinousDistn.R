# Basics R
# Continuous distributions in R, splicing and mixing

#install.packages("actuar")
library(actuar)
library(VGAM)

###############################################################
# Example 1: gamma distribution
# define a grid
x <- seq(0,1000,by=1)

# define a set of scale and shape parameters
scaleparam <- seq(100,250,by=50)
shapeparam <- 2:5

# varying the shape parameter
plot(x, dgamma(x, shape = shapeparam[1], scale = 100), type = "l", ylab = "Gamma density")

for(k in 2:length(shapeparam)){
  lines(x,dgamma(x,shape = shapeparam[k], scale = 100), col = k)
}
legend("topright", c("shape=2", "shape=3", "shape=4", "shape=5"), lty=1, col = 1:4)
title("Pdf gamma density, with scale=100, and varying shape")

###############################################################
# varying the scale parameter
plot(x, dgamma(x, shape = 2, scale = scaleparam[1]), type = "l", ylab = "Gamma density")

for(k in 2:length(scaleparam)){
  lines(x,dgamma(x,shape = 2, scale = scaleparam[k]), col = k)
}

legend("topright", c("scale=100", "scale=150", "scale=200", "scale=250"), lty=1, col = 1:4)
title("Pdf gamma density, with shape=2, and varying scale")

###############################################################
## Example 2: Pareto density 

z<- seq(0,3000,by=1)

scaleparam <- seq(2000,3500,500)
shapeparam <- 1:4

# varying the shape parameter
plot(z, dparetoII(z, loc=0, shape = shapeparam[1], scale = 2000), ylim=c(0,0.002),type = "l", ylab = "Pareto density")

for(k in 2:length(shapeparam)){
  lines(z,dparetoII(z,loc=0, shape = shapeparam[k], scale = 2000), col = k)
}
legend("topright", c("shape=1", "shape=2", "shape=3", "shape=4"), lty=1, col = 1:4)
title("Pdf Pareto density, with scale=2000, and varying shape")

###############################################################
#varying the scale parameter
plot(z, dparetoII(z, loc=0, shape = 3, scale = scaleparam[1]), type = "l", ylab = "Pareto density")

for(k in 2:length(scaleparam)){
  lines(z,dparetoII(z,loc=0, shape = 3, scale = scaleparam[k]), col = k)
}

legend("topright", c("scale=2000", "scale=2500", "scale=3000", "scale=3500"), lty=1, col = 1:4)
title("Pdf Pareto density, with shape=3, and varying scale")
###############################################################
## Example 3: Weibull density 

z<- seq(0,400,by=1)

scaleparam <- seq(50,200,50)
shapeparam <- seq(1.5,3,0.5)

# varying the shape parameter
plot(z, dweibull(z, shape = shapeparam[1], scale = 100), ylim=c(0,0.012), type = "l", ylab = "Weibull density")

for(k in 2:length(shapeparam)){
  lines(z,dweibull(z,shape = shapeparam[k], scale = 100), col = k)
}

legend("topright", c("shape=1.5", "shape=2", "shape=2.5", "shape=3"), lty=1, col = 1:4)
title("Pdf Weibull density, with scale=100, and varying shape")
###############################################################

#varying the scale parameter
plot(z, dweibull(z, shape = 3, scale = scaleparam[1]), type = "l", ylab = "Weibull density")

for(k in 2:length(scaleparam)){
  lines(z,dweibull(z,shape = 3, scale = scaleparam[k]), col = k)
}
legend("topright", c("scale=50", "scale=100", "scale=150", "scale=200"), lty=1, col = 1:4)
title("Pdf Weibull density, with shape=3, and varying scale")
###############################################################
## Example 4:GB2
gb2density <- function(x,shape1,shape2,shape3,scale){
  mu <- log(scale)
  sigma <- 1/shape3
  xt <- (log(x)-mu)/sigma
  logexpxt<-ifelse(xt>23,yt,log(1+exp(xt)))
  logdens <- shape1*xt - log(sigma) - log(beta(shape1,shape2)) - (shape1+shape2)*logexpxt -log(x) 
  exp(logdens)
}
x<- seq(0,400,by=1)

alpha1<-5
alpha2<-4 
gamma <-2
theta <- seq(150,250,50)

# varying the scale parameter
plot(x, gb2density(x, shape1=alpha1,shape2=alpha2,shape3=gamma, scale = theta[1]), 
     type = "l", ylab = "Gen Beta 2 density",
     main = 
     expression(paste("GB2 density with ", alpha[1], "=5,", alpha[2], "=4,", alpha[3], 
                      "=2, and varying scale (",theta, ") parameters")) )

for(k in 2:length(theta)){
  lines(z,gb2density(x,shape1=alpha1,shape2=alpha2,shape3=gamma, scale = theta[k]), col = k)
}
legend("topleft", c("theta=150", "theta=200", "theta=250"), lty=1, cex=0.6,col = 1:3)

###############################################################
## Example 5: A mixed density
## specify density of a mixture of 2 gamma distributions
MixtureGammaDensity <- function(x, a1, a2, alphaGamma1, thetaGamma1, alphaGamma2, thetaGamma2){
  a1 * dgamma(x, shape = alphaGamma1, scale = thetaGamma1) + a2 * dgamma(x, shape = alphaGamma2, scale = thetaGamma2)
}

w <- 1:30000/100
a1<-0.5
a2<-0.5
alpha1 <- 4
theta1 <- 7
alpha2 <- 15
theta2 <- 7

MixGammadens <- MixtureGammaDensity(w, a1,a2,alpha1, theta1, alpha2, theta2)

plot(w, MixGammadens, type = "l")
###############################################################
# Example 6: density obtained through splicing
## combine an Exp on (0,c) with a Pareto on (c,\infty)

SpliceExpPar <- function(x, c, v, theta, gamma, alpha){
  if(0<=x & x<c){return(v * dexp(x, 1/theta)/pexp(c,1/theta))}else
    if(x>=c){return((1-v)*dparetoII(x,loc=0, shape = alpha, scale = theta)/(1-pparetoII(x,loc=0, shape = alpha, scale = theta)))}
}

x <- t(as.matrix(1:2500/10))

spliceValues <- apply(x,2,SpliceExpPar, c = 100, v = 0.6, theta = 100, gamma = 200, alpha = 4)

plot(x,spliceValues, type = 'l')
###############################################################
## Example 7: Gamma vs Pareto Tails 

y <- 100:10000/100

fgamma <- dgamma(y,shape=1/3,scale=15)        #Gamma density function with mean=5 and variance=75


fpareto <- dpareto(y, shape = 3, scale = 10)  #Pareto density function with mean=5 and variance=75

plot(y,fgamma,lwd=2,type="l",ylim=c(0,0.2))
lines(y, fpareto, col = "red", lwd = 2)      ## but tail behavior is different
