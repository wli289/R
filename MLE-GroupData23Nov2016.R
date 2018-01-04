### MLE for grouped data
## Exam C#276 SOA
## Losses follow the distribution function F(x)=1-(thetaùúÉ/x), x>0.
## A sample of 20 losses resulted in the following:
## Interval     |Number of Losses
## (0,10]       |      9
## (10,25]      |      6
## (25,infinity)|      5
##Calculate the maximum likelihood estimate of thetaùúÉ.

##Log Likelihood function 
likgrp <- function(theta) {
  loglike <-log(((1-(theta/10))^9)*(((theta/10)-(theta/25))^6)* (((theta/25))^5))
  return(-sum(loglike))
}
#  "optim" is a general purpose minimization function
grplik <- optim(c(1),likgrp,method=c("L-BFGS-B"),hessian=TRUE)
#Estimates - Answer "B" on SoA Problem
grplik$par
#standard error
sqrt(diag(solve(grplik$hessian)))
#t-statistics
(tstat = grplik$par/sqrt(diag(solve(grplik$hessian))) )

#Plot of Negative Log-Likelihood function 
vllh = Vectorize(likgrp,"theta")
theta=seq(0,10, by=0.01)
plot(theta, vllh(theta), pch=16, main ="Negative Log-Likelihood function" , cex=.25, 
     xlab=expression(theta), ylab=expression(paste("L(",theta,")")))


