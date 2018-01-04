#################################################################################################
# SoA Exam C # 3
data <- c(2,3,3,3,7)    # c(.) means "concatenate"
plot(density(data))     # visualize the results, "density" calculates the kernel density function
# calculate the kernel density estimate at 2.5
density(data, kernel = "triangular", bw=2/sqrt(6),from=2.5,to=2.5, n=1)
# in R, "bw" is the standard deviation of the kernel
# for example, specifying a uniform (or "rectangular") over [-1,1], the variance
# is (1- (-1))^2/12 = 4/12 = 1/3, so if we want b=2, specify bw=2/sqrt(3)
# check that the standard deviation for the triangular kernel is 1/sqrt(6)
# So, if we want b=2, specify bw=2/sqrt(6)

# Just for fun, try out these examples
plot(density(data, bw=.33), main="", xlab="x")                # Change the bandwidth
plot(density(data, kernel = "triangular"), main="", xlab="x") # Change the kernel
#################################################################################################
# SoA Exam C # 46
library(survival)
data <- c(2,3,3,5,5,6,7,7,9,10)
status <- c(1,1,1,1,0,1,1,0,1,0)
(SurvObject <- Surv(data, status,type="right"))              # create a survival object
(surv.fit <- survfit(SurvObject ~ 1,type="kaplan-meier"))    # calculates the survival curve
rbind(surv.fit$time, surv.fit$surv)                          # gives the curve for these time points
#  This problem wants the survival curve at 8. Because the product-limit estimate is constant 
#  between observations, the value at 8 is found from the curve at 7, which is 0.36.

# Just for fun, you can visualize the curve
plot(survfit(SurvObject ~ 1, type="kaplan-meier", conf.int=FALSE))
plot(survfit(SurvObject ~ 1, type="kaplan-meier", conf.type="plain"))

# Try doing it with the "fleming-harrington"-Nelson-Aalen estimate of the cumulative hazard
(survFH.fit <- survfit(SurvObject ~ 1,type="fleming-harrington"))    # calculates the survival curve
rbind(survFH.fit$time, survFH.fit$surv)                          # gives the curve for these time points

#################################################################################################
# SoA Exam C # 252
library(survival)
data <- c(4,4,5,5,5,8,10,10,12,15)
status <- c(1,1,0,0,0,1,0,0,1,1)
(SurvObject <- Surv(data, status,type="right"))              # create a survival object
(surv.fit <- survfit(SurvObject ~ 1,type="kaplan-meier"))    # calculates the survival curve
Greenwood.Var <- (surv.fit$surv*surv.fit$std.err)^2
rbind(surv.fit$time, surv.fit$surv,Greenwood.Var)            # gives the curve for these time points

# Just for fun, here is the "fleming-harrington"-Nelson-Aalen estimate of the cumulative hazard
(survFH.fit <- survfit(SurvObject ~ 1,type="fleming-harrington"))    # calculates the survival curve
rbind(survFH.fit$time, survFH.fit$surv)  
