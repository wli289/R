
#  THIS CODE USES THE DATASET SingaporeAuto.csv

#  SINGAPORE DATA
Singapore = read.csv("SingaporeAuto.csv",  quote = "",header=TRUE)
#  CHECK THE NAMES, DIMENSION IN THE FILE AND LIST THE FIRST 8 OBSERVATIONS  ;
names(Singapore)
dim(Singapore)
Singapore[1:8,]
attach(Singapore)

#  GOODNESS OF FIT FOR POISSON DISTRIBUTION 
table(Clm_Count) 
n<-length(Clm_Count)

CountPoisson1 = glm(Clm_Count ~ 1,poisson(link=log))
summary(CountPoisson1)
lambda<-exp(CountPoisson1$coefficients)
lambda

table1p = cbind(n*(dpois(0,lambda)),
                n*(dpois(1,lambda)),
                n*(dpois(2,lambda)),
                n*(dpois(3,lambda)),
                n*(dpois(4,lambda)))
table1p


#  PEARSON GOODNESS-OF-FIT STATISTIC
actual = data.frame(table(Clm_Count))[,2];
actual[5] = 0
diff = actual-table1p
(PearsonG = sum(diff*diff/table1p))
cbind(table1p,PearsonG)
#  p-value
1-pchisq(PearsonG, df=5-1-1)


#  GOODNESS OF FIT FOR NEGATIVE BINOMIAL DISTRIBUTION  
library(MASS)
fm_nb <- glm.nb(Clm_Count~1,link=log)
summary(fm_nb)

fm_nb$theta
beta<-exp(fm_nb$coefficients)/fm_nb$theta
prob<-1/(1+beta)

table1nb = cbind(n*(dnbinom(0,size=fm_nb$theta,prob)),
                n*(dnbinom(1,size=fm_nb$theta,prob)),
                n*(dnbinom(2,size=fm_nb$theta,prob)),
                n*(dnbinom(3,size=fm_nb$theta,prob)),
                n*(dnbinom(4,size=fm_nb$theta,prob)));


#  PEARSON GOODNESS-OF-FIT STATISTIC
  actual = data.frame(table(Clm_Count))[,2];actual[5] = 0
  diff = actual-table1nb
  Pearson_nb = sum(diff*diff/table1nb);
  cbind(table1p,Pearson_nb)
#  p-value
1-pchisq(Pearson_nb, df=5-2-1)







