# Loss modeling using R
# Coverage modifications

library(actuar)

# we check the functionalities of 'coverage'

# ordinary deductible
# per payment variable
# thus: Y^P with pdf f_{Y^P}(y) = f_X(y+d)/S_X(d)
# use d=1 as deductible
f <- coverage(dgamma, pgamma, deductible = 1) # create the object
mode(f) # it's a function
f # the coding
f(0, 3) # calculate in x = 0, shape=3, rate=1
f(5, 3) # calculate in x = 5, shape=3, rate=1

dgamma(5 + 1, 3)/pgamma(1, 3, lower = FALSE) # DIY
curve(dgamma(x, 3), from = 0, to = 10, ylim = c(0, 0.3), lwd=1,col="gray")  # original pdf
curve(dgamma(x, 3), from = 1, to = 10, ylim = c(0, 0.3), add=TRUE, lwd=2) 
curve(f(x, 3), from = 0.01, col = "blue", add = TRUE,lwd=2)     # modified pdf
legend("topright", c("Original Pdf", "Modified pdf"), lty=1, cex=0.6,col = c("black","blue"))

##################################################################
# ordinary deductible 
# per payment variable with policy limit, coinsurance and inflation 
f <- coverage(dgamma, pgamma, deductible = 1, limit = 100, coinsurance = 0.9, inflation = 0.05) # create the object
mode(f) # it's a function
f # the coding
f(0, 3) # calculate in x = 0, shape=3, rate=1
f(5, 3) # calculate in x = 5, shape=3, rate=1

curve(dgamma(x, 3), from = 0, to = 10, ylim = c(0, 0.3), lwd=1,col="gray")# original pdf
curve(dgamma(x, 3), from = 1, to = 10, ylim = c(0, 0.3), add=TRUE, lwd=2)
curve(f(x, 3), from = 0.01, col = "blue", add = TRUE,lwd=2)     # modified pdf
legend("topright", c("Original Pdf", "Modified pdf"), lty=1, cex=0.6,col = c("black","blue"))


##################################################################
# ordinary deductible
# per loss variable
# thus: Y^L with pdf 
f <- coverage(dgamma, pgamma, deductible = 1, per.loss = TRUE)
f(0, 3) # mass at 0
pgamma(0+1, 3) # idem

curve(dgamma(x, 3), from = 0, to = 10, ylim = c(0, 0.3), lwd=1, col="gray") # original
curve(dgamma(x, 3), from = 1, to = 10, ylim = c(0, 0.3), lwd=2, add=TRUE)
curve(f(x, 3), from = 0.01, col = "blue", add = TRUE, lwd=2) # modified
points(0, f(0, 3), pch = 16, col = "blue")
legend("topright", c("Original Pdf", "Modified pdf"), lty=1, cex=0.6,col = c("black","blue"))

##################################################################
# franchise deductible
# per payment variable
f <- coverage(dgamma, pgamma, deductible = 1, franchise = TRUE)
f(0, 3)   # x = 0
f(0.5, 3) # 0 < x < 1
f(1, 3) # x = 1
f(5, 3) # x > 1

dgamma(5, 3)/pgamma(1, 3, lower = FALSE) # idem

curve(dgamma(x, 3), from = 0, to = 10, ylim = c(0, 0.3)) 	# original
curve(f(x, 3), from = 1.1, col = "blue", add = TRUE) 		# modified
curve(f(x, 3), from = 0, to = 1, col = "blue", add = TRUE)  # 0 < x < 1
legend("topright", c("Original Pdf", "Modified pdf"), lty=1, cex=0.6,col = c("black","blue"))

##################################################################
# franchise deductible
# per loss variable
f <- coverage(dgamma, pgamma, deductible = 1,
per.loss = TRUE, franchise = TRUE)
f(0, 3) 	  # mass at 0
pgamma(1, 3)  # idem
f(0.5, 3) # 0 < x < 1
f(1, 3)   # x = 1
f(5, 3)   # x > 1
dgamma(5,3)

curve(dgamma(x, 3), from = 0, to = 10, ylim = c(0, 0.3)) # original
curve(f(x, 3), from = 1.1, col = "blue", add = TRUE) # modified
points(0, f(0, 3), pch = 16, col = "blue") # mass at 0
curve(f(x, 3), from = 0.1, to = 1, col = "blue", add = TRUE) # 0 < x < 1
legend("topright", c("Original Pdf", "Modified pdf"), lty=1, cex=0.6,col = c("black","blue"))
##################################################################






