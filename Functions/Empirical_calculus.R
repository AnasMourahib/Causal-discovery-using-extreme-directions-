install.packages("mev")
library(mev)


####Observational sample 
set.seed(7)
N1 <- 10^6
Z1 <- rmev(N1, d = 2, param = 0.5, model = "log")
Z2 <- rmev(N1, d = 2, param = 0.5, model = "log")

a21 = 1/3
a22 = 1
X1 <- Z1[,1]
X2 <- pmax(a21 * Z1[,2], a22 * Z2[,2])

u <- unname(quantile(X1, probs = c(seq(0.9 , 0.99 , by = 0.01))   ))
lu <- length(u)
joint_exc <- rep(0 , lu)

###P(Z^(1)_1 >x \cap X_2 >x) / P(Z^(1)_1 >x )
for (i in 1: lu){
  joint_exc[i] <- length(which(X1 > u[i] & X2 >u[i])) / length(which(X1 > u[i] ))
}

small_o <- rep(0 , lu)
big_o <- rep(0 , lu)
for (i in 1: lu){
  Exc_1 <- length(which(X2 > 2 * u[i] & Z1[,1] > 2 * u[i] & Z1[,2] > 4 * u[i])) 
  Exc_2 <- length(which(X2 > 2 * u[i] & Z1[,1] > 2 * u[i] & Z1[,2] < 4 * u[i])) 
  Exc_3 <-  length(which(X1 > 2 * u[i] ))
  small_o[i] <- Exc_2 / Exc_3
  big_o[i] <- Exc_1 /Exc_3
}



##### Conditional expectation 

set.seed(7)
N1 <- 10^5
Z1 <- rmev(N1, d = 2, param = 0.5, model = "log")
Z2 <- rmev(N1, d = 2, param = 0.5, model = "log")

a11 = 1/2
a21 = 1
a22 = 1
X1 <- a11 * Z1[,1]
X2 <- pmax(a21 * Z1[,2], a22 * Z2[,2])


estimate_expectation <- function(X2, Z1, x, a11) {
  
  # empirical CDF evaluated at the observations
  F2_X2 <- ecdf(X2)(X2)
  
  mean(F2_X2 * (a11 * Z1 > x))
}


estimate_prob <- function(Z1, x, a11 , a21 ) {
  mean(a11 * Z1[,1] > x & a21 * Z1[,2] > x)
}

u <- unname(quantile(X1, probs = c(seq(0.9 , 0.99 , by = 0.01))   ))
lu <- length(u)
fraction <- rep(0 , lu)
conditional_expectation <- rep(0 , lu)
joint_exceedance <- rep(0 , lu)
for (i in 1: lu){
  conditional_expectation[i] <- estimate_expectation(X2 = X2, Z1 = Z1[,1], x = u[i], a11 = a11)
  joint_exceedance[i] <- estimate_prob(Z1 = Z1 , x = u[i] , a11 = a11 , a21 = a21)
  fraction[i]  <- conditional_expectation[i] / joint_exceedance[i]
}
