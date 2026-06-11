install.packages("mev")
library(mev)


####Observational sample 
set.seed(123)
a21=a22=1
N1 <- 100
Z_small_1 <- rmev(N1, d = 2, param = 0.25, model = "log")
Z_small_2 <- rmev(N1, d = 2, param = 0.25, model = "log")
Z_small_1 <- (1 - exp(-1 / Z_small_1))^(-1 / alpha)
Z_small_2 <- (1 - exp(-1 / Z_small_2))^(-1 / alpha)

X1_small <- Z_small_1[,1]
X2_small <- pmax(a21 * Z_small_1[,2], a22 * Z_small_2[,2])


#######Intervened sample X1 > u


N2 <- 1000   # big so we get ~1000 exceedances

Z_big_1 <- rmev(N2, d = 2, param = 0.25, model = "log")
Z_big_1 <- (1 - exp(-1 / Z_big_1))^(-1 / alpha)
Z_big_2 <- rmev(N2, d = 2, param = 0.25, model = "log")
Z_big_2 <- (1 - exp(-1 / Z_big_2))^(-1 / alpha)

X1_big <- Z_big_1[,1]
X2_big <- pmax(a21 * Z_big_1[,2], a22 * Z_big_2[,2])

u <- quantile(X1_big, 0.95)

idx <- X1_big > u

X1_cond <- X1_big[idx]
X2_cond <- X2_big[idx]



plot(X1_small, X2_small,
     pch = 16,
     col = rgb(0, 0, 0, 0.8),
     xlab = "X1",
     ylab = "X2",
     xlim = c(1 , 12),
     ylim = c(1 , 12),
     main = "Unconditional vs Intervened (X1 > u)")

points(X1_cond, X2_cond,
       pch = 16,
       col = rgb(1, 0, 0, 0.3))



#######Intervened sample X2 > u


N2 <- 1000   # big so we get ~1000 exceedances





u <- quantile(X2_big, 0.95)





idx <- X2_big > u

X1_cond <- X1_big[idx]
X2_cond <- X2_big[idx]



plot(X1_small, X2_small,
     pch = 16,
     col = rgb(0, 0, 0, 0.8),
     xlab = "X1",
     ylab = "X2",
     xlim = c(1 , 12),
     ylim = c(1 , 12),
     main = "Unconditional vs Intervened (X2 > u)")

points(X1_cond, X2_cond,
       pch = 16,
       col = rgb(1, 0, 0, 0.3))







