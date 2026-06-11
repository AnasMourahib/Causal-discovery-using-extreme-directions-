
N <- 1000
alpha <- 1
xm <- 1


########Latent variables
Z1 <- runif(n)^(-1/alpha)
Z2 <- runif(n)^(-1/alpha)

X1 = Z1
X2 = (X1)^(2) + Z2


