install.packages("EnvStats")
install.packages("tailDepFun")
install.packages("parallel")

library(EnvStats)
library(tailDepFun)
library(parallel)


source("Functions_Causality/Generate_Factor_Model.R")
source("Functions_Causality/Loss_function.R")
source("Functions_Causality/param_estim.R")
source("Functions_Causality/Starting_points.R")
source("Functions_Causality/stdf_Factormodel.R")
source("Functions_Causality/Help_functions.R")
source("Functions_Causality/cross_validation.R")
source("Functions_Causality/main.R")






rfrechet <- function(n, alpha, scale = 1, location = 0) {
  u <- runif(n)
  location + scale * (-log(u))^(-1 / alpha)
}

n = 10^3

Z1 = rfrechet(n , alpha = 1)
Z2 = rfrechet(n , alpha = 1)
Z3 = rfrechet(n , alpha = 1)


X1 = Z1
X2 = a21 * X1 + Z2
X3 = a32 * X2 + Z3










N <- 10^5
alpha <- 2
a12 <- a13 <- a24 <- a34 <- 0.5
d <- 4
A <- matrix(0 , nrow = d , ncol = d)
diag(A) <- 1
A[2 , 1] <- a12
A[3 , 1]  <- a13
A[4 , 1] <- a12 * a24 + a13 * a34
A[4 , 2] <- a24
A[4 , 3] <- a34

data <- Generate_Factor_Model(N , A , alpha)

points_log <- c(0,1/4,1/3,1/2,3/4,1)
Grid_points <- selectGrid(cst = points_log, d = d, nonzero = c(2,3))
q <- nrow(Grid_points)
k <- round (N * 0.05)
num_col = 4
lambda = 10^(-3)

start <- starting_point(data , num_col)
initial_A  <- c(t(start))
initial_alpha <- 0.5
initial <- c(initial_A  , initial_alpha)
R <- apply(data, 2, rank)
w <- sapply(1:q, function(m) stdfEmp(R, k, Grid_points[m, ]))
W_CV <- W_calculus(k = k, num_class = 10 , X = data , grid = Grid_points , q = q)


#########Test for the param estim function
res <- param_estim (q , d , w , Grid_points  , num_col , lambda, initial_A, pen_exp = 1)

###########Test for the cross validation function

CV <- cross_validation(d = d  , w = W_CV, grid = Grid_points , lambda = lambda , num_col = 4 , initial = initial , num_class = 10 , pen_exp = 1)

##########Test for the main function
seed = 7
N = 10^3
tail_fraction = 0.1
num_col = 4
sd_Noise = 3
num_class = 10
lambda_grid <- 10^(-3)
pen_exp = 1
cl <- makeCluster(7)
# Export necessary functions and variables to cluster

clusterExport(cl, varlist = c("stdf_FactorModel" , "param_estim", "Loss_function" , "penalization_function" , "cross_validation" , "q", "d", "w", "grid", "num_col", "initial", "num_class" , "pen_exp"   ))


res <- main(seed = seed  , N = N ,  A = A  , alpha = alpha , type = c("Pareto") , tail_fraction = tail_fraction , lambda_grid = lambda_grid
     , grid = Grid_points , num_col = num_col ,  num_class = num_class , sd_Noise = sd_Noise , cl = cl , pen_exp = pen_exp )










###########Try with d = 2

N <- 500
alpha <- 2
a12 <- 0.5
d <- 2
A <- matrix(0 , nrow = d , ncol = d)
diag(A) <- 1
A[2 , 1] <- a12

data <- Generate_Factor_Model(N , A , alpha)

points_log <- c(0,1/4,1/3,1/2,3/4,1)
Grid_points <- selectGrid(cst = points_log, d = d, nonzero = c(2))
q <- nrow(Grid_points)
k <- round (N * 0.1)
num_col = 2
lambda = 10^(-8)

start <- starting_point(data , num_col)
initial_A  <- c(t(start))
initial_alpha <- 0.5
initial <- c(initial_A  , initial_alpha)
R <- apply(data, 2, rank)
w <- sapply(1:q, function(m) stdfEmp(R, k, Grid_points[m, ]))
W_CV <- W_calculus(k = k, num_class = 10 , X = data , grid = Grid_points , q = q)


#########Test for the param estim function
res <- param_estim (q , d , w , Grid_points  , num_col , lambda, initial)
