
#data is an (n \times d) matrix, where n is the sample size and d is the dimension
#grid is a (q \times d) matrix, where each row m corresponds to the c_m where we will evaluate the stdf and the empirical stdf

param_estim <- function( d , w , grid , num_col , lambda, initial, alpha = NULL , pen_exp){
  if(is.null(alpha)){  p <- d * num_col + 1 }
  else {  p <- d * num_col }
  sub_Loss_function <-function(Theta){
  Loss_function(Theta = Theta , grid = grid , w = w , lambda =  lambda, alpha , pen_exp) 
    }
  lower_bounds <- rep(0, p)
  temp <- optim(initial, sub_Loss_function , method = "L-BFGS-B", lower = lower_bounds , control = list(maxit = 1000))
  estim <- temp$par
  if(is.null(alpha)){
    estimate_Theta_A <- estim[-p]
    estimate_alpha <- estim[p]
    estimate_matrix_A <- matrix(estimate_Theta_A, nrow = d , byrow = TRUE)
    estimates = list("matrix"  =   estimate_matrix_A , "alpha" = estimate_alpha)
    }
  else {  estimate_Theta_A <- estim
  estimate_matrix_A <- matrix(estimate_Theta_A, nrow = d , byrow = TRUE)
  rowsumss <- rowSums(estimate_matrix_A)
  estimates <- estimate_matrix_A / rowsumss
  }
  return(estimates)
}




######


rfrechet <- function(n, alpha, scale = 1, location = 0) {
  u <- runif(n)
  location + scale * (-log(u))^(-1 / alpha)
}

n = 10^3

Z1 = rfrechet(n , alpha = 1)
Z2 = rfrechet(n , alpha = 1)
Z3 = rfrechet(n , alpha = 1)

a21 = 1/2 
a32 = 1/3
X1 = Z1
X2 = a21 * X1 + (1 - a21) * Z2
X3 = a32 * X2 + (1 - a32) *Z3
X <- cbind(X1 , X2 , X3)





d = 3
R <- apply(X, 2, rank)
points_log <- c(0,1/4,1/3,1/2,3/4,1)
Grid_points <- selectGrid(cst = points_log, d = d, nonzero = c(2,3))
q <- nrow(Grid_points)
k <- 0.1 * N
w <-  sapply(1:q, function(m) stdfEmp(R, k, Grid_points[m, ]))
num_col = 3
lambda = 0.001
initial <- c(starting_point(X , nrcol = num_col , quant = 0.9))
alpha = 1

estimate <- param_estim( d , w , grid = Grid_points , num_col  , lambda  , initial, alpha , pen_exp = 0.2 )
