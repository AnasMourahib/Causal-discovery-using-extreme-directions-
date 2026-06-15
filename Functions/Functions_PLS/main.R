main <- function(seed , N ,  A  , alpha , type = c("Pareto") , tail_fraction , lambda_grid , grid  , num_col ,  num_class , sd_Noise , cl , pen_exp ){
  d <- nrow(A)
  q <- nrow(grid)
  set.seed(seed)
  X <- Generate_Factor_Model(N , A , alpha , type = type )
  #Noise <- matrix( rnorm(N * d , mean = 0 , sd = sd_Noise)  ,  nrow = N , ncol = d )
  #X <- X + Noise

  #####Initial values
  initial_A <- starting_point(data = X , nrcol = num_col)
  initial_alpha <- 0.5
  initial <- c(initial_A  , initial_alpha)

  k <- round(N * tail_fraction)
  #Calculate the empirical stdf estimates on the grid points for the cross validation part
  w <- W_calculus(k = k, num_class = num_class, X = X , grid = grid , q = q)
  #Calculate the empitical stdf estimates on the grid points for the estimation part
  w_total <- sapply(1:q, function(m) stdfEmp(R, k, grid[m, ]))

  # Cross-validation wrapper
  wrapper_cross_validation <- function(lambda) {
    cross_validation(q = q  , d = d , w = w , grid = grid , lambda  = lambda , num_col = num_col , initial = initial , num_class = num_class , pen_exp )
      }

  # Parallelize cross-validation
  scores <- unlist(parLapply(cl, lambda_grid, wrapper_cross_validation))

  nan <-  which(is.nan(scores)  )
  if(length(nan)>0){
    scores <- scores[ -nan   ]
  }
  # Determine optimal lambda
  index <- which(scores == min(scores))
  lambda_optim <- lambda_grid[index]

  result <- param_estim(q = q , d = d , w = w_total , grid = grid , num_col = num_col , lambda = lambda_optim, initial = initial , pen_exp)
  return(result)
}









