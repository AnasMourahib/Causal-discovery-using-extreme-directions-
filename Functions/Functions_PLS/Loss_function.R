
#Here, $\Theta \in \mathbb{R}^{dr+1}$, and its first $dr$ components are obtained by stacking the entries of a $d \times r$ matrix row by row. The last elements is alpha
Loss_function <- function(Theta , grid , w , lambda , alpha , pen_exp  ){
  d <- ncol(grid)
  p <- length(Theta)
  Theta_A <- Theta
  Matrix_A <- matrix(Theta_A , nrow  = d , byrow = TRUE)
  Theta_alpha <- alpha
  sub_stdf <- function(x) { stdf_FactorModel(Matrix_A , Theta_alpha , x) }
  stdf_Grid <- apply(grid , 1 , sub_stdf)
  penal <- penalization(Matrix_A , pen_exp)
  Loss <- sum( (stdf_Grid - w)^(2)  ) + lambda * penal
  return(Loss)
}
