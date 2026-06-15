Generate_Factor_Model <- function(N , A , alpha , type = c("Pareto")){
  d <- nrow(A)
  r <- ncol(A)
  multiply <- function(vec) {  A %*% vec    }
  if(type == "Pareto"){
    Z <- rpareto(N * r , location = 1 , shape = 1)
    Z <- matrix(Z , nrow = N , ncol = r )
    X <- t(apply(Z, 1, multiply))
  }
  return(X)
}

