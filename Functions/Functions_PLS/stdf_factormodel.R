#This is the stdf for the factor model with matrix A and tail coefficient alpha
stdf_FactorModel <- function(A , alpha , x){
  d <- nrow(A)
  r <- ncol(A)
  A_pow_alpha <- A^(alpha)
  #sum_A_pow_alpha <- apply(A_pow_alpha, 1, sum)
  #B_alpha <- A_pow_alpha / sum_A_pow_alpha
  A_pow_alpha_timesx <-  A_pow_alpha * x
  res <- sum(apply( A_pow_alpha_timesx , 2 , max ))
  return(res)
}


#####Trial for the stdf_maxlin
#a12 <- a13 <- a24 <- a34 <- 0.5
#A <- matrix(0 , nrow = 4 , ncol = 4)
#diag(A) <- 1
#A[2 , 1] <- a12
#A[3 , 1]  <- a13
#A[4 , 1] <- a12 * a24 + a13 * a34
#A[4 , 2] <- a24
#A[4 , 3] <- a34
#alpha <- 2
#x <- c(0 , 0 , 2 , 0 )
#stdf_maxlin(A , x , alpha)
#############


