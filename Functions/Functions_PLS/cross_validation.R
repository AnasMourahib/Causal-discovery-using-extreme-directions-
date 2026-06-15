cross_validation <- function(q , d , w , grid , lambda , num_col , initial , num_class, pen_exp){
  w_train <- w$train
  w_test <- w$test
  q <- nrow(grid)
  CV <- vector(length = num_class)
  for (class_k in 1:num_class){
    optimizer_minus_class_k <- param_estim(q , d , w_train[[class_k]] , grid   , num_col , lambda , initial , pen_exp )
    estimate_matrix_A <- optimizer_minus_class_k$matrix
    ### Show the result for each fold
    cat("\n The estimated matrix on the k-fold:\n")
    print(estimate_matrix_A)
    #####
    estimate_alpha <- optimizer_minus_class_k$alpha
    cat("\n The estimated alpha on the k-fold:\n")
    print(estimate_alpha)
    Theta <- c(t(estimate_matrix_A) , estimate_alpha)
    CV[class_k] <- Loss_function(Theta , grid , w_test[[class_k]] , lambda , pen_exp )
  }
  return(mean(CV))
}


