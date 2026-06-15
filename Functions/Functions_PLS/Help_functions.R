W_calculus <- function(k, num_class, X, grid , q){
  W_train <- list()
  W_test <- list()
  N<-nrow(X)
  for(class_k in 1: num_class) {
    low <- ((class_k-1)*(N/num_class)+1)
    up <- (class_k*(N/num_class))
    X_train=X[-c(low:up),]
    X_test=X[c(low:up),]
    R_train<-apply(X_train,2,rank)
    R_test<-apply(X_test,2,rank)
    W_train[[class_k]] <- sapply(c(1:q), function(m) stdfEmp(R_train, k = (1-1/num_class)*k, grid[m,]))
    W_test[[class_k]]  <- sapply(c(1:q), function(m) stdfEmp(R_test, k = (1/num_class)*k, grid[m,]))
  }
  return(list("train"=W_train, "test"=W_test))
}

penalization_function <- function(p , A){
  A_pow_p <- A^(p)
  sum_col <- apply(A_pow_p, 1, sum)
  res <- sum(sum_col^(1/p))
  return(res)
}
