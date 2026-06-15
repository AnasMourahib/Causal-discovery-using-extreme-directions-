penalization <- function(mat , pen_exp){
  mat_pow <- mat^(pen_exp)
  sum_row <- apply(mat_pow, 1, sum)
  sum <- sum( (sum_row)^(1/pen_exp) )
  return(sum)
}