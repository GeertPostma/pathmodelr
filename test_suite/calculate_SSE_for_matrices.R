calculate_SSE_for_matrices <- function(X1, X2){
  
  max_n_col <- min(ncol(X1), ncol(X2))
  
  SSE <- sum((as.matrix(X1)[,max_n_col]-(as.matrix(X2)[,max_n_col]))^2)
  
  return(SSE)
}