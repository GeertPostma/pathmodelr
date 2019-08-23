SOPLS_orthogonalize <- function(X_2, T_1){

  X_2_orth <- (diag(dim(T_1)[[1]]) - tcrossprod(T_1 %*% solve(crossprod(T_1, T_1)), T_1)) %*% X_2

  return(X_2_orth)
}
