#D argument is based on: Næs, Tormod, et al. "Path modelling by sequential PLS regression." Journal of Chemometrics 25.1 (2011): 28-40.
# Needs to be used in order to allow for orthogonalization in cross-validation
SOPLS_orthogonalize <- function(X_2, T_1, D=NULL){

  if(is.null(D)){
    D <- tcrossprod(solve(crossprod(T_1, T_1)), T_1) %*% X_2
    X_2_orth <- X_2 - T_1 %*% D
  }

  X_2_orth <- X_2 - T_1 %*% D

  return(list("X_2_orth"=X_2_orth, "D"=D))
}
