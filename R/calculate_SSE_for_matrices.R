#' Calculate the Sum of Squared Errors of two matrices
#'
#' Calculates the Sum of Squared Errors of the two given matrices. If the
#' matrices do not have the same number of columns, the larger one is subsetted
#' from the first column onward to yield a matrix of equal dimensions. This
#' means that not all columns may be incorporated in the calculation.
#'
#' @param X1 A matrix of m-by-n
#' @param X2 A matrix of m-by-p
#' @return The sum of squared errors (the difference) of the two matrices.
#' @examples
#' calculate_SSE_for_matrices(matrix(1, nrow=2, ncol=2), matrix(-1, nrow=2, ncol=2))
#' @export
calculate_SSE_for_matrices <- function(X1, X2){

  max_n_col <- min(ncol(X1), ncol(X2))

  SSE <- sum((as.matrix(X1)[,max_n_col]-(as.matrix(X2)[,max_n_col]))^2)

  return(SSE)
}
