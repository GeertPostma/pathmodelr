#' Calculates the Mean Squared Error of two vectors or matrices
#'
#' Calculates the Mean of the squared errors, or the difference between the two
#' input vectors or matrices.
#'
#' When the number is higher than the number of variables in the internal X
#' matrix, the function might display unwanted behaviour.
#'
#' @param target A m-by-n matrix or length m vector of target values.
#' @param predicted A m-by-n matrix or length m vectorof predicted values.
#' @return A Double indicating the Mean Squared Error
#' @export
MSE <- function(target, predicted){

  return(mean((target-predicted)^2))

}
#Does sign flip to allow for minimization
#' @export
SSE <- function(target, predicted){

  return(sum((target-predicted)^2))
}
