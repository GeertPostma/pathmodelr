#' Converts estimator string names to function handle
#'
#' This is an internal function used to convert each implemented estimator name
#' to its corresponding function/closure.
#'
#' Supported options are: "none". The conversion renders the name
#' case-insensitive.
#' @param estimator_name A string indicating the name of the estimator method.
#' @return A function handle to an estimator function which takes a node as an
#'   argument.
estimator_string_to_function <- function(estimator_name, parallelise=FALSE, n_cores=NULL){
if(tolower(estimator_name) == "none"){
    est <- no_estimator
    attr(est, "is_iterative") <- FALSE
    return(est)
  }
}
