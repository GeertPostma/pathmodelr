#' Converts estimator string names to function handle
#'
#' This is an internal function used to convert each implemented estimator name
#' to its corresponding function handle.
#'
#' Supported options are: PLS, PCA, and Full. The conversion renders the name
#' case-insensitive.
#' @param estimator_name A string indicating the name of the estimator method.
#' @return A function handle to an estimator function which takes a node as an
#'   argument.
estimator_string_to_function <- function(estimator_name){
  if(tolower(estimator_name) == "pls"){
    return(PLS_estimator)
  }
  else if(tolower(estimator_name) == "pca"){
    return(PCA_estimator)
  }
  else if(tolower(estimator_name) == "full"){
    return(full_estimator)
  }

}