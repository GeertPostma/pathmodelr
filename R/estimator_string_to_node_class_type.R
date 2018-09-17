#' Converts estimator string names to Node class type
#'
#' This is an internal function used to convert each implemented estimator name
#' to its Node class type.
#'
#' Supported options are: PLS. Other estimators are set to the general R6Class
#' Node. The conversion renders the name case-insensitive.
#' @param estimator_name A string indicating the name of the estimator method.
#' @return A R6Class Node, or a R6Class which inherits from Node.
#' @include node_classes.R
estimator_string_to_node_class_type <- function(estimator_name){

  if(tolower(estimator_name) == "pls"){
    return(PLSNode)
  }
  else {
    return(Node)
  }

}
