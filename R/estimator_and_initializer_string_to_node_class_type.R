#' Converts estimator and initializer string names to Node class type
#'
#' This is an internal function used to convert each implemented estimator or
#' initializer name to its Node class type.
#'
#' Supported options are: "normalpls" and "endpls" Other estimators are set to
#' the general R6Class Node. The conversion renders the name case-insensitive.
#' @param estimator_name A string indicating the name of the estimator method.
#' @param estimator_name A string indicating the name of the initializer method.
#' @return A R6Class Node, or a R6Class which inherits from Node.
#' @include node_classes.R
estimator_and_initializer_string_to_node_class_type <- function(estimator_name, initializer_name){

  if(tolower(estimator_name) == "normalpls"){
    return(PLSNode)
  }
  else if(tolower(estimator_name) == "endpls"){
    return(PLSNode)
  }
  else if(tolower(estimator_name) == "normalsopls"){
    return(SOPLSNode)
  }
  else if(tolower(estimator_name) == "endsopls"){
    return(SOPLSNode)
  }
  else if(tolower(initializer_name) == "normalpls"){
    return(PLSNode)
  }
  else if(tolower(initializer_name) == "endpls"){
    return(PLSNode)
  }
  else if(tolower(initializer_name) == "startsopls"){
    return(DataNode)
  }
  else if(tolower(initializer_name) == "normalsopls"){
    return(SOPLSNode)
  }
  else {
    return(Node)
  }

}
