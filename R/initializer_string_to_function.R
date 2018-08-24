#' Converts initializer string names to function handle
#'
#' This is an internal function used to convert each implemented initialiser
#' name to its corresponding function handle.
#'
#' Supported options are: PCA and Full. The conversion renders the name
#' case-insensitive.
#' @param initializer_name A string indicating the name of the initialiser method.
#' @return A function handle to an initialiser function which takes a node as an
#'   argument.
initializer_string_to_function <- function(initializer_name){

  if(tolower(initializer_name) == "pca"){
    return(PCA_initializer)
  }
  else if(tolower(initializer_name) == "full"){
    return(full_initializer)
  }
}
