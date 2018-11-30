calculate_node_PLS_coefficients <- function(model){

  for(i in seq_along(model$nodes)){
      PLS_regression_connection(model$nodes[[i]])
  }

}
