#help function for getting the ci
get_ci <- function(result_matrix, bootstrap_iter, bootstrap_ci){
  ordered_matrix <- apply(result_matrix, c(1,2), order)
  lower_index <- ceiling((bootstrap_iter * (1-bootstrap_ci))/2)
  upper_index <- bootstrap_iter-lower_index

  return(list("lower"=ordered_matrix[ , , lower_index], "upper"=ordered_matrix[ , , upper_index]))
}
