#help function for getting the ci
get_path_ci <- function(result_matrix, bootstrap_iter, bootstrap_ci){
  ordered_matrix <- aperm (apply (result_matrix, 1:2, sort), c(2, 3, 1))
  lower_index <- ceiling((bootstrap_iter * (1-bootstrap_ci))/2)
  upper_index <- bootstrap_iter-lower_index

  return(list("lower"=ordered_matrix[ , , lower_index], "upper"=ordered_matrix[ , , upper_index]))
}
