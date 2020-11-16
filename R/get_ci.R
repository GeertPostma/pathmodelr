#help function for getting the ci
get_path_ci <- function(result_matrix, bootstrap_iter, bootstrap_ci){
  ordered_matrix <- aperm (apply (result_matrix, 1:2, sort), c(2, 3, 1))
  lower_index <- ceiling((bootstrap_iter * (1-bootstrap_ci))/2)
  upper_index <- bootstrap_iter-lower_index

  return(list("lower"=ordered_matrix[ , , lower_index], "upper"=ordered_matrix[ , , upper_index]))
}

get_effects_ci <- function(result_list, bootstrap_iter, bootstrap_ci){
  lower_index <- ceiling((bootstrap_iter * (1-bootstrap_ci))/2)
  upper_index <- bootstrap_iter-lower_index

  sorted_list <- lapply(result_list, function(result) aperm(apply(result, 1, sort), c(2,1)))

  lower <- lapply(sorted_list, function(result) result[,lower_index])
  upper <- lapply(sorted_list, function(result) result[,upper_index])

  return(list("lower"=lower, "upper"=upper))
}
