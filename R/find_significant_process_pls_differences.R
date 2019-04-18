#' @export
find_significant_process_pls_differences <- function(first_model, second_model){
  higher_in_first_model <- list()
  lower_in_first_model <- list()

  first_lower_ci <- first_model$bootstrap_results$variance_effects$ci$lower
  first_upper_ci <- first_model$bootstrap_results$variance_effects$ci$upper

  second_lower_ci <- second_model$bootstrap_results$variance_effects$ci$lower
  second_upper_ci <- second_model$bootstrap_results$variance_effects$ci$upper

  for(i in 1:dim(first_lower_ci)[1]){
    for(j in 1:dim(first_lower_ci)[2]){
      if(first_lower_ci[[i,j]] > second_upper_ci[[i,j]]){
        higher_in_first_model <- c(higher_in_first_model, paste(rownames(first_lower_ci)[[i]], ": ", colnames(first_lower_ci)[[j]], sep=""))
      }
      else if(second_lower_ci[[i,j]] > first_upper_ci[[i,j]]){
        lower_in_first_model <- c(lower_in_first_model, paste(rownames(first_lower_ci)[[i]], ": ", colnames(first_lower_ci)[[j]], sep=""))
      }
    }
  }


  return(list("higher_in_first_model"=higher_in_first_model, "lower_in_first_model"=lower_in_first_model))
}
