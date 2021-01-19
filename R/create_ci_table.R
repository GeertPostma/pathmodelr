#' @export
create_variance_effect_ci_table <- function(process_pls_model){

  ci_table <- as.data.frame(process_pls_model$bootstrap_results$variance_effects$mean)

  lower_ci <- process_pls_model$bootstrap_results$variance_effects$ci$lower
  upper_ci <- process_pls_model$bootstrap_results$variance_effects$ci$upper

  for(i in 1:dim(ci_table)[1]){
    for(j in 1:dim(ci_table)[2]){
      ci_table[[i,j]] <- paste("(", toString(round(lower_ci[[i,j]], 3)), " - ", toString(round(upper_ci[[i,j]], 3)), ")", sep="")
    }
  }

  return(ci_table)
}
