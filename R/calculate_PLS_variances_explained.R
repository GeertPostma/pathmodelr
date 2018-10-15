#' Gets the variances explained from predictor LVs through direct effects, to
#' target LVs
#'
#' Variance explained is calculated by looking at the ratio between the Sum of
#' Squares of the difference between Y and predicted Y through the direct
#' effects, and the Sum of Squares of Y. This ratio is subtracted from 1 to
#' yield the variance explained between 0 and 1. Note that negative values are
#' possible if more variance is introduced by prediction. This indicates that a
#' certain effect is not predictive, and one should consider to remove the
#' connection in the model definition.
#' @param model A path_model model estimated using the process_pls wrapper.
#' @return a matrix, the same shape as the connection matrix, where the non zero
#'   elements indicate the explained variance from one node to another. The
#'   matrix is lower triangular.
#' @export
calculate_PLS_variances_explained <- function(model){
  variances_explained <- model$connection_matrix

  for(i in seq_along(model$effects$direct)){

    current_node_name <- names(model$effects$direct)[[i]]
    for(j in seq_along(model$effects$direct[[i]])){
      previous_node_name <- names(model$effects$direct[[i]])[[j]]

      Q_t <- model$effects$direct[[i]][[j]]
      X_LV <-  model$nodes[[previous_node_name]]$LVs
      Y <- model$nodes[[current_node_name]]$LVs

      variances_explained[[current_node_name, previous_node_name]] <- 1 - (sum((Y - X_LV %*% Q_t)^2) / sum(Y^2))
    }
  }
  return(variances_explained)
}
