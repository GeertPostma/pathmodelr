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
#' @param scaling A string indicating the options for the scaling of the
#'   variance explained. The options are: "numerical", which indicates the
#'   numerical variance explained from one block to another according to: var =
#'   1 - SS(Y - LV * Qt) / SS(Y) "partial_variance", where in 1 - SS(Y - LV \*
#'   Qt) / SS(Y); {Y - LV \* Qt} and {Y} are rescaled over their columns by how
#'   much variance of the manifest variable variance the LVs in a block explain
#'   each. "variance", where in 1 - SS(Y - LV \* Qt) / SS(Y); {Y - LV \* Qt} is
#'   rescaled over its columns by how much variance of the manifest variable
#'   variance the LVs in a block explain each.
#' @return a matrix, the same shape as the connection matrix, where the non zero
#'   elements indicate the explained variance from one node to another. The
#'   matrix is lower triangular.
#' @export
calculate_PLS_variances_explained <- function(model, scaling="partial_variance"){
  variances_explained <- model$connection_matrix

  for(i in seq_along(model$path_effects$direct)){

    current_node_name <- names(model$path_effects$direct)[[i]]
    for(j in seq_along(model$path_effects$direct[[i]])){
      previous_node_name <- names(model$path_effects$direct[[i]])[[j]]

      Q_t <- model$path_effects$direct[[i]][[j]]
      X_LV <-  model$nodes[[previous_node_name]]$LVs
      Y <- model$nodes[[current_node_name]]$LVs
      Y_pred <- X_LV %*% Q_t

      if(tolower(scaling) == "numerical"){
        diff <- Y - Y_pred
        variances_explained[[current_node_name, previous_node_name]] <- 1 - (sum((diff)^2) / sum(Y^2))
      }
      else if(tolower(scaling) == "partial_variance"){
        variances_per_LV <- model$nodes[[current_node_name]]$variance_explained
        scaling_vec <- sqrt(variances_per_LV) #take the square root to acquire stdev
        diff <- Y - Y_pred
        diff <- diff * scaling_vec
        Y <- Y * scaling_vec
        variances_explained[[current_node_name, previous_node_name]] <- 1 - (sum((diff)^2) / sum(Y^2))
      }
      else if(tolower(scaling) == "variance"){
        variances_per_LV <- model$nodes[[current_node_name]]$variance_explained
        total_variance_in_LVs <- sum(variances_per_LV)
        scaling_vec <- sqrt(variances_per_LV) #take the square root to acquire stdev
        diff <- Y - Y_pred
        diff <- diff * scaling_vec
        Y <- Y * scaling_vec
        variances_explained[[current_node_name, previous_node_name]] <- total_variance_in_LVs - (sum((diff)^2) / sum(Y^2)) * total_variance_in_LVs
      }
      else{
        stop("An invalid scaling option was provided.")
      }

    }
  }
  return(variances_explained)
}
