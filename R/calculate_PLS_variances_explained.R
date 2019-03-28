#' Gets the variances explained from predictor LVs through direct effects, to
#' target LVs for an enire path model.
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
calculate_PLS_variances_explained <- function(model, scaling="numerical"){
  variances_explained <- model$connection_matrix

  for(i in seq_along(model$path_effects$direct)){

    current_node_name <- names(model$path_effects$direct)[[i]]

    Y <- model$nodes[[current_node_name]]$LVs

    Y_pred <- matrix(0, nrow=nrow(Y), ncol=ncol(Y))

    E_pred_part <- array(dim=list(nrow(Y), ncol(Y), length(model$path_effects$direct[[i]])))
    Y_pred_part <- array(dim=list(nrow(Y), ncol(Y), length(model$path_effects$direct[[i]])))

    Y_pred_part_sum <- 0

    for(j in seq_along(model$path_effects$direct[[i]])){

      previous_node_name <- names(model$path_effects$direct[[i]])[[j]]

      B <- model$path_effects$direct[[i]][[j]]
      X_LV <-  model$nodes[[previous_node_name]]$LVs

      Y_pred_part[,,j] <- X_LV %*% B

      Y_pred <- Y_pred + Y_pred_part[,,j]

      E_pred_part[,,j] <- Y - Y_pred_part[,,j]

      Y_pred_part_sum <- Y_pred_part_sum + sum(Y_pred_part[,,j]^2)

    }

    E_pred <- Y - Y_pred

    for(j in seq_along(model$path_effects$direct[[i]])){
      previous_node_name <- names(model$path_effects$direct[[i]])[[j]]

      #Distribution of variance over predictors is incorrect.
      variances_explained[[current_node_name, previous_node_name]] <- (1 - (sum((E_pred)^2) / sum(Y^2))) * (sum(Y_pred_part[,,j]^2) / Y_pred_part_sum)


    }
  }
  return(variances_explained)
}
