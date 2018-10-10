get_PLS_variances_explained <- function(model){
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
  return(round(variances_explained, digits = 3))
}
