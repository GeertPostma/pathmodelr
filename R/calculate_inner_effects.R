#' @export
calculate_inner_effects <- function(model, scaling_method=NULL){

  variable_effects_on_LV_per_block <- list()
  variable_effects_on_LV <- list()
  variable_effects <- list()

  for(i in seq_along(model$path_effects$total)){

    current_node_name <- names(model$path_effects$total)[[i]]

    variable_effects_on_LV_per_block[[current_node_name]] <- list()

    for(j in seq_along(model$path_effects$total[[i]])){
      previous_node_name <- names(model$path_effects$total[[i]])[[j]]

      variable_effects_on_LV_per_block[[current_node_name]][[previous_node_name]] <- model$nodes[[previous_node_name]]$X_loadings %*% model$path_effects$total[[i]][[j]]

    }

    if(!is.null(j)){

      variable_effects_on_LV[[current_node_name]] <- do.call(rbind, variable_effects_on_LV_per_block[[current_node_name]])

      if(is.null(scaling_method)){
        variable_effects[[current_node_name]] <- rowSums(variable_effects_on_LV[[current_node_name]])
      }
      else if(tolower(scaling_method) == "variance"){
        if(!is.null(model$nodes[[current_node_name]]$variance_explained)){
          variable_effects[[current_node_name]] <- rowSums(variable_effects_on_LV[[current_node_name]] * model$nodes[[current_node_name]]$variance_explained)
        }
        else{
          stop("variance_explained is not an attribute of this node type")
        }
      }
      else{
        stop("No valid scaling method was selected.")
      }
    }
    else{
      variable_effects_on_LV[[current_node_name]] <- NULL
      variable_effects[[current_node_name]] <- NULL
    }

  }

  return(list("effects_on_LV_per_block"=variable_effects_on_LV_per_block, "effects_on_LV" = variable_effects_on_LV,"effects"=variable_effects))
}
