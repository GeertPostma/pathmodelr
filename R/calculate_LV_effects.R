#' @export
calculate_LV_effects <- function(model){

  node_names <- list()


  #FInd paths through model
  for(i in seq_along(model$nodes)){

    node <- model$nodes[[i]]

    find_paths_to_node(node)

    node_names[[i]] <- node$node_name

  }

  #Construct list of lists for indirect and direct effects between nodes
  direct_LV_effects <- listenv()
  indirect_LV_effects <- listenv()

  for(i in seq_along(model$nodes)){
    direct_LV_effects[[i]] <- listenv()
    indirect_LV_effects[[i]] <- listenv()

    for(j in seq_along(model$nodes)){

      direct_LV_effects[[i]][[j]] <- listenv()
      indirect_LV_effects[[i]][[j]] <- listenv()

    }
    names(direct_LV_effects[[i]]) <- node_names
    names(indirect_LV_effects[[i]]) <- node_names

  }

  names(direct_LV_effects) <- node_names
  names(indirect_LV_effects) <- node_names

  #Calculate effects and fill into model
  for(i in seq_along(model$nodes)){

    node <- model$nodes[[i]]

    effects <- calculate_effect_per_node(node, direct_LV_effects, indirect_LV_effects)

  }


}
