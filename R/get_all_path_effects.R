#' Calculates and gets all path effects from a path model
#'
#' Calculates individual, direct, indirect, and total effects based on the
#' connecting path effects calculated for the nodes.
#'
# @param model A path_model containing estimated nodes at the model$nodes attribute.
#'
#' @return a list of lists with elements: TODO: needs more specifics on structure of return value.
#'
#'              \code{"individual"=all_individual_effects,} ( list of matrices )
#'
#'              \code{"direct"=all_direct_effects,} ( list of matrices )
#'
#'              \code{"indirect"=all_indirect_effects,} ( list of matrices )
#'
#'              \code{"total"=all_total_effects,} ( list of matrices )
#'
#'              \code{))}
#'
#'
#' @export
get_all_path_effects <- function(model){

  all_individual_effects <- list()
  all_direct_effects <- list()
  all_indirect_effects <- list()
  all_total_effects <- list()

  for(i in seq_along(model$nodes)){
    node <- model$nodes[[i]]

    effects <- node$get_node_path_effects()

    all_individual_effects[[node$node_name]] <- effects$individual_effects
    all_direct_effects[[node$node_name]] <- effects$direct_effects
    all_indirect_effects[[node$node_name]] <- effects$indirect_effects
    all_total_effects[[node$node_name]] <- effects$total_effects
  }

  return(list("individual"=all_individual_effects, "direct"=all_direct_effects, "indirect"=all_indirect_effects, "total"=all_total_effects))

}
