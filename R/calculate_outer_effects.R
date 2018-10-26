#' @export
get_outer_effects <- function(model){
  outer_effects_on_LV <- list()
  outer_effects <- list()

  for(i in seq_along(model$nodes)){
    node <- model$nodes[[i]]

    outer_effects_on_LV[[node$node_name]] <- node$X_loadings

    outer_effects[[node$node_name]] <- rowSums(outer_effects_on_LV[[i]] * node$variance_explained) / sum(node$variance_explained)

  }

  return(list("outer_effects"=outer_effects, "outer_effects_on_LV"=outer_effects_on_LV))
}
