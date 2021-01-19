#' @export
combine_target_LVs <- function(node){

  #get dimensions pass:
  total_LVs <- 0
  Y_indices <- list()
  for(i in seq_along(node$next_nodes)){
    Y_indices[[i]] <- (total_LVs+1):(total_LVs + node$next_nodes[[i]]$n_LVs)
    names(Y_indices)[[i]] <- node$next_nodes[[i]]$node_name
    total_LVs <- total_LVs + node$next_nodes[[i]]$n_LVs
  }

  #Construct matrix pass:
  Y <- matrix(0, nrow=dim(node$LVs)[1], ncol=total_LVs)

  for(i in seq_along(node$next_nodes)){
    Y[,Y_indices[[i]]] <- node$next_nodes[[i]]$LVs
  }

  return(list("Y"=Y, "Y_indices"=Y_indices))

}

#' @export
combine_target_manifest_variables <- function(node){

  #get dimensions pass:
  total_manifests <- 0
  Y_indices <- list()
  for(i in seq_along(node$next_nodes)){
    Y_indices[[i]] <- (total_manifests+1):(total_manifests + dim(node$next_nodes[[i]]$preprocessed_X)[2])
    names(Y_indices)[[i]] <- node$next_nodes[[i]]$node_name
    total_manifests <- total_manifests + dim(node$next_nodes[[i]]$preprocessed_X)[2]
  }

  #Construct matrix pass:
  Y <- matrix(0, nrow=dim(node$preprocessed_X)[1], ncol=total_manifests)

  for(i in seq_along(node$next_nodes)){
    Y[,Y_indices[[i]]] <- node$next_nodes[[i]]$preprocessed_X
  }

  return(list("Y"=Y, "Y_indices"=Y_indices))

}
#' @export
combine_previous_LVs <- function(node){

  #get dimensions pass:
  total_LVs <- 0
  X_indices <- list()
  for(i in seq_along(node$previous_nodes)){
    X_indices[[i]] <- (total_LVs+1):(total_LVs + node$previous_nodes[[i]]$n_LVs)
    names(X_indices)[[i]] <- node$previous_nodes[[i]]$node_name
    total_LVs <- total_LVs + node$previous_nodes[[i]]$n_LVs
  }

  #Construct matrix pass:
  X <- matrix(0, nrow=dim(node$LVs)[1], ncol=total_LVs)

  for(i in seq_along(node$previous_nodes)){
    X[,X_indices[[i]]] <- node$previous_nodes[[i]]$LVs
  }

  return(list("X"=X, "X_indices"=X_indices))

}
#' @export
combine_previous_manifest_variables <- function(node){

  #get dimensions pass:
  total_manifests <- 0
  X_indices <- list()
  for(i in seq_along(node$previous_nodes)){
    X_indices[[i]] <- (total_manifests+1):(total_manifests + dim(node$previous_nodes[[i]]$preprocessed_X)[2])
    names(X_indices)[[i]] <- node$previous_nodes[[i]]$node_name
    total_manifests <- total_manifests + dim(node$previous_nodes[[i]]$preprocessed_X)[2]
  }

  #Construct matrix pass:
  X <- matrix(0, nrow=dim(node$preprocessed_X)[1], ncol=total_manifests)

  for(i in seq_along(node$previous_nodes)){
    X[,X_indices[[i]]] <- node$previous_nodes[[i]]$preprocessed_X
  }

  return(list("X"=X, "X_indices"=X_indices))

}
