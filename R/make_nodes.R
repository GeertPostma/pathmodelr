# @include Node.R
#' @import listenv
make_nodes <- function(blocked_data, connection_matrix, block_names, estimators, initializers, local_preprocessors, global_preprocessors, node_types){

  node_list <- listenv()

  #Initial construction pass
  for(i in 1:length(blocked_data)){

    node_type <- node_types[[i]]


    node <- Node$new(node_name   = block_names[[i]],
                     X_data      = blocked_data[[i]],
                     estimator   = estimators[[i]],
                     initializer = initializers[[i]],
                     local_preprocessor = local_preprocessors[[i]],
                     global_preprocessor = global_preprocessors[[i]])


    node_list[[i]] <- node
  }
  #Node connecting pass
  for(i in 1:length(node_list)){

    node <- node_list[[i]]

    next_node_indices     <- get_next_nodes(connection_matrix, i)
    previous_node_indices <- get_previous_nodes(connection_matrix, i)

    next_nodes <- node_list[next_node_indices]
    previous_nodes <- node_list[previous_node_indices]

    node$add_connected_nodes(next_nodes=next_nodes, previous_nodes=previous_nodes)

  }

   return(node_list)
}
