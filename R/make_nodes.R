#' Constructs a listenv of nodes according to the specified model in
#' connection_matrix
#'
#'
#' @param blocked_data A list of matrices containing the data that corresponds
#'   to each node/block. The ordering of the outer list should be the same as
#'   that of the \code{connection_matrix}.
#' @param connection_matrix A lower triangular matrix where the non zero
#'   elements what connections exist. The rows indicate the node where the edge
#'   is going to, and the columns indicates the node where the edge is coming
#'   from.
#' @param block_names An ordered list containing the names that should be
#'   assigned to each block. The ordering of the list should be the same as that
#'   of the \code{connection_matrix}. When the argument is not set, names are
#'   extracted from \code{data}, or set to dummy names based on ordering when
#'   \code{data} contains no names.
#' @param estimators A list of functions indicating which estimator should be
#'   used for which node. The ordering of the list should be the same as that of
#'   the \code{connection_matrix}.
#' @param initializers A list of functions indicating which initializer should
#'   be used for which node. The ordering of the list should be the same as that
#'   of the \code{connection_matrix}.
#' @param local_preprocessors A list of preprocessing functions when
#'   unique_node_preprocessing is \code{FALSE}, a list of lists of preprocessing
#'   functions when unique_node_preprocessing is \code{TRUE}. The ordering of
#'   the outer list should be the same as that of the \code{connection_matrix}.
#'   The order of the inner list is only important when the function results
#'   differ when their order of application is changed. The funtions will be
#'   applied from beginning to end. The supplied function is assumed to be
#'   influenced by subsampling, and therefore can be applied on subsets when
#'   (cross-)validating. User-implemented functions must take a Matrix as input,
#'   and return the preprocessed matrix. Implemented functions are:
#'   (block_scale, standardize, and mean_center)
#' @param global_preprocessors A list of preprocessing functions when
#'   unique_node_preprocessing is \code{FALSE}, a list of lists of preprocessing
#'   functions when unique_node_preprocessing is \code{TRUE}. The ordering of
#'   the outer list should be the same as that of the \code{connection_matrix}.
#'   The order of the inner list is only important when the function results
#'   differ when their order of application is changed. The funtions will be
#'   applied from beginning to end. The supplied function must be invariant to
#'   subsampling. User-implemented functions must take a Matrix as input, and
#'   return the preprocessed matrix.
#' @param node_class_types A list of node class types. The ordering of the outer
#'   list should be the same as that of the \code{connection_matrix}.
#' @return A listenv of uninitialized and connected nodes.
#' @include node_classes.R
#' @import listenv
make_nodes <- function(blocked_data, connection_matrix, block_names, estimators, initializers, local_preprocessors, global_preprocessors, node_class_types){

  node_list <- listenv()

  #Initial construction pass
  for(i in 1:length(blocked_data)){

    node_class_type <- node_class_types[[i]]


    node <- node_class_type$new(node_name           = block_names[[i]],
                                X_data              = blocked_data[[i]],
                                estimator           = estimators[[i]],
                                initializer         = initializers[[i]],
                                local_preprocessor  = local_preprocessors[[block_names[[i]]]],
                                global_preprocessor = global_preprocessors[[block_names[[i]]]])

    node_list[[i]] <- node
  }
  #Node connecting pass
  for(i in 1:length(node_list)){

    node <- node_list[[i]]

    next_node_indices     <- get_next_node_indices(connection_matrix, i)
    previous_node_indices <- get_previous_node_indices(connection_matrix, i)

    next_nodes <- node_list[next_node_indices]
    previous_nodes <- node_list[previous_node_indices]

    node$add_connected_nodes(next_nodes=next_nodes, previous_nodes=previous_nodes)
  }

  #Node initialisation pass
  for(i in 1:length(node_list)){
    node_list[[i]]$call_initializer()
  }

   return(node_list)
}
