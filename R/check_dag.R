#' Check Cyclicity in a Directed Acyclic Graph
#'
#' Checks whether a given connection matrix is acyclic. The function is
#' recursive and shrinks the search space until a solution is found.
#'
#' This is an internal function called by verify_dag()
#'
#' @param DAG A matrix where the non zero elements what connections exist. The
#'   rows indicate the node where the edge is going to, and the columns
#'   indicates the node where the edge is coming from.
#' @param node_name A string indicating the identifier of the node from which
#'   cyclicity is checked.
#' @param visited_nodes A list of strings indicating all the nodes that have
#'   been visited in the recursive loop.
#' @return A boolean indicating whether a cycle is detected (TRUE), or not
#'   (FALSE)
#'
check_dag <- function(DAG, node_name, visited_nodes = list()){
  visited_nodes <- cbind(visited_nodes, node_name)


  if (sum(DAG) == 0){
    return(TRUE)
  }
  else{
    connected_nodes <- rownames(DAG)[which(DAG[, node_name] != 0, arr.ind = TRUE)]

    if (any(connected_nodes %in% visited_nodes)){ #Exits when cycle is detected)
      return(FALSE)
    }

    for(i in connected_nodes){ #Check all connected_nodes

      numerical_node_number <- which(row.names(DAG) == node_name)
      if (!check_dag(DAG[-numerical_node_number,-numerical_node_number], toString(i), visited_nodes)){
        return(FALSE)
      }
    }
    return(TRUE)

  }

}
