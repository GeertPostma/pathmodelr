#' Verifies whether a given connection matrix is a Directed Acyclic Graph.
#' verify_dag wraps check_dag for a recursive Depth First Search for acyclicity.
#'
#' @param DAG A matrix where the non zero elements what connections exist. The rows indicate the node where the edge is going to, and the columns indicates the node where the edge is coming from.
#' @return A list of errors and warnings indicating what conditions of the DAG requirements are not met.
#' @examples
#' verify_dag(t(matrix(c(0,0,0,0,1,0,0,0,1,0,0,0,0,1,1,0),nrow=4, ncol=4)))
#' @export
#'
verify_dag <- function(DAG){
  #Wrapper function to prepare for recursive Depth First Search for acyclicity
  error_list <- c()
  warning_list <- c()

  #check for square matrix
  if ( nrow(DAG) != ncol(DAG)){
    error_list <- c(error_list,c("DAG Connection matrix is not square."))
  }

  #check for unconnected nodes
  for(i in 1:nrow(DAG)){
    if((sum(DAG[,i]) == 0) && (sum(DAG[i,]) == 0)){
      error_list <- c(error_list,paste("Node ", i, " of the DAG connection matrix is unconnected."))
    }
  }

  #check for unidirectional edges
  if (any(DAG & t(DAG))){
    error_list <- c(error_list,"DAG Connection matrix Edges are not unidirectional.")
  }

  #attribute rownames and colnames:
  if (is.null(rownames(DAG)) || is.null(colnames(DAG))){
    warning_list <- c(warning_list, "row and column names were not both assigned to the DAG connection matrix. ")
    rownames(DAG) <- 1:nrow((DAG))
    colnames(DAG) <- 1:nrow((DAG))
  }

  #Start from all nodes without incoming edges
  for (n in rownames(DAG)){
    if ((sum(DAG[n,]) == 0) && (!check_dag(DAG, n))){
      error_list <- c(error_list, "The DAG Connection matrix can't be cyclical.")
    }
  }

  return(c("error_list"=error_list, "warning_list"=warning_list))

}
