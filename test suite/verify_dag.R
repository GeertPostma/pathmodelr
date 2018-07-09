verify_dag <- function(DAG){
  #Wrapper function to prepare for recursive DFS for acyclicity
  
  
  #check for square matrix
  if ( nrow(DAG) != ncol(DAG)){
    stop("DAG matrix is not square.")
  }
  
  #check for unconnected nodes
  for(i in 1:nrow(DAG)){
    if((sum(DAG[,i]) == 0) && (sum(DAG[i,]) == 0)){
      stop(paste("Node ", i, " is unconnected."))
    }
  }
  
  #check for unidirectional edges
  if (any(DAG & t(DAG))){
    stop("Edges are not unidirectional.")
  }
  
  #attribute rownames and colnames:
  if (is.null(rownames(DAG)) || is.null(colnames(DAG))){
    warning("row and column names were not both assigned to the DAG. Now using numerical naming scheme.")
    rownames(DAG) <- 1:nrow((DAG))
    colnames(DAG) <- 1:nrow((DAG))
  }
  
  #Start from all nodes without incoming edges
  for (n in rownames(DAG)){
    if ((sum(DAG[n,]) == 0) && (!check_dag(DAG, n))){
      return(FALSE)
    }
  }
  
  return(TRUE)
  
}