get_all_node_types <- function(connection_matrix){
  
  node_types <- list()
  
  for(i in 1:dim(connection_matrix)[1]){
    node_types[[i]] <- get_node_type(connection_matrix, i)
  }
  
  return(node_types)
}