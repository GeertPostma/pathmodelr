get_next_nodes <- function(connection_matrix, node_index){
  
  result <- (1:dim(connection_matrix)[1])[connection_matrix[,node_index] == 1]
}