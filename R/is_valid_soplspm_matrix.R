is_valid_soplspm_matrix <- function(connection_matrix){

  #check if square of at least 2-by-2
  if(ncol(connection_matrix) != nrow(connection_matrix) || nrow(connection_matrix) < 2 || ncol(connection_matrix) < 2){
    return(FALSE)
  }

  #check if fully connected lower triangular:
  for(i in 1:nrow(connection_matrix)){
    for(j in 1:ncol(connection_matrix)){
      if(i > j){
        if(connection_matrix[i,j] != 1){
          return(FALSE)
        }
      }
      else{
        if(connection_matrix[i,j] != 0){
          return(FALSE)
        }
      }
    }
  }
  return(TRUE)
}
