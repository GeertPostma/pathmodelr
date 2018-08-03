initializer_string_to_function <- function(initializer_name){
  
  if(tolower(initializer_name) == "pca"){
    return(PCA_initializer)
  }
  else if(tolower(initializer_name) == "full"){
    return(full_initializer)
  } 
}