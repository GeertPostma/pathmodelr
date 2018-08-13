estimator_string_to_function <- function(estimator_name){
  if(tolower(estimator_name) == "pls"){
    return(PLS_estimator)
  }
  else if(tolower(estimator_name) == "pca"){
    return(PCA_estimator)
  }
  else if(tolower(estimator_name) == "full"){
    return(full_estimator)
  } 
  
}