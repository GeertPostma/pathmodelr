# Post processor function for standardizing LVs
standardize_LVs <- function(node){


  if(dim(node$LVs)[2] > 1){
    scaling_settings <- apply(node$LVs, 2, sd)
  }
  else{
    scaling_settings <- sd(node$LVs)
  }
  return(scaling_settings)
}
