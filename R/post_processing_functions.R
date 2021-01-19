# Post processor function for standardizing LVs
#' @export
standardize_LVs <- function(node){

  scaling_settings <- apply(node$LVs, 2, sd)

  return(scaling_settings)
}
