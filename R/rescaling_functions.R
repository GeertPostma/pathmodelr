
get_scale_vec <- function(LVs, variance_explained, block_scale=FALSE, variance_scale=FALSE){

  scale_vec <- rep(1, length(variance_explained))

  if(variance_scale){
    scale_vec <- scale_vec * sqrt(scale_vec * variance_explained)
  }

  if(block_scale){
    SS <- sum((LVs %*% scale_vec)^2)
    scale_vec <- scale_vec / sqrt(SS)
  }

  return(scale_vec)
}

rescale_X_weights <- function(X_weights, scale_vec){
  return(X_weights %*% diag(scale_vec, nrow=length(scale_vec)))
}

rescale_LVs <- function(LVs, scale_vec){
  return(LVs %*% diag(scale_vec, nrow=length(scale_vec)))
}
