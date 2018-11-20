
get_scale_vec <- function(node, block_scale=FALSE, variance_scale=FALSE, rescale=FALSE){

  scale_vec <- rep(1, length(node$variance_explained))

  if(variance_scale){
    if(rescale){
      SSi <- colSums(node$LVs^2)
    }
    else{
      SSi <- colSums(node$previous_LVs^2)
    }

    scale_vec <- scale_vec * sqrt(scale_vec * SSi / node$variance_explained)
  }

  if(block_scale){
    if(rescale){
      scale_vec <- scale_vec * sqrt(sum((node$LVs %*% diag(1/scale_vec,nrow=length(scale_vec)))^2))
    }
    else{
      scale_vec <- scale_vec * sqrt(sum((node$previous_LVs %*% diag(1/scale_vec,nrow=length(scale_vec)))^2))
    }

  }

  return(scale_vec)
}

get_train_test_scale_vec <- function(node, test_indices, block_scale=FALSE, variance_scale=FALSE){
  Y <- node$previous_LVs
  Y_train_temp <- Y[-test_indices, , drop=FALSE]
  Y_test_temp  <- Y[test_indices, , drop=FALSE]

  scale_vec_train <- rep(1, length(node$variance_explained))
  scale_vec_test  <- rep(1, length(node$variance_explained))

  if(variance_scale){
    SSi_train <- colSums(Y_train_temp^2)
    SSi_test  <- colSums(Y_test_temp^2)
    scale_vec_train <- scale_vec_train * sqrt(scale_vec_train * SSi_train / node$variance_explained)
    scale_vec_test  <- scale_vec_test  * sqrt(scale_vec_test  * SSi_test  / node$variance_explained)
  }

  if(block_scale){
    scale_vec_train <- scale_vec_train * sqrt(sum((Y_train_temp %*% diag(1/scale_vec_train,nrow=length(scale_vec_train)))^2))
    scale_vec_test  <- scale_vec_test  * sqrt(sum((Y_test_temp  %*% diag(1/scale_vec_test ,nrow=length(scale_vec_test )))^2))
  }

  return(list("Y_train_temp"=Y_train_temp, "Y_test_temp"=Y_test_temp, "scale_vec_train" = scale_vec_train, "scale_vec_test" = scale_vec_test))
}
