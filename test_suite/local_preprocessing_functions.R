#local preprocessing functions
#use general settings structure to allow for easy calling on training and test sets


block_scale <- function(data, settings=list("block_std")){
  block_std <- settings$block_std
  
  if(is.null(block_std)){
    block_std <- sd(data)
  }
  
  block_scaled_data <- data / matrix(block_std, nrow=nrow(data), ncol=ncol(data))
  
  settings <- list("block_std"=block_std)
  
  return(list("preprocessed_data"=block_scaled_data, "settings"=settings))
}

standardize <- function(data, settings=list("column_means"=NULL, "column_stds"=NULL)){
  column_means <- settings$column_means
  column_stds <- settings$column_stds
  
  mc_list <- mean_center(data, list("column_means"=column_means))
  centered_data <- mc_list$preprocessed_data
  column_means <- mc_list$settings$column_means
  
  if(is.null(column_stds)){
    column_stds <- apply(centered_data, 2, sd)
  }
  
  scaled_data <- centered_data / rep(1, nrow(centered_data)) %*% t(column_stds)
  
  settings <- list("column_means"=column_means, "column_stds"=column_stds)
  
  return(list("preprocessed_data"=scaled_data,"settings"=settings))
}

mean_center <- function(data, settings=list("column_means"=NULL)){
  column_means <- settings$column_means
  
  if(is.null(column_means)){
    column_means <- colMeans(data)
  }
  centered_data <- data - rep(1, nrow(data)) %*% t(column_means)
  
  settings <- list("column_means"=column_means)
  
  return(list("preprocessed_data"=centered_data,"settings"=settings))
}