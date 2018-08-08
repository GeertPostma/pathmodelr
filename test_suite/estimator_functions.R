#All estimators must call add_estimate!

#Also updates same level and next level nodes
PLS_estimator <- function(node){ 
  #TODO: Add cross-validation n_LVs selection
  
  n_LVs <- 2
  
  #final result step:
  combined_and_masked <- combine_and_mask(node)
  
  X <- combined_and_masked$X
  Y <- combined_and_masked$Y
  covariance_mask <- combined_and_masked$covariance_mask
  cols_per_X_node <- combined_and_masked$cols_per_X_node
  same_level_nodes <- combined_and_masked$same_level_nodes
  
  X_loadings <- SIMPLS(X,Y, max_n_comp=n_LVs, minimal=TRUE, covariance_mask=covariance_mask)$X_loadings

  for(i in seq_along(same_level_nodes)){
    update_node <- same_level_nodes[[i]]
    node_cols <- cols_per_X_node[[i]]
    
    node_loadings <- X_loadings[node_cols,]
    node_LVs <- update_node$preprocessed_X %*% node_loadings
    
    update_node$add_estimate(n_LVs, node_LVs, node_loadings)
  }
  
}

PCA_estimator <- function(node){ #Simple PCA estimation
  
  rank = dim(node$preprocessed_X)[2] #TODO: Change to meaningful number based on Heuristic, bootstrapping, or cross validation
  
  PCA_object <- prcomp(node$preprocessed_X, scale. = FALSE, center = FALSE, rank = rank)
  
  LVs <- PCA_object$x
  n_LVs <- rank
  X_loadings <- PCA_object$rotation
  
  node$add_estimate(n_LVs, LVs, X_loadings)
}

full_estimator <- function(node){ #Simple Full estimation (generally used for end-nodes)

  LVs <- node$X_data
  n_LVs <- dim(node$preprocessed_X)[2]
  X_loadings <- diag(dim(node$preprocessed_X)[2])
  
  node$add_estimate(n_LVs, LVs, X_loadings)
}