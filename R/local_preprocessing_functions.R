#' Scales a block of data to unit variance
#'
#' @param input_data A matrix containing data. The rows indicate the samples,
#'   the columns indicate the variables.
#' @param settings A list with a single named element "block_variance". When
#'   block_variance is set, the supplied value is used for scaling, otherwise a value of 1 is used.
#' @return A named list with "preprocessed_data": the scaled input_data matrix,
#'   and "settings", which is equal to the settings input parameter when set,
#'   and otherwise contains the calculated and used block_std.
#' @export
#' @importFrom stats sd
block_scale <- function(input_data, settings=list("block_variance"=1)){
  block_variance <- settings$block_variance

  scale_vec <- rep(sqrt(block_variance)/sqrt(dim(input_data)[2]), dim(input_data)[2])

  block_scaled_data <- input_data %*% diag(scale_vec,nrow=length(scale_vec))

  settings <- list("block_variance"=block_variance)

  return(list("preprocessed_data"=block_scaled_data, "settings"=settings))
}

#' Mean-centers each variable in a block of data and then scale each variable to
#' unit variance
#'
#' @param input_data A matrix containing data. The rows indicate the samples,
#'   the columns indicate the variables.
#' @param settings A list with a two named elements "column_stds" and
#'   "column_means". When "column_stds" is set, the supplied vector is used for
#'   scaling, otherwise it is calculated. When "column_means" is set, the
#'   supplied vector is used for mean substraction. Both vectors should have a
#'   length equal to the number of variables. All named elements can be set, and
#'   optionally left out, seperately.
#' @return A named list with "preprocessed_data": the scaled input_data matrix,
#'   and "settings", which is equal to the settings input parameter when set,
#'   and otherwise contains the calculated and used column_stds and
#'   column_means.
#' @export
#' @importFrom stats sd
standardize <- function(input_data, settings=list("column_means"=NULL, "column_stds"=NULL)){
  column_means <- settings$column_means
  column_stds <- settings$column_stds

  mc_list <- mean_center(input_data, list("column_means"=column_means))
  centered_data <- mc_list$preprocessed_data
  column_means <- mc_list$settings$column_means

  if(is.null(column_stds)){
    column_stds <- apply(centered_data, 2, sd)
  }

  scaled_data <- centered_data %*% diag(1/column_stds, nrow=length(column_stds))

  settings <- list("column_means"=column_means, "column_stds"=column_stds)

  return(list("preprocessed_data"=scaled_data,"settings"=settings))
}

#' Mean-centers each variable in a block of data
#'
#' @param input_data A matrix containing data. The rows indicate the samples,
#'   the columns indicate the variables.
#' @param settings A list with a one named elements "column_means". When
#'   "column_means" is set, the supplied vector is used for mean substraction.
#'   The vectors should have a length equal to the number of variables.
#' @return A named list with "preprocessed_data": the mean-centered input_data
#'   matrix, and "settings", which is equal to the settings input parameter when
#'   set, and otherwise contains the calculated column_means.
#' @export
mean_center <- function(input_data, settings=list("column_means"=NULL)){
  column_means <- settings$column_means

  if(is.null(column_means)){
    column_means <- colMeans(input_data)
  }
  centered_data <- input_data - rep(1, nrow(input_data)) %*% t(column_means)

  settings <- list("column_means"=column_means)

  return(list("preprocessed_data"=centered_data,"settings"=settings))
}


#' @export
normalize <- function(input_data, settings=list("column_means"=NULL, "column_scaling_factor"=NULL)){
  column_means <- settings$column_means
  column_scaling_factor <- settings$column_scaling_factor

  mc_list <- mean_center(input_data, list("column_means"=column_means))
  centered_data <- mc_list$preprocessed_data
  column_means <- mc_list$settings$column_means

  if(is.null(column_scaling_factor)){
    column_scaling_factor <- apply(centered_data, 2, function(x) sqrt(sum(x^2)))
  }

  normalized_data <- centered_data %*% diag(1/column_scaling_factor, nrow=length(column_scaling_factor))

  settings <- list("column_means"=column_means, "column_scaling_factor"=column_scaling_factor)

  return(list("preprocessed_data"=normalized_data,"settings"=settings))
}
