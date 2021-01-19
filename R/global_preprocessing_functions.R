#' Adds Gaussian noise to data
#'
#' @param input_data A matrix containing data. The rows indicate the samples,
#'   the columns indicate the variables.
#' @param noise_stdev The standard deviation of the noise
#' @return The input data with added gaussian noise
#' @export
#' @importFrom MASS mvrnorm
noisify_data <- function(input_data, noise_stdev=0.1){

  noise <- matrix(mvrnorm(nrow(input_data)*ncol(input_data), mu=0, Sigma=noise_stdev), nrow=nrow(input_data), ncol=ncol(input_data))
  noisified_data <- input_data + noise

  return(noisified_data)
}

