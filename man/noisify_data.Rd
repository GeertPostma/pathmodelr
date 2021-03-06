% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_preprocessing_functions.R
\name{noisify_data}
\alias{noisify_data}
\title{Adds Gaussian noise to data}
\usage{
noisify_data(input_data, noise_stdev = 0.1)
}
\arguments{
\item{input_data}{A matrix containing data. The rows indicate the samples,
the columns indicate the variables.}

\item{noise_stdev}{The standard deviation of the noise}
}
\value{
The input data with added gaussian noise
}
\description{
Adds Gaussian noise to data
}
