% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_nodes.R
\name{make_nodes}
\alias{make_nodes}
\title{Constructs a listenv of nodes according to the specified model in
connection_matrix}
\usage{
make_nodes(
  blocked_data,
  connection_matrix,
  block_names,
  estimators,
  initializers,
  local_preprocessors,
  global_preprocessors,
  node_class_types
)
}
\arguments{
\item{blocked_data}{A list of matrices containing the data that corresponds
to each node/block. The ordering of the outer list should be the same as
that of the \code{connection_matrix}.}

\item{connection_matrix}{A lower triangular matrix where the non zero
elements what connections exist. The rows indicate the node where the edge
is going to, and the columns indicates the node where the edge is coming
from.}

\item{block_names}{An ordered list containing the names that should be
assigned to each block. The ordering of the list should be the same as that
of the \code{connection_matrix}. When the argument is not set, names are
extracted from \code{data}, or set to dummy names based on ordering when
\code{data} contains no names.}

\item{estimators}{A list of functions indicating which estimator should be
used for which node. The ordering of the list should be the same as that of
the \code{connection_matrix}.}

\item{initializers}{A list of functions indicating which initializer should
be used for which node. The ordering of the list should be the same as that
of the \code{connection_matrix}.}

\item{local_preprocessors}{A list of preprocessing functions when
unique_node_preprocessing is \code{FALSE}, a list of lists of preprocessing
functions when unique_node_preprocessing is \code{TRUE}. The ordering of
the outer list should be the same as that of the \code{connection_matrix}.
The order of the inner list is only important when the function results
differ when their order of application is changed. The funtions will be
applied from beginning to end. The supplied function is assumed to be
influenced by subsampling, and therefore can be applied on subsets when
(cross-)validating. User-implemented functions must take a Matrix as input,
and return the preprocessed matrix. Implemented functions are:
(block_scale, standardize, and mean_center)}

\item{global_preprocessors}{A list of preprocessing functions when
unique_node_preprocessing is \code{FALSE}, a list of lists of preprocessing
functions when unique_node_preprocessing is \code{TRUE}. The ordering of
the outer list should be the same as that of the \code{connection_matrix}.
The order of the inner list is only important when the function results
differ when their order of application is changed. The funtions will be
applied from beginning to end. The supplied function must be invariant to
subsampling. User-implemented functions must take a Matrix as input, and
return the preprocessed matrix.}

\item{node_class_types}{A list of node class types. The ordering of the outer
list should be the same as that of the \code{connection_matrix}.}
}
\value{
A listenv of uninitialized and connected nodes.
}
\description{
Constructs a listenv of nodes according to the specified model in
connection_matrix
}
