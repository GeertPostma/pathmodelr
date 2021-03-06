% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_all_path_effects.R
\name{get_all_path_effects}
\alias{get_all_path_effects}
\title{Calculates and gets all path effects from a path model}
\usage{
get_all_path_effects(model)
}
\value{
a list of lists with elements: TODO: needs more specifics on structure of return value.

             \code{"individual"=all_individual_effects,} ( list of matrices )

             \code{"direct"=all_direct_effects,} ( list of matrices )

             \code{"indirect"=all_indirect_effects,} ( list of matrices )

             \code{"total"=all_total_effects,} ( list of matrices )

             \code{))}
}
\description{
Calculates individual, direct, indirect, and total effects based on the
connecting path effects calculated for the nodes.
}
