#' @import ggplot2
#' @import GGally
#' @import sna
#' @importFrom network as.network
#' @export
plot_variances <- function(model,
                           mode="circle"){

  net <- as.network(as.data.frame(t(round(as.matrix(model$path_variances_explained), digits=3))),
                    ignore.eval = FALSE,
                    names.eval = "weights",
                    directed = TRUE,
                    matrix_type = "incidence")

  ggnet2(net,
         label = TRUE,
         edge.label = "weights",
         arrow.size = 10,
         arrow.gap = 0.06,
         size = 30,
         mode=mode)
}

#' @import ggplot2
#' @import GGally
#' @import sna
#' @importFrom network as.network
#' @export
plot_inner_model <- function(model,
                           mode="circle"){

  net <- as.network(as.data.frame(model$connection_matrix),
                    directed = TRUE,
                    matrix_type = "incidence")

  ggnet2(net,
         label = TRUE,
         arrow.size = 10,
         arrow.gap = 0.06,
         size = 30,
         mode=mode)
}
