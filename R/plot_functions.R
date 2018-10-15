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
         arrow.gap = 0.04,
         size = 20,
         mode="circle")
}