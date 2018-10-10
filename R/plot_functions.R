#' @import ggplot2
#' @import GGally
#' @import sna
#' @import network
plot_variances <- function(model,
                           mode="circle"){

  net <- as.network(as.data.frame(t(round(as.matrix(model$variances_explained), digits=3))),
                    ignore.eval = FALSE,
                    names.eval = "weights",
                    directed = TRUE,
                    matrix_type = "incidence")

  ggnet2(net,
         label = TRUE,
         edge.label = "weights",
         arrow.size = 12,
         arrow.gap = 0.025,
         mode="circle")
}
