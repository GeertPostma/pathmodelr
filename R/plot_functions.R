#' Plot the variances explained through the path
#' @import ggplot2
#' @import GGally
#' @import sna
#' @import network
#' @export
plot_variances <- function(model,
                           mode="circle"){

  net <- as.network.matrix(as.data.frame(t(round(as.matrix(model$path_variances_explained), digits=3))),
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
         mode=mode)
}

#' Plot the inner model
#' @import ggplot2
#' @import GGally
#' @import sna
#' @import network
#' @export
plot_inner_model <- function(model,
                           mode="circle"){

  net <- as.network.matrix(as.data.frame(model$connection_matrix),
                    directed = TRUE,
                    matrix_type = "incidence")

  ggnet2(net,
         label = TRUE,
         arrow.size = 10,
         arrow.gap = 0.04,
         size = 20,
         mode=mode)
}

plot_variable_effects <- function(model){






  ggplot(data = model,

              aes(x = name, y = abs(weight), fill = block)) +

    geom_bar(stat = 'identity', position = 'dodge') +

    # add title

    # ggtitle(paste("Barchart of", attr(model, "method"), attr(model, "what"), attr(model, "data"))) +

    # rotate x-axis names

    theme(axis.text.x = element_text(angle = 90)) +

    ylab("Absolute Effect")
}

