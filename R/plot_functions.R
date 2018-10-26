#' Plot the variances explained through the path
#' @import ggplot2
#' @import ggnetwork
#' @import network
#' @export
plot_variances <- function(model,
                           layout=layout){

  net <- as.network.matrix(as.data.frame(t(round(as.matrix(model$path_variances_explained), digits=3))),
                    ignore.eval = FALSE,
                    names.eval = "weights",
                    directed = TRUE,
                    matrix_type = "incidence")

  ggnet <- ggnetwork(net, arrow.gap = 0.04, layout="circle")

  ggplot(ggnet, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(arrow = arrow(length = unit(6, "pt"), type="closed"), color = "grey50") +
    geom_edgetext(aes(label = weights), color = "black", fill = "white") +
    geom_nodes(color = "grey", size = 20) +
    geom_nodetext(aes(label=vertex.names)) +
    theme_blank()
}

#' Plot the inner model
#' @import ggplot2
#' @import ggnetwork
#' @import network
#' @export
plot_inner_model <- function(model,
                           layout="circle"){

  net <- as.network.matrix(as.data.frame(t(round(as.matrix(model$path_variances_explained), digits=3))),
                    directed = TRUE,
                    matrix_type = "incidence")

  ggnet <- ggnetwork(net, arrow.gap = 0.04, layout=layout)

  ggplot(ggnet, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_edges(arrow = arrow(length = unit(6, "pt"), type="closed"), color = "grey50") +
    geom_nodes(color = "grey", size = 20) +
    geom_nodetext(aes(label=vertex.names)) +
    theme_blank()
}

#Default: show all end nodes, give name
#if input is list of names: plot all
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @importFrom dplyr inner_join
#' @export
plot_variable_effects <- function(model, what_node=NULL, negative_values="absolute"){

  #what_node="Product"

  if(is.null(what_node)){
    what_node <- unlist(lapply(model$nodes, function(node) if(node$node_type=="End") node$node_name))
  }

  plots <- list()
  for(plot_node in what_node){
    effects <- model$variable_effects$effects[[plot_node]]

    var_names <- names(effects)

    plot_df <- data.frame(name=var_names, effects=effects, row.names=NULL, stringsAsFactors=FALSE)

    #get all vars in blocks:
    vars_in_block <- bind_rows(lapply(model$nodes, function(node) data.frame(rep(node$node_name, times=length(colnames(node$X_data))), colnames(node$X_data), stringsAsFactors=FALSE)))

    colnames(vars_in_block) <- c("block", "name")

    plot_df <- inner_join(plot_df, vars_in_block, by = "name")

    if(tolower(negative_values) == "absolute"){
      plots[[plot_node]] <- ggplot(data = plot_df,

        aes(x = reorder(name, 1:length(name)), y = abs(effects), fill = block)) +

        geom_bar(stat = 'identity', position = 'dodge') +

        theme(axis.text.x = element_text(angle = 270)) +

        ylab("Absolute Effect") +

        xlab("Variable name") +

        ggtitle(plot_node)
    }
    else if(tolower(negative_values)){
      if(to_lower(negative_values) == "negative"){
        plots[[plot_node]] <- ggplot(data = plot_df,

          aes(x = reorder(name, 1:length(name)), y = effects, fill = block)) +

          geom_bar(stat = 'identity', position = 'dodge') +

          theme(axis.text.x = element_text(angle = 270)) +

          ylab("Effect") +

          xlab("Variable name") +

          ggtitle(plot_node)
      }
    }
    else{
      stop("An incorrect value was supplied for the negative_values argument.")
    }
  }
  return(plots)
}

