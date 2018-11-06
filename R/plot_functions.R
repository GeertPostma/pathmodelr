#' Plot the variances explained through the path
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
plot_inner_effects <- function(model, what_node=NULL, negative_values="absolute"){

  #what_node="Product"

  if(is.null(what_node)){
    what_node <- unlist(lapply(model$nodes, function(node) if(node$node_type=="End") node$node_name))
  }

  plots <- list()
  for(plot_node in what_node){
    effects <- model$inner_effects$effects[[plot_node]]

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
    else if(tolower(negative_values) == "negative"){
      plots[[plot_node]] <- ggplot(data = plot_df,

        aes(x = reorder(name, 1:length(name)), y = effects, fill = block)) +

        geom_bar(stat = 'identity', position = 'dodge') +

        theme(axis.text.x = element_text(angle = 270)) +

        ylab("Effect") +

        xlab("Variable name") +

        ggtitle(plot_node)
    }
    else{
      stop("An incorrect value was supplied for the negative_values argument.")
    }
  }
  return(plots)
}


#' If combine is true: plot all outer model weights in a single plot
#' Default plots
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @export
plot_outer_effects <- function(model, what_node=NULL, combine=TRUE, negative_values="absolute"){

  if(is.null(what_node)){
    what_node <- unlist(lapply(model$nodes, function(node) if(node$node_type != "End")node$node_name))
  }
  plot_dfs <- list()

  for(plot_node in what_node){
    effects <- model$outer_effects$outer_effects[[plot_node]]

    var_names <- names(effects)

    plot_dfs[[plot_node]] <- data.frame(name=var_names, effects=effects, block=rep(plot_node, times=length(effects)), row.names=NULL, stringsAsFactors=FALSE)

  }

  plots <- list()

  if(combine){

    plot_df <- bind_rows(plot_dfs)

    if(tolower(negative_values) == "absolute"){
      plots <- ggplot(data = plot_df,

        aes(x = reorder(name, 1:length(name)), y = abs(effects), fill = block)) +

        geom_bar(stat = 'identity', position = 'dodge') +

        theme(axis.text.x = element_text(angle = 270)) +

        ylab("Absolute Effect") +

        xlab("Variable name")
    }
    else if(tolower(negative_values) == "negative"){
      plots[[plot_node]] <- ggplot(data = plot_df,

        aes(x = reorder(name, 1:length(name)), y = effects, fill = block)) +

        geom_bar(stat = 'identity', position = 'dodge') +

        theme(axis.text.x = element_text(angle = 270)) +

        ylab("Effect") +

        xlab("Variable name")

    }
    else{
      stop("An incorrect value was supplied for the negative_values argument.")
    }
  }
  else{
    for(plot_node in what_node){

      plot_df <- plot_dfs[[plot_node]]

      if(tolower(negative_values) == "absolute"){
        plots[[plot_node]] <- ggplot(data = plot_df,

          aes(x = reorder(name, 1:length(name)), y = abs(effects), fill = block)) +

          geom_bar(stat = 'identity', position = 'dodge') +

          theme(axis.text.x = element_text(angle = 270)) +

          ylab("Absolute Effect") +

          xlab("Variable name") +

          ggtitle(plot_node)
      }
      else if(tolower(negative_values) == "negative"){
          plots[[plot_node]] <- ggplot(data = plot_df,

                                       aes(x = reorder(name, 1:length(name)), y = effects, fill = block)) +

            geom_bar(stat = 'identity', position = 'dodge') +

            theme(axis.text.x = element_text(angle = 270)) +

            ylab("Effect") +

            xlab("Variable name") +

            ggtitle(plot_node)
      }
      else{
        stop("An incorrect value was supplied for the negative_values argument.")
      }
    }
  }

  return(plots)
}



