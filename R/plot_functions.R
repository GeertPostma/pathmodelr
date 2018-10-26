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

#Default: show all end nodes, give name
#if input is list of names: plot all
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @importFrom dplyr inner_join
#' @export
plot_variable_effects <- function(model, what_node=NULL){

  #what_node="Product"

  if(is.null(what_node)){
    what_node <- unlist(lapply(model$nodes, function(node) if(node$node_type=="End") node$node_name))
  }

  plots <- list()
  for(study_node in what_node){
    effects <- model$variable_effects$effects[[study_node]]

    var_names <- names(effects)

    plot_df <- data.frame(name=var_names, effects=effects, row.names=NULL, stringsAsFactors=FALSE)

    #get all vars in blocks:
    vars_in_block <- bind_rows(lapply(model$nodes, function(node) data.frame(rep(node$node_name, times=length(colnames(node$X_data))), colnames(node$X_data), stringsAsFactors=FALSE)))

    colnames(vars_in_block) <- c("block", "name")

    plot_df <- inner_join(plot_df, vars_in_block, by = "name")

    plots[[study_node]] <- ggplot(data = plot_df,

      aes(x = reorder(name, 1:length(name)), y = abs(effects), fill = block)) +

      geom_bar(stat = 'identity', position = 'dodge') +

      theme(axis.text.x = element_text(angle = 270)) +

      ylab("Absolute Effect") +

      xlab("Variable name") +

      ggtitle(study_node)


  }
  return(plots)

}

