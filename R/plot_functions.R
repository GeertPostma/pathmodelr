#' Plot the variances explained through the path
#' @importFrom plspm innerplot
#' @export
plot_inner_model_variances <- function(model){

  innerplot(as.matrix(model$path_variances_explained), show.values=TRUE)
}

#' Plot the inner model
#' @import ggplot2
#' @export
plot_inner_model <- function(model){

  innerplot(as.matrix(model$path_variances_explained))
}

#Default: show all end nodes, give name
#if input is list of names: plot all
#' @import ggplot2
#' @importFrom dplyr bind_rows
#' @importFrom dplyr inner_join
#' @export
plot_variable_effects <- function(model, what_node=NULL, negative_values="negative", show_bootstrap=FALSE){

  if(is.null(what_node)){
    what_node <- unlist(lapply(model$nodes, function(node) if(node$node_type=="End") node$node_name))
  }

  if(show_bootstrap && !(tolower(negative_values) == "negative")){
    stop("Showing of bootstrap results is only possible when showing non-absolute effects.")
  }

  plots <- list()
  for(plot_node in what_node){
    effects <- model$inner_effects$effects[[plot_node]]

    var_names <- names(effects)

    if(show_bootstrap){
      lower_bound_effects <- model$bootstrap_results$inner_effects$ci$lower[[plot_node]]
      upper_bound_effects <- model$bootstrap_results$inner_effects$ci$upper[[plot_node]]

      plot_df <- data.frame(name=var_names, effects=effects, upper_bound_effects=upper_bound_effects, lower_bound_effects=lower_bound_effects, row.names=NULL, stringsAsFactors=FALSE)
    }
    else{
      plot_df <- data.frame(name=var_names, effects=effects, row.names=NULL, stringsAsFactors=FALSE)
    }


    #get all vars in blocks:
    vars_in_block <- bind_rows(lapply(model$nodes, function(node) data.frame(rep(node$node_name, times=length(colnames(node$X_data))), colnames(node$X_data), stringsAsFactors=FALSE)))

    colnames(vars_in_block) <- c("block", "name")

    plot_df <- inner_join(plot_df, vars_in_block, by = "name")

    if(tolower(negative_values) == "absolute"){
      plots[[plot_node]] <- ggplot(data = plot_df,

        aes(x = reorder(name, 1:length(name)), y = abs(effects), fill = block)) +

        geom_bar(stat = 'identity', position = 'dodge', color="black") +

        theme(axis.text.x = element_text(angle = 60, hjust=1)) +

        ylab("Absolute Effect") +

        xlab("Variable name") +

        ggtitle(plot_node)
    }
    else if(tolower(negative_values) == "negative"){
      plots[[plot_node]] <- ggplot(data = plot_df,

        aes(x = reorder(name, 1:length(name)), y = effects, fill = block)) +

        geom_bar(stat = 'identity', position = 'dodge', color="black") +

        theme(axis.text.x = element_text(angle = 60, hjust=1)) +

        ylab("Effect") +

        xlab("Variable name") +

        ggtitle(plot_node)
    }
    else{
      stop("An incorrect value was supplied for the negative_values argument.")
    }

    if(show_bootstrap){
      plots[[plot_node]] <- plots[[plot_node]] + geom_errorbar(aes(ymin=lower_bound_effects, ymax=upper_bound_effects), width=0.5)
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

        theme(axis.text.x = element_text(angle = 60, hjust=1)) +

        ylab("Absolute Effect") +

        xlab("Variable name")
    }
    else if(tolower(negative_values) == "negative"){
      plots[[plot_node]] <- ggplot(data = plot_df,

        aes(x = reorder(name, 1:length(name)), y = effects, fill = block)) +

        geom_bar(stat = 'identity', position = 'dodge') +

        theme(axis.text.x = element_text(angle = 60, hjust=1)) +

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

          theme(axis.text.x = element_text(angle = 60, hjust=1)) +

          ylab("Absolute Effect") +

          xlab("Variable name") +

          ggtitle(plot_node)
      }
      else if(tolower(negative_values) == "negative"){
          plots[[plot_node]] <- ggplot(data = plot_df,

                                       aes(x = reorder(name, 1:length(name)), y = effects, fill = block)) +

            geom_bar(stat = 'identity', position = 'dodge') +

            theme(axis.text.x = element_text(angle = 60, hjust=1)) +

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



