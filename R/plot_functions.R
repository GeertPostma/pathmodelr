#' @title Plot inner model
#'
#' @description
#' Plot the inner (structural) model fora path matrix.
#' Adapted from plspm package by Gaston Sanchez.
#'
#' @param x Either a matrix defining an inner model or an
#' object of class \code{"plspm"}.
#' @param colpos Color of arrows for positive path coefficients.
#' @param colneg Color of arrows for negative path coefficients.
#' @param box.prop Length/width ratio of ellipses.
#' @param box.size Size of ellipses.
#' @param box.cex Relative size of text in ellipses.
#' @param box.col fill color of ellipses,
#' @param lcol border color of ellipses.
#' @param box.lwd line width of the box.
#' @param txt.col color of text in ellipses.
#' @param shadow.size Relative size of shadow of label box.
#' @param curve arrow curvature.
#' @param lwd line width of arrow.
#' @param arr.pos Relative position of arrowheads on arrows.
#' @param arr.width arrow width.
#' @param arr.lwd line width of arrow, connecting two different points,
#' (one value, or a matrix with same dimensions as \code{x}).
#' @param cex.txt Relative size of text on arrows.
#' @param show.values should values be shown when \code{x} is a matrix.
#' @param \dots Further arguments passed on to \code{\link{plotmat}}.
#' @note \code{innerplot} uses the function
#' \code{\link{plotmat}} in package \code{diagram}. \cr
#' \url{http://cran.r-project.org/web/packages/diagram/vignettes/diagram.pdf}
#' @importFrom diagram plotmat
#' @export
innerplot <-
  function(x, colpos = "#6890c4BB", colneg = "#f9675dBB",
           box.prop = 0.55, box.size = 0.08, box.cex = 1, box.col = "gray95",
           lcol="gray95", box.lwd = 2, txt.col = "gray50", shadow.size = 0,
           curve = 0, lwd = 3, arr.pos = 0.5, arr.width = 0.2, arr.lwd = 3,
           cex.txt = 0.9, show.values = FALSE,
           ...)
  {
    # =======================================================
    # checking arguments
    # =======================================================
    if (!inherits(x, "plspm") && !inherits(x, "matrix"))
      stop(paste("\nSorry, can't work with an object of class "), class(x))
    # if x is "plspm"
    if (inherits(x, "plspm"))
    {
      # get ingredients
      IDM = x$model$IDM
      lvs = nrow(IDM)
      # matrix of path coefficients
      MPC = x$path_coefs[lvs:1,]
      MPC = MPC[,lvs:1]
      names = rownames(MPC)
      # arrow matrix colors
      AM.col = MPC
      AM.col[MPC < 0] = colneg # negative path coeffs in red
      AM.col[MPC >= 0] = colpos # positive path coeffs in blue
    } else {
      if (nrow(x) != ncol(x))
        stop("\nSorry, the provided matrix is not a square matrix")
      lvs = nrow(x)
      if (is.null(rownames(x))) {
        if (is.null(colnames(x))) {
          rownames(x) = as.character(1:lvs)
        } else {
          rownames(x) = colnames(x)
        }
      } else {
        colnames(x) = rownames(x)
      }
      MPC = x[lvs:1,]
      MPC = MPC[,lvs:1]
      names = rownames(MPC)
      AM.col = MPC
      AM.col[MPC < 0] = colneg # negative path coeffs in red
      AM.col[MPC >= 0] = colpos # positive path coeffs in blue
      if (!show.values) cex.txt = 0
    }
    # check arr.lwd
    if (is.matrix(arr.lwd))
    {
      # Arrow Line Width
      ALW = arr.lwd[lvs:1,]
      ALW = ALW[,lvs:1]
      arr.lwd = ALW
    }

    # plot of inner model (adapted function from plotmat)
    plotmat(round(MPC, 4),     # square coefficient matrix
                     name = names,               # names of elements
                     box.type = "ellipse",       # shape of label box
                     box.size = box.size,        # size of label box
                     box.prop = box.prop,        # length/width ratio of label box
                     box.col = box.col,          # fill color of label box
                     lcol = lcol,                # color of box line
                     box.lwd = box.lwd,          # line width of the box
                     box.cex = box.cex,          # relative size of text in boxes
                     txt.col = txt.col,          # color of text in boxes
                     curve = curve,              # arrow curvature
                     lwd = lwd,                  # line width of arrow
                     cex.txt = cex.txt,          # relative size of arrow text
                     arr.type = "triangle",      # type of arrowhead
                     arr.pos = arr.pos,          # relative pos of arrowhead on arrow line
                     arr.lwd = arr.lwd,          # width of arrow, connecting two points
                     shadow.size = shadow.size,  # relative size of shadow of label box
                     prefix = "",                # added in front of non-zero arrow labels
                     arr.lcol = AM.col,          # color of arrow line
                     arr.col = AM.col,           # color of arrowhead
                     arr.width = arr.width,      # arrow width
                     self.arrpos = pi/2,         # position of the self-arrow
                     ...)
  }
#' Plot the variances explained through the path
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



