#' @import R6
DataNode <- R6Class("DataNode",
  public = list(
    #Fields
    node_name                = NA_character_,
    X_data                   = NULL,
    preprocessed_X           = NULL,
    is_initialized           = FALSE,

    previous_nodes           = NULL,
    next_nodes               = NULL,
    node_type                = NA_character_,

    initializer              = NULL,
    estimator                = NULL,
    local_preprocessor       = NULL,
    global_preprocessor      = NULL,

    #Methods
    initialize = function(node_name, X_data, initializer, local_preprocessor, global_preprocessor){
      self$node_name           <- node_name

      for(preprocessor in global_preprocessor){
        X_data <- preprocessor(X_data)
      }
      self$X_data              <- X_data
      self$estimator           <- estimator
      self$initializer         <- initializer
      self$local_preprocessor  <- local_preprocessor
      self$global_preprocessor <- global_preprocessor

      self$preprocess_X()

    },

    call_initializer = function(){

      self$initializer(self)
      self$is_initialized <- TRUE
    },

    add_connected_nodes = function(next_nodes, previous_nodes){
      self$next_nodes <- next_nodes
      self$previous_nodes <- previous_nodes


      if(length(self$next_nodes) != 0 && length(self$previous_nodes) != 0){#Middle
        self$node_type <- "Middle"
      }
      else if(length(self$next_nodes) == 0 && length(self$previous_nodes) != 0){#End
        self$node_type <- "End"
      }
      else if(length(self$next_nodes) != 0 && length(self$previous_nodes) == 0){#Start
        self$node_type <- "Start"
      }
      else { #unconnected
        stop("The node is unconnected. This should have been predetected by the input checker")
      }
    },

    preprocess_train_test = function(test_indices){

      train_data <- as.matrix(self$X_data[-test_indices, ])
      test_data <- as.matrix(self$X_data[test_indices, ])

      for(preprocessor in self$local_preprocessor){

        preprocessed_train <- preprocessor(train_data)
        train_data <- preprocessed_train$preprocessed_data
        settings <- preprocessed_train$settings

        preprocessed_test <- preprocessor(test_data, settings=settings)
        test_data <- preprocessed_test$preprocessed_data
      }

      return(list("train_data"=train_data, "test_data"=test_data))
    },

    preprocess_X = function(){

      self$preprocessed_X <- as.matrix(self$X_data)

      for(preprocessor in self$local_preprocessor){

        self$preprocessed_X <- preprocessor(self$preprocessed_X)$preprocessed_data
        colnames(self$preprocessed_X) <- colnames(self$X_data)
      }
    },

    get_paths_to_self = function(){

      path_coefficients_to_self <- list()
      path_names_to_self <- list()

      for(i in seq_along(self$previous_nodes)){
        node <- self$previous_nodes[[i]]


        paths_to_self <- node$get_paths_through_self_to_node(self)
        path_coefficients_to_self[[i]] <- paths_to_self$path_coefficients
        path_names_to_self[[i]] <- paths_to_self$path_names

      }

      path_coefficients_to_self <- unlist(path_coefficients_to_self, recursive=FALSE)
      path_names_to_self <- unlist(path_names_to_self, recursive=FALSE)

      if(!is.null(path_coefficients_to_self)){
        names(path_coefficients_to_self) <- lapply(lapply(path_names_to_self, c, self$node_name), paste, collapse=" -> ")
      }

      return(list("path_names"=path_names_to_self, "path_coefficients"=path_coefficients_to_self))
    },

    get_node_path_effects = function(){

      individual_effects <- self$get_paths_to_self()
      direct_effects <- list()
      indirect_effects <- list()
      total_effects <- list()

      direct_effects <- individual_effects$path_coefficients[lapply(individual_effects$path_names, length)==1]
      names(direct_effects) <- individual_effects$path_names[lapply(individual_effects$path_names, length)==1]

      for(preceding_block_name in unique(unlist(individual_effects$path_names))){

        total_effect <- Reduce('+', individual_effects$path_coefficients[lapply(individual_effects$path_names, "[[", 1) == preceding_block_name])

        total_effects[[preceding_block_name]] <- total_effect

        if(is.null(direct_effects[[preceding_block_name]])){
          indirect_effects[[preceding_block_name]] <- total_effect
        }
        else{
          indirect_effects[[preceding_block_name]] <- total_effect - direct_effects[[preceding_block_name]]
        }

      }

      return(list("individual_effects"=individual_effects, "direct_effects"=direct_effects, "indirect_effects"=indirect_effects, "total_effects"=total_effects))

    },


    #TODO: Needs thorough testing for non-path-propagating nodes. Do matrix effect multiplications still work?
    get_outgoing_path_to_node = function(node){

      return(matrix(0, nrow=1, ncol=1))
    }
  )
)



#' @import R6
LVNode <- R6Class("LVNode",
inherit = DataNode,

  public = list(
    #Fields
    n_LVs                    = NA_integer_,
    previous_n_LVs           = NA_integer_,
    LVs                      = NULL,
    previous_LVs             = NULL,
    X_loadings               = NULL,

    variance_explained       = NA_real_,

    estimator                = NULL,
    is_estimated             = FALSE,


    #Methods
    initialize = function(node_name, X_data, estimator, initializer, local_preprocessor, global_preprocessor){
      self$node_name           <- node_name

      for(preprocessor in global_preprocessor){
        X_data <- preprocessor(X_data)
      }
      self$X_data              <- X_data
      self$estimator           <- estimator
      self$initializer         <- initializer
      self$local_preprocessor  <- local_preprocessor
      self$global_preprocessor <- global_preprocessor

      self$preprocess_X()

    },

    add_estimate = function(n_LVs, LVs, X_loadings, variance_explained=NULL){
      self$n_LVs                <- n_LVs
      self$LVs                  <- LVs
      self$X_loadings           <- X_loadings
      rownames(self$X_loadings) <- colnames(self$X_data)
      self$variance_explained   <- variance_explained

      if(!is.null(self$previous_LVs)){
        self$error <- calculate_SSE_for_matrices(self$LVs, self$previous_LVs)
      }
      else{
        self$error <- 0
      }

      self$is_estimated <- TRUE
    },

    estimate = function(){

      if(!self$is_estimated){
        self$estimator(self)
      }
    },

    prepare_next_estimation = function(){

      if(self$is_iterative){
        self$previous_LVs <- self$LVs
        self$LVs <- NULL

        self$previous_n_LVs <- self$n_LVs
        self$n_LVs <- NA_integer_

        self$iteration <- self$iteration + 1

        self$is_estimated <- FALSE
      }
    }
  )
)

#' PathNode is a template Node class for nodes which propagate effects, such as
#' the PLSNode. Path nodes should implement the get_paths_through_self_to_node method
#' @import R6
PathNode <- R6Class("PathNode",
  inherit = LVNode,
  public = list(

    #Methods
    get_paths_through_self_to_node = function(node){

      outgoing_coefficients <- self$get_outgoing_path_to_node(node)

      path_coefficients <- list()
      path_names <- list()
      path_coefficients[[1]] <- outgoing_coefficients
      path_names[[1]] <- self$node_name

      for(i in seq_along(self$previous_nodes)){

        incoming_paths <- self$previous_nodes[[i]]$get_paths_through_self_to_node(self)
        incoming_path_coefficients <- incoming_paths[["path_coefficients"]]
        incoming_path_names <- incoming_paths[["path_names"]]

        for(j in seq_along(incoming_path_coefficients)){

          path_coefficients[[length(path_coefficients)+1]] <- incoming_path_coefficients[[j]] %*% outgoing_coefficients
          path_names[[length(path_names)+1]] <- c(incoming_path_names[[j]], self$node_name)
        }

      }
      paths <- list("path_coefficients"=path_coefficients, "path_names"=path_names)
      return(paths)
    }

  )
)

#' @import R6
PLSNode <- R6Class("PLSNode",
  inherit = PathNode,
  public = list(
    #Fields
    path_coefficients = list(),

    #Methods
    add_estimate = function(n_LVs, LVs, X_loadings, variance_explained=NULL){
      self$n_LVs                <- n_LVs
      self$LVs                  <- LVs
      self$X_loadings           <- X_loadings
      rownames(self$X_loadings) <- colnames(self$X_data)
      self$variance_explained   <- variance_explained

      if(!is.null(self$previous_LVs)){
        self$error <- calculate_SSE_for_matrices(self$LVs, self$previous_LVs)
      }
      else{
        self$error <- 0
      }

      self$is_estimated <- TRUE
    },

    update_coefficients_loadings_and_LVs = function(LVs, X_loadings, path_coefficients=NULL){
      self$LVs               <- LVs
      self$X_loadings        <- X_loadings
      if(!is.null(path_coefficients)){
        self$path_coefficients <- path_coefficients
      }

    },

    add_path_coefficients = function(path_coefficients, next_node_name){
      self$path_coefficients[[length(self$path_coefficients)+1]] <- path_coefficients
      names(self$path_coefficients)[[length(self$path_coefficients)]] <- next_node_name
    },

    get_outgoing_path_to_node = function(node){

      for(i in seq_along(self$next_nodes)){

        if(self$next_nodes[[i]]$node_name == node$node_name){

          return(self$path_coefficients[[node$node_name]])
        }
      }
    }
  )
)
