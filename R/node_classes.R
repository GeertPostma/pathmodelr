#' @import R6
Node <- R6Class("Node",
  public = list(
    #Fields
    node_name           = NA_character_,
    X_data              = NULL,
    is_initialized      = FALSE,
    is_estimated        = FALSE,
    is_iterative        = TRUE,
    n_LVs               = NA_integer_,
    previous_n_LVs      = NA_integer_,
    LVs                 = NULL,
    previous_LVs        = NULL,
    X_loadings          = NULL,
    preprocessed_X      = NULL,
    error               = NA_real_, #SSE between iterations

    previous_nodes      = NULL,
    next_nodes          = NULL,
    node_type           = NA_character_,

    initializer         = NULL,
    estimator           = NULL,
    local_preprocessor  = NULL,
    global_preprocessor = NULL,

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

      self$error <- 0

      self$preprocess_X()

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

    add_estimate = function(n_LVs, LVs, X_loadings){
      self$n_LVs        <- n_LVs
      self$LVs          <- LVs
      self$X_loadings   <- X_loadings

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

        self$is_estimated <- FALSE
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
      }
    },

    get_paths_to_self = function(){

      paths_to_self <- list()

      for(i in seq_along(self$previous_nodes)){
        node <- self$previous_nodes[[i]]

        paths_to_self[[i]] <- node$get_paths_through_self_to_node(self)

      }

      paths_to_self <- unlist(paths_to_self, recursive=FALSE)

      return(paths_to_self)
    },


    #TODO: Needs thorough testing for non-path-propagating nodes. Do matrix effect multiplications still work?
    get_outgoing_path = function(node){

      return(0)
    },

    finalize = function(){

    }
  )
)

#' PathNode is a template Node class for nodes which propagate effects, such as
#' the PLSNode. Path nodes should implement the get_outgoing_path method
#' @import R6
PathNode <- R6Class("PathNode",
  inherit = Node,
  public = list(

    #Methods
    get_paths_through_self_to_node = function(node){

      outgoing_coefficients <- self$get_outgoing_path(node)

      paths <- list()
      paths[[1]] <- outgoing_coefficients

      for(i in seq_along(self$previous_nodes)){

        incoming_paths <- self$previous_nodes[[i]]$get_paths_through_self_to_node(self)

        for(j in seq_along(incoming_paths)){

          paths[[length(paths)+1]] <- incoming_paths[[j]] %*% outgoing_coefficients
        }

      }

      return(paths)
    }

  )
)

#' @import R6
PLSNode <- R6Class("PLSNode",
  inherit = PathNode,
  public = list(
    #Fields
    Y_loadings = NULL, #Follows same orders as next_nodes

    #Methods
    add_estimate = function(n_LVs, LVs, X_loadings, Y_loadings=NULL){
      self$n_LVs        <- n_LVs
      self$LVs          <- LVs
      self$X_loadings   <- X_loadings
      self$Y_loadings   <- Y_loadings

      if(!is.null(self$previous_LVs)){
        self$error <- calculate_SSE_for_matrices(self$LVs, self$previous_LVs)
      }
      else{
        self$error <- 0
      }

      self$is_estimated <- TRUE
    },

    get_outgoing_path = function(node){

      for(i in seq_along(self$next_nodes)){

        if(self$next_nodes[[i]]$node_name == node$node_name){

          return(t(self$Y_loadings[[i]]))
        }
      }
    }

    finalize = function(){

    }

  )
)