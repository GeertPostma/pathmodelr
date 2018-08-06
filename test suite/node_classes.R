Node <- R6Class("Node", 
  public =list(
    #Fields
    node_name                = NA_character_,
    X_data                   = NULL,
    is_initialized           = FALSE,
    is_estimated             = FALSE,
    is_iterative             = TRUE,
    n_LVs                    = NA_integer_,
    previous_n_LVs           = NA_integer_,
    LVs                      = NULL,
    previous_LVs             = NULL,
    X_loadings               = NULL,
    
    previous_nodes = NULL,
    next_nodes = NULL,
    node_type = NA_character_,

    initializer = NULL,
    estimator = NULL,
    
    #Methods
    initialize = function(node_name, X_data, estimator, initializer){
      self$node_name      <- node_name
      self$X_data         <- X_data
      self$estimator      <- estimator
      self$initializer    <- initializer
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
    }
  )
)