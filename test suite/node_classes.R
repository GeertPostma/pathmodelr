Node <- R6Class("Node", 
  public =list(
    #Fields
    node_name                = NA_character_,
    X_data                   = NULL,
    is_initialized           = FALSE,
    n_LVs                    = NA_integer_,
    LVs                      = NULL,
    X_loadings               = NULL,
    path_coefficients        = NULL,
    is_estimated             = FALSE,
    
    #Methods
    estimator = NULL,
    
    initialize = function(node_name, X_data, estimator){
      self$node_name      <- node_name
      self$X_data         <- X_data
      self$estimator      <- estimator
      self$is_initialized <- TRUE      
    },
    
    add_estimate = function(n_LVs, LVs, X_loadings, path_coefficients){
      self$n_LVs        <- n_LVs
      self$Lvs          <- LVs
      self$X_loadings   <- X_loadings
      self$coefficients <- path_coefficients
      self$is_estimated <- TRUE
    }
  )
)

StartNode <- R6Class("StartNode", 
  inherit = Node,
  public = list(
    #Fields
    next_nodes = NULL,
    
    
    #Methods
    add_connected_nodes = function(next_nodes){
      self$next_nodes <- next_nodes
    }
  )
  
)

MiddleNode <- R6Class("MiddleNode", 
 inherit = Node,
 public = list(
    #Fields
    previous_nodes = NULL,
    next_nodes = NULL,
                       
    #Methods
    add_connected_nodes = function(next_nodes, previous_nodes){
      self$next_nodes <- next_nodes
      self$previous_nodes <- previous_nodes
    }
  )
                     
)

EndNode <- R6Class("EndNode", 
  inherit = Node,
  public = list(
    #Fields
    previous_nodes = NULL,
    
    #Methods
    add_connected_nodes = function(next_nodes, previous_nodes){
      self$previous_nodes <- previous_nodes
    }
  )
)                  