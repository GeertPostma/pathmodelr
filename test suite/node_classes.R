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
    next_node_indices = NA_integer_,
    
    
    #Methods
    initialize = function(node_name, next_node_indices, X_data){
      self$node_name             <- node_name
      self$next_node_indices <- next_node_indices
      self$X_data                <- X_data
      self$is_initialized        <- TRUE
    }
  )
  
)

MiddleNode <- R6Class("MiddleNode", 
 inherit = Node,
 public = list(
    #Fields
    previous_node_indices = NA_integer_,
    next_node_indices = NA_integer_,
                       
    #Methods
    initialize = function(node_name, next_node_indices, previous_node_indices, X_data){
    self$node_name               <- node_name
    self$next_node_indices       <- next_node_indices
    self$previous_node_indices   <- previous_node_indices
    self$X_data                  <- X_data
    self$is_initialized          <- TRUE
    }
  )
                     
)

EndNode <- R6Class("EndNode", 
  inherit = Node,
  public = list(
    #Fields
    previous_node_indices = NA_integer_,
    
    #Methods
    initialize = function(node_name, previous_node_indices, X_data){
      self$node_name               <- node_name
      self$previous_node_indices   <- previous_node_indices
      self$X_data                  <- X_data
      self$is_initialized          <- TRUE
    }
  )
)                  