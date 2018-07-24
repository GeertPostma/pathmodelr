node <- setClass(
  "node",
  
  slots = c(node_type                = 'character',
            node_name                = 'character',
            downstream_node_indices  = 'integer',
            upstream_node_indices    = 'integer',
            X_data                   = 'list',
            is_initialized           = 'logical',
            is_estimated             = 'logical',
            n_LVs                    = 'numeric',
            LVs                      = 'list',
            X_loadings               = 'list',
            coefficients             = 'list'
           ),
  
  prototype = list(is_initialized = FALSE, is_estimated = FALSE)
  
)

setGeneric("is_initialized", function(object) {
  standardGeneric("is_initialized")
})

setMethod("is_initialized", signature(object = "node"), function(object) {
  object@is_initialized
})

setGeneric("is_estimated", function(object) {
  standardGeneric("is_estimated")
})

setMethod("is_estimated", signature(object = "node"), function(object) {
  object@is_estimated
})