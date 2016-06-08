

#' @exportMethod simTrial
setGeneric("simTrial", def = function(object, nsim = 1L, ...){
  warning("This function is to run trials simulation. This function expects an S4 class object with pre-defined method.")
})

#' @exportMethod fitModel
setGeneric("fitModel", def = function(object, nsim = 1L, ...){
  warning("This function is to fit a model. This function expects an S4 class object with pre-defined methods.")
})

#' @exportMethod getDecision
setGeneric("getDecision", def = function(object, nsim = 1L, ...){
  warning("This function is to fit a model and make a decision. This function expects an S4 class object with pre-defined methods.")
})

