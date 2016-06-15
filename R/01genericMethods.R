

#' simTrial
#'
#' Runs trial level simulations
#'
#' @slot object An object with parent class /code{/link{trialDesign}}
#' @slot parallel TRUE/FALSE Whether to run the simulations in parallel or not
#' @slot seed Set the seed at the start of the run
#'
#' @exportMethod simTrial
setGeneric("simTrial", def = function(object, ...){
  standardGeneric("simTrial")
})

#' fitModel
#'
#' Fit the model to the data and return the model
#'
#' @slot object An object with parent class /code{/link{trialDesign}}
#'
#' @exportMethod fitModel
setGeneric("fitModel", def = function(object, ...){
  standardGeneric("fitModel")
})

#' getDecision
#'
#' Fit the model to the data and return the decision
#'
#' @slot object An object with parent class /code{/link{trialDesign}}
#'
#' @exportMethod getDecision
setGeneric("getDecision", def = function(object, ...){
  standardGeneric("getDecision")
})

#' @exportMethod summary
#setGeneric("summary", def = function(object, ...){
#  standardGeneric("summary")
#})
