

#' simTrial
#'
#' Runs trial level simulations
#'
#' @slot object An object with parent class \code{\link{trialDesign}}
#' @slot parallel TRUE/FALSE Whether to run the simulations in parallel or not
#' @slot seed Set the seed at the start of the run
#'
#' @exportMethod simTrial
setGeneric("simTrial", def = function(object, ...){
  standardGeneric("simTrial")
})

#' getModel
#'
#' Fit the model to the data and returns the model. The data to use should be stored in object@data
#'
#' @slot object An object with parent class \code{\link{trialDesign}}
#'
#' @exportMethod getModel
setGeneric("getModel", def = function(object, ...){
  standardGeneric("getModel")
})

#' getDecision
#'
#' Fit the model to the data and return the decision
#'
#' @slot object An object with parent class \code{\link{trialDesign}}
#'
#' @exportMethod getDecision
setGeneric("getDecision", def = function(object, ...){
  standardGeneric("getDecision")
})

#' getData
#'
#' Creates data for the next patient or patients. The data to add to should be stored in object@data
#'
#' @slot object An object with parent class \code{\link{trialDesign}}
#'
#' @exportMethod getData
setGeneric("getData", def = function(object, ...){
  standardGeneric("getData")
})

#' getBaseline
#'
#' Creates baseline data for the next patient. The data to add to should be stored in object@data
#'
#' @slot object An object with parent class \code{\link{trialDesign}}
#'
#' @exportMethod getBaseline
setGeneric("getBaseline", def = function(object, ...){
  standardGeneric("getBaseline")
})

#' getOutcome
#'
#' Creates outcome data for the next patient. The data to add to should be stored in object@data. The decision in object@p$decision is used to determine any special requirements such as inclusion due to biomarkers or trial recruitment paused.
#'
#' @slot object An object with parent class \code{\link{trialDesign}}
#'
#' @exportMethod getOutcome
setGeneric("getOutcome", def = function(object, ...){
  standardGeneric("getOutcome")
})

#' @exportMethod summary
#setGeneric("summary", def = function(object, ...){
#  standardGeneric("summary")
#})
