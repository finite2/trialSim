

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
#' @seealso
#' \code{\link{experimentalModel}}
#'
#' \code{\link{getData}}, \code{\link{getBaseline}}, \code{\link{getOutcome}}, \code{\link{getTrigger}} \code{\link{getmodel}}, \code{\link{getDecision}}
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
#' @seealso
#' \code{\link{makeDecisions-class}}
#'
#' \code{\link{getData}}, \code{\link{getBaseline}}, \code{\link{getOutcome}}, \code{\link{getTrigger}} \code{\link{getmodel}}, \code{\link{getDecision}}
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
#'
#' @seealso
#' \code{\link{simulateData}}
#'
#' \code{\link{getData}}, \code{\link{getBaseline}}, \code{\link{getOutcome}}, \code{\link{getTrigger}} \code{\link{getmodel}}, \code{\link{getDecision}}
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
#' @seealso
#' \code{\link{simulateBaseline}}
#'
#' \code{\link{getData}}, \code{\link{getBaseline}}, \code{\link{getOutcome}}, \code{\link{getTrigger}} \code{\link{getmodel}}, \code{\link{getDecision}}
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
#' @seealso
#' \code{\link{simulateOutcome}}
#'
#' \code{\link{getData}}, \code{\link{getBaseline}}, \code{\link{getOutcome}}, \code{\link{getTrigger}} \code{\link{getmodel}}, \code{\link{getDecision}}
#'
#' @exportMethod getOutcome
setGeneric("getOutcome", def = function(object, ...){
  standardGeneric("getOutcome")
})

#' getTrigger
#'
#' Runs the object@triggerAnalysis creating a decision class object to determine if the analysis should be run. The data to add to should be stored in object@data.
#'
#' @slot object An object with parent class \code{\link{trialDesign}}
#'
#' @seealso
#' \code{\link{readyForAnalysis}}
#'
#' \code{\link{getData}}, \code{\link{getBaseline}}, \code{\link{getOutcome}}, \code{\link{getTrigger}} \code{\link{getmodel}}, \code{\link{getDecision}}
#'
#' @exportMethod getTrigger
setGeneric("getTrigger", def = function(object, ...){
  standardGeneric("getTrigger")
})

#' @exportMethod summary
#setGeneric("summary", def = function(object, ...){
#  standardGeneric("summary")
#})
