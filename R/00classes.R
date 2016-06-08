
#########################################################################
# Data

#' @exportClass simulateBaseline
#' @export simulateBaseline
simulateBaseline = setClass( "simulateBaseline", list(fun = "function"))


#' @exportClass simualteOutcome
#' @export simualteOutcome
simualteOutcome = setClass( "simualteOutcome", list(fun = "function"))


#' @exportClass simualteData
#' @export simualteData
simualteData = setClass( "simualteData", list(fun = "function"))

#########################################################################
# readyForAnalysis

#' @exportClass readyForAnalysis
#' @export readyForAnalysis
readyForAnalysis = setClass( "readyForAnalysis", list(fun = "function"))


#########################################################################
# Model

#' @exportClass experimentalModel
#' @export experimentalModel
experimentalModel = setClass( "experimentalModel", list(fun = "function"))

#########################################################################
# Decision

#' @exportClass makeDecisions
#' @export makeDecisions
makeDecisions = setClass( "makeDecisions", list(fun = "function"))

#' @exportClass decision
#' @export decision
decision = setClass("decision", list(analyse = "logical", continue = "logical", cohort = "numeric", recruiting = "logical", stopReason = "character", cohortSize = "integer"), prototype = list(analyse = TRUE, continue = TRUE, recruiting = TRUE, cohort = 1L))

#########################################################################
# Compiled design

#' @exportClass trialDesign
#' @export trialDesign
trialDesign = setClass( "trialDesign", list(
  triggerAnalysis = "readyForAnalysis",
  model = "experimentalModel",
  decision = "makeDecisions",
  data = "data.frame",
  sims = "list",
  p = "list"
))
