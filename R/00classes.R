
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
decision = setClass("decision", list(continue = "logical", recruiting = "logical", stopReason = "character"))

#########################################################################
# Compiled design

#' @exportClass trialDesign
#' @export trialDesign
trialDesign = setClass( "trialDesign", list(
  model = "experimentalModel",
  decision = "makeDecisions",
  data = "data.frame",
  sims = "list",
  p = "list"
))
