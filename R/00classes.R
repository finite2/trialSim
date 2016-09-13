

#' setupFunction
#'
#' Provides a place to do random setup for each run. Particularly useful for non patient level random variables such as surgeon effect. May also be used to compute things like assurance given a prior belief and coverage given a distribution for data rather than a known effect.
#'
#'
#' @slot fun Does one time random number generation saving results to p for use in the other functions.
#'
#'
#' @exportClass setupFunction
#' @export setupFunction
setupFunction = setClass("setupFunction", list(fun = "function"), prototype = list(fun = function(...){
  return(list(...))
}))


#########################################################################
# Data


#' simulateBaseline
#'
#' A class and generator function to store a function which simulates baseline data one patient at a time and appends it to the current data.
#'
#' @slot fun Does the simulation and appends the new data. This function should allow any number of arguments to be passed to it using \dots. This function must return the data.
#'
#' To reduce the risk of errors we recommend specifying the inputs required without defaults. This way if none are specified in p for the \code{\link{trialDesign-class}} class then it will throw a meaningful error.
#'
#' This function should also be capable of creating the dataframe if provided with an empty data.frame.
#'
#' @exportClass simulateBaseline
#' @export simulateBaseline
simulateBaseline = setClass( "simulateBaseline", list(fun = "function"))

#' simulateOutcome
#'
#' A class and generator function to store a function which simulates outcome data and adds it to the current data. This function may rely on the current \code{\link{Decision}} object.
#'
#' @slot fun Does the simulation and adds the new data. This function should allow any number of arguments to be passed to it using \dots. This function must return the data.
#'
#' To reduce the risk of errors we recommend specifying the inputs required without defaults. This way if none are specified in p for the \code{\link{trialDesign}} class then it will throw a meaningful error.
#'
#'
#' @exportClass simulateOutcome
#' @export simulateOutcome
simulateOutcome = setClass( "simulateOutcome", list(fun = "function"))

#' simulateData
#'
#' A class and generator function to store a function which simulates baseline and outcome data and adds it to the current data.
#'
#' @slot fun Does the simulation and adds the new data. This function should allow any number of arguments to be passed to it using \dots. This function must return the data.
#'
#' To reduce the risk of errors we recommend specifying the inputs required without defaults. This way if none are specified in p for the \code{\link{trialDesign}} class then it will throw a meaningful error.
#'
#' This function should also be capable of creating the dataframe if provided with an empty data.frame.
#'
#' @exportClass simulateData
#' @export simulateData
simulateData = setClass( "simulateData", list(fun = "function"))

#########################################################################
#' readyForAnalysis
#'
#' A class and generator function to store a function which decides if it is time to run the model. Take the data and an object of \code{\link{Decision}} class and returns a \code{\link{Decision}} class object.
#'
#' @slot fun Decides if it is time to run the model. This function should allow any number of arguments to be passed to it using \dots. This function must return a \code{\link{Decision}} class object.
#'
#' To reduce the risk of errors we recommend specifying the inputs required without defaults. This way if none are specified in p for the \code{\link{trialDesign}} class then it will throw a meaningful error.
#'
#'
#' @exportClass readyForAnalysis
#' @export readyForAnalysis
readyForAnalysis = setClass("readyForAnalysis", list(fun = "function"))


#########################################################################
#' experimentalModel
#'
#' A class and generator function to store a function which runs the model and returns detailed results of the run. The more results it returns the easier it will be to make different decisions. If this is purely a rule based approach then the default for fun can be used which returns NULL. Then the rules can be written entirely in the \code{\link{makeDecisions}} component.
#'
#' @slot fun Fits the model. This function should allow any number of arguments to be passed to it using \dots. This function can return anything but the more useful information the better.
#'
#' To reduce the risk of errors we recommend specifying the inputs required without defaults. This way if none are specified in p for the \code{\link{trialDesign}} class then it will throw a meaningful error.
#'
#'
#' @exportClass experimentalModel
#' @export experimentalModel
experimentalModel = setClass( "experimentalModel", list(fun = "function"), prototype = function(...){ return(NULL)})

#########################################################################
#' makeDecisions
#'
#' A class and generator function to store a function which takes the data and the model and makes decisions as to what should happend next. Returns an object of class \code{\link{decision}}
#'
#' @slot fun Makes decisions based on the data and fitted model. This function should allow any number of arguments to be passed to it using \dots. This function can return anything but the more useful information the better.
#'
#' To reduce the risk of errors we recommend specifying the inputs required without defaults. This way if none are specified in p for the \code{\link{trialDesign}} class then it will throw a meaningful error.
#'
#'
#' @exportClass makeDecisions
#' @export makeDecisions
makeDecisions = setClass( "makeDecisions", list(fun = "function"))


#########################################################################
#' decision
#'
#' This class is used to determine what is going on in the trial at any given time. It is returned from the \code{\link{readyForAnalysis}} and \code{\link{makeDecisions}} functions within the \code{\link{trialDesign}} class.
#'
#' @slot analyse TRUE/FALSE whether the trials should fit the model
#' @slot continue TRUE/FALSE whether the trial should continue
#' @slot cohort A number specifying the current cohort
#' @slot recruiting TRUE/FALSE whether new patients can be recruited to the trial at present
#' @slot stopReason A string detailing why the trial is stopping
#' @slot cohortSize The number of patients to recruit before running the \code{\link{readyForAnalysis}} function
#'
#' @exportClass decision
#' @export decision
decision = setClass("decision", list(analyse = "logical", continue = "logical", cohort = "numeric", recruiting = "logical", stopReason = "character", cohortSize = "integer"), prototype = list(analyse = TRUE, continue = TRUE, recruiting = TRUE, cohort = 1L))

#########################################################################
#' trialDesign
#'
#' This is the parent class for all trial designs.
#'
#' @slot triggerAnalysis An object of class \code{\link{readyForAnalysis}} which makes decisions about when tofit the model and pause recruitment
#' @slot model An object of class \code{\link{experimentalModel}} which fits the model and returns a model object
#' @slot decision An object of class \code{\link{makeDecisions}} which makes decisions based on the model and data
#' @slot data A data.frame of initial data when simulating part way through a trial
#' @slot seed An integer used to seed the simulations
#' @slot nSim Number of simulations to run
#' @slot sims A slot to store a list of the simulated trials run
#' @slot p A list of all of the required parameters for the running functions
#'
#' @exportClass trialDesign
#' @export trialDesign
trialDesign = setClass( "trialDesign", list(
  setupFun = "setupFunction",
  triggerAnalysis = "readyForAnalysis",
  model = "experimentalModel",
  decision = "makeDecisions",
  data = "data.frame",
  seed = "integer",
  nSim = "integer",
  sims = "list",
  p = "ANY"
))
