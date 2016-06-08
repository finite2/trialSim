#' @exportClass singleDataDesign
#' @export singleDataDesign
singleDataDesign = setClass( "singleDataDesign", list(
  simualteData = "simualteData"),
  contains = "trialDesign"
)
