#' @exportClass stageDecision
#'@export stageDecision

stageDecision = setClass("stageDecision", slots = c(stage = "character", part = "character", trail = "character"), contains = "decision")
