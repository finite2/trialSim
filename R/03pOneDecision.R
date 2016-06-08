#' @exportClass pOneDecision
#' @export pOneDecision
pOneDecision = setClass("pOneDecision", list(dose = "ANY"), contains = "decision", prototype = list(continue= TRUE, recruiting = TRUE, stopReason = "", cohort = 1))
