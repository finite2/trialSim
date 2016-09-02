#' Convinient class for running jags models
#' @exportClass jagsVariables
#' @export jagsVariables
jagsVariables=setClass("jagsVariables", slots = c(model.file = "function", nSample = "numeric", burnin = "numeric", nThin = "numeric", n.chains="numeric", parameters.to.save = "character"))
