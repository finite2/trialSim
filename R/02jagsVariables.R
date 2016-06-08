#' Convinient class for running jags models
#' @import R2jags
#' @exportClass jagsVariables
#' @export jagsVariables
jagsVariables=setClass("jagsVariables", slots = c(model.file = "function", nSample = "numeric", burnin = "numeric", nThin = "numeric", n.chains="numeric", parameters.to.save = "character"))
