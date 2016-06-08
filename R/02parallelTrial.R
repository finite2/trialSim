
#' @export parallelTrial
parallelTrial = function (fun, nsim, vars, parallel) {

  ret <- if (!parallel) {
    lapply(X = seq_len(nsim), FUN = fun)
  } else {
    cores <- parallel::detectCores()
    cl <- parallel::makeCluster(cores, outfile='')
    parallel::clusterEvalQ(cl, fun = {
      library(trialSim)
      NULL
    })
    parallel::clusterExport(cl = cl, varlist = vars, envir = parent.frame())
    parallel::clusterExport(cl = cl, varlist = ls(.GlobalEnv))
    res <- parallel::parLapply(cl = cl, X = seq_len(nsim), fun = fun)
    parallel::stopCluster(cl)
    res
  }
  return(ret)
}
