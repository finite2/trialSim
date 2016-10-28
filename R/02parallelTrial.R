#' parallelTrial
#'
#' This is a utility function for running simulations in parallel.
#'
#' @param fun The function to run nsim times
#' @param nsim The number of simulations to run
#' @param vars The variables which need to be passes to each cluster
#' @param parallel A logical value to determine if the runs should be made parallel
#'
#' @return
#' This functions returns a list of results for fun. This is identical to a call to \code{\link{lapply}}.
#'
#' @export parallelTrial
parallelTrial = function (fun, nsim, vars, parallel, p) {
  ret <- if (!parallel) {
    lapply(X = seq_len(nsim), FUN = fun)
  } else {
    cores <- parallel::detectCores()
    cl <- parallel::makeCluster(cores, outfile='')


    parallel::clusterEvalQ(cl,expr = {

      library(trialSim)

      NULL
    })

    if(!is.null(p$sourceSetupCode)){
    parallel::clusterCall(cl,function(sourceSetupCode){
      source(sourceSetupCode)
      NULL
    }, sourceSetupCode = p$sourceSetupCode)
    }

    parallel::clusterExport(cl = cl, varlist = vars, envir = parent.frame())
    parallel::clusterExport(cl = cl, varlist = ls(.GlobalEnv))
    res <- parallel::parLapply(cl = cl, X = seq_len(nsim), fun = fun)
    parallel::stopCluster(cl)
    res
  }
  return(ret)
}
