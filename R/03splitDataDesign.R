
#' @exportClass splitDataDesign
#' @export splitDataDesign
splitDataDesign = setClass( "splitDataDesign", list(
  simBaseline = "simulateBaseline",
  simOutcome = "simualteOutcome"),
  contains = "trialDesign"
)


setMethod("simTrial", signature = c(object = "splitDataDesign"), definition = function( object, nsim = 1L, ...){

  validObject(object)

  .local <- function (object, nsim = 1L, seed = NULL, parallel = FALSE, packages = character(0), ...){

    set.seed(seed)
    simSeeds <- sample(x = seq_len(1e+08), size = nsim)

    ##############################################################
    # function doing all the work
    runSim = function(iterSim) {
      seed = simSeeds[iterSim]
      set.seed(seed)

      params = object@p

      baselineFun = object@simBaseline@fun
      outcomeFun = object@simOutcome@fun
      modelFun = object@model@fun
      decisionFun = object@decision@fun
      data = object@data


      if(dim(data)[1] == 0) {
        # simulate first data
        params$data = data
        data = do.call(baselineFun, params)
        params$data = data
        model = do.call(modelFun, params)

        Decision = params$decision

        # get outcome data if recruiting
        params$data = data
        params$decision = Decision
        data = do.call(outcomeFun, params)
      }

      if(!is.na(data$dose[dim(data)[1]])) {
        # simuate baseline for next patient if not provided
        params$data = data
        data = do.call(baselineFun, params)
      }
      # fit the model at the timepoint the next patient arrives
      params$data = data
      model = do.call(modelFun, params)

      # make decisions as to what happens next
      params$data = data
      params$model = model
      Decision = do.call(decisionFun, params)

      while (Decision@continue) {
        #print(data)
        #print(Decision)

        # get outcome data if recruiting
        params$data = data
        params$decision = Decision
        data = do.call(outcomeFun, params)

        # get baseline data for next patient
        params$data = data
        data = do.call(baselineFun, params)

        # fit the model at the timepoint the next patient arrives
        params$data = data
        model = do.call(modelFun, params)

        # make decisions as to what happens next
        params$data = data
        params$model = model
        Decision = do.call(decisionFun, params)

      }
      return(list(seed = seed ,data = data, stopReason = Decision@stopReason, MTD = model$MTD))
    }
    ####################################################################

    # The below allows for parallelisation
    resultList <- parallelTrial(fun = runSim, nsim = nsim,
                                vars = c("simSeeds", "object"), parallel = parallel)
  }

  object@sims =  .local(object, nsim, ...)

  return(object)
})


setMethod("fitModel", signature = c(object = "splitDataDesign"), definition = function(object, seed = 123) {

  modelFun = object@model@fun
  param = object@p
  data = object@data

  if(!is.na(data$dose[dim(data)[1]])) {

    set.seed(seed)

    baselineFun = object@simBaseline@fun

    # simuate baseline for next patient if not provided
    param$data = data
    data = do.call(baselineFun, param)

    warning("No baseline data provided./nSimulated baseline data for one patient. This may be used.")
  }
  # fit the model at the timepoint the next patient arrives
  param$data = data
  model = do.call(modelFun, param)

  return(model)
})


setMethod("getDecision", signature = c(object = "splitDataDesign"), definition = function(object, seed = 123) {
  set.seed(seed)

  modelFun = object@model@fun
  decisionFun = object@decision@fun
  param = object@p

  data = object@data


  if(!is.na(data$dose[dim(data)[1]])) {

    baselineFun = object@simBaseline@fun

    # simuate baseline for next patient if not provided
    param$data = data
    data = do.call(baselineFun, param)

    warning("No baseline data provided./nSimulated baseline data for one patient. This may be used.")
  }
  # fit the model at the timepoint the next patient arrives
  param$data = data
  model = do.call(modelFun, param)

  # make decisions as to what happens next
  param$data = data
  param$model = model
  Decision = do.call(decisionFun, param)

  return(Decision)
})
