
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
      params$data = object@data

      baselineFun = object@simBaseline@fun
      outcomeFun = object@simOutcome@fun
      triggerFun = object@triggerAnalysis@fun
      modelFun = object@model@fun
      decisionFun = object@decision@fun


      if(dim(params$data)[1] == 0) {
        # simuate baseline for first patient if not data at all
        params$data = do.call(baselineFun, params)
      }

      if(!is.na(params$data$dose[dim(params$data)[1]])) {
        # simuate baseline for next patient if not provided
        params$data = do.call(baselineFun, params)
      }

      while (params$decision@continue) {
        #print(data)
        print(params$decision)

        ###########################################
        # if recruiting now asign arm/dose etc. and get new patient
        if(params$decision@recruiting){
        for(i in 1:params$decision@cohortSize){
          # get outcome data if recruiting
          params$data = do.call(outcomeFun, params)

          # get baseline data for next patient
          params$data = do.call(baselineFun, params)
        }
        } else {
          # get baseline data for next patient (in particular arrival time)
          params$data = do.call(baselineFun, params)
        }

        ###########################################
        # check if ready to analyse
        params$decision = do.call(triggerFun, params)
        if(params$decision@analyse){
        print(params$data)

        # fit the model at the timepoint the next patient arrives
        params$model = do.call(modelFun, params)

        # make decisions as to what happens next
        params$decision = do.call(decisionFun, params)
        }
        ###########################################
      }
      return(list(seed = seed ,data = params$data, decision = params$decision, params = params$model))
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
  set.seed(seed)

  modelFun = object@model@fun
  params = object@p
  params$data = object@data

  if(!is.na(data$dose[dim(data)[1]])) {

    baselineFun = object@simBaseline@fun

    # simuate baseline for next patient if not provided
    params$data = do.call(baselineFun, params)

    warning("No baseline data provided./nSimulated baseline data for one patient. This may be used.")
  }
  # fit the model at the timepoint the next patient arrives
  params$model = do.call(modelFun, params)

  return(params$model)
})


setMethod("getDecision", signature = c(object = "splitDataDesign"), definition = function(object, seed = 123) {
  set.seed(seed)

  modelFun = object@model@fun
  decisionFun = object@decision@fun
  params = object@p

  params$data = object@data


  if(!is.na(data$dose[dim(data)[1]])) {

    baselineFun = object@simBaseline@fun

    # simuate baseline for next patient if not provided
    data = do.call(baselineFun, params)

    warning("No baseline data provided./nSimulated baseline data for one patient. This may be used.")
  }
  # fit the model at the timepoint the next patient arrives
  params$model = do.call(modelFun, params)

  # make decisions as to what happens next
  params$decision = do.call(decisionFun, params)

  return(params$decision)
})
