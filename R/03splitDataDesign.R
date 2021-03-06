
#' @exportClass splitDataDesign
#' @export splitDataDesign
splitDataDesign = setClass( "splitDataDesign", list(
  simBaseline = "simulateBaseline",
  simOutcome = "simulateOutcome"),
  contains = "trialDesign"
)


setMethod("simTrial", signature = c(object = "splitDataDesign"), definition = function( object, nSim = NULL, ...){

  validObject(object)

  if(!is.null(nSim)) {
    object@nSim = as.integer(nSim)
  }

  .local <- function (object, nsim = 1L, parallel = FALSE, ...){

    if(length(object@seed) > 0){
      set.seed(object@seed)
    }
    simSeeds <- sample(x = seq_len(1e+08), size = nsim)
    ##############################################################
    # function doing all the work
    runSim = function(iterSim) {
      seed = simSeeds[iterSim]
      set.seed(seed)

      params = do.call(object@setupFun@fun,object@p)
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

      while (params$decision@continue) {
        # print(data)
        # print(params$decision)

        ###########################################
        # if recruiting now asign arm/dose etc. and get new patient
        if(params$decision@recruiting){

          # get outcome data if recruiting
          params$data = do.call(outcomeFun, params)
          # get baseline data for next patient (in particular arrival time)
          params$data = do.call(baselineFun, params)

        } else {
          # get baseline data for next patient (in particular arrival time)
          params$data = do.call(baselineFun, params)
        }

        ###########################################
        # check if ready to analyse
        params$decision = do.call(triggerFun, params)
        if(params$decision@analyse){
        # print(params$data)
        # fit the model at the timepoint the next patient arrives
        params$model = do.call(modelFun, params)
        # make decisions as to what happens next
        params$decision = do.call(decisionFun, params)
        }

        ###########################################
      }
      return(list(seed = seed ,data = params$data, decision = params$decision))
    }
    ####################################################################
    # The below allows for parallelisation
    resultList <- parallelTrial(fun = runSim, nsim = nsim, vars = c("simSeeds", "object"), parallel = parallel, p = object@p)
  }

  object@sims =  .local(object, object@nSim, ...)
  return(object)
})

setMethod("getBaseline", signature = c(object = "splitDataDesign"), definition = function(object) {

  validObject(object)

  if(length(object@seed) > 0){
    set.seed(object@seed)
  }

  params = object@p
  params$data = object@data
  baseFun = object@simBaseline@fun

  # fit the model at the timepoint the next patient arrives
  params$data = do.call(baseFun, params)

  return(params$data)
})

setMethod("getOutcome", signature = c(object = "splitDataDesign"), definition = function(object) {

  validObject(object)

  if(length(object@seed) > 0){
    set.seed(object@seed)
  }

  params = object@p
  params$data = object@data
  outcomeFun = object@simOutcome@fun

  # fit the model at the timepoint the next patient arrives
  params$data = do.call(outcomeFun, params)

  return(params$data)
})
