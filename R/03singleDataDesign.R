#' @exportClass singleDataDesign
#' @export singleDataDesign
singleDataDesign = setClass( "singleDataDesign", list(
  simualteData = "simualteData"),
  contains = "trialDesign"
)



setMethod("simTrial", signature = c(object = "singleDataDesign"), definition = function( object, nsim = 1L, ...){

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

      dataFun = object@dataBaseline@fun
      triggerFun = object@triggerAnalysis@fun
      modelFun = object@model@fun
      decisionFun = object@decision@fun


      while (params$decision@continue) {
        #print(data)
        print(params$decision)


        # get data for next patient
        params$data = do.call(dataFun, params)

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


setMethod("fitModel", signature = c(object = "singleDataDesign"), definition = function(object, seed = 123) {

  modelFun = object@model@fun
  params = object@p
  params$data = object@data

  # fit the model at the timepoint the next patient arrives
  params$model = do.call(modelFun, params)

  return(params$model)
})


setMethod("getDecision", signature = c(object = "singleDataDesign"), definition = function(object, seed = 123) {
  set.seed(seed)

  modelFun = object@model@fun
  decisionFun = object@decision@fun
  params = object@p
  params$data = object@data

  # fit the model at the timepoint the next patient arrives
  params$model = do.call(modelFun, params)

  # make decisions as to what happens next
  params$decision = do.call(decisionFun, params)

  return(params$decision)
})
