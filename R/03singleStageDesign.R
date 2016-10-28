#' @exportClass singleStageDesign
#' @export singleStageDesign
singleStageDesign = setClass( "singleStageDesign", list(
  simData = "simulateData"),
  contains = "trialDesign"
)



setMethod("simTrial", signature = c(object = "singleStageDesign"), definition = function(object, nSim = NULL, ...){

  validObject(object)

  if(!is.null(nSim)) {
    object@nSim = as.integer(nSim)
  }

  .local <- function (object, nsim = 1L, seed = NULL, parallel = FALSE, packages = character(0), ...){

    set.seed(seed)
    simSeeds <- sample(x = seq_len(1e+08), size = nsim)

    ##############################################################
    # function doing all the work
    runSim = function(iterSim) {
      seed = simSeeds[iterSim]
      set.seed(seed)

      params = do.call(object@setupFun@fun,object@p)
      params$data = object@data

      dataFun = object@simData@fun
      modelFun = object@model@fun
      decisionFun = object@decision@fun

      # get all data
      params$data = do.call(dataFun, params)

      # fit the model at the timepoint the next patient arrives
      params$model = do.call(modelFun, params)

      # make decisions as to what happens next
      params$decision = do.call(decisionFun, params)

      return(list(seed = seed, data = params$data, decision = params$decision))
    }
    ####################################################################

    # The below allows for parallelisation
    resultList <- parallelTrial(fun = runSim, nsim = nsim, vars = c("simSeeds", "object"), parallel = parallel, p = object@p)
  }

  object@sims =  .local(object, object@nSim, ...)

  return(object)
})


setMethod("getData", signature = c(object = "singleStageDesign"), definition = function(object) {

  validObject(object)

  if(length(object@seed) > 0){
    set.seed(object@seed)
  }

  params = object@p
  params$data = object@data
  simDataFun = object@simData@fun

  # fit the model at the timepoint the next patient arrives
  params$data = do.call(simDataFun, params)

  return(params$data)
})



setMethod("summary", signature = "singleStageDesign", definition = function(object){
  selectMethod("summary", list("trialDesign"))(object)
})

