#' @exportClass singleDataDesign
#' @export singleDataDesign
singleDataDesign = setClass( "singleDataDesign", list(
  simulateData = "simulateData"),
  contains = "trialDesign"
)





setMethod("simTrial", signature = c(object = "singleDataDesign"), definition = function( object, nSim = NULL, ...){

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

      dataFun = object@simulateData@fun
      triggerFun = object@triggerAnalysis@fun
      modelFun = object@model@fun
      decisionFun = object@decision@fun



      while (params$decision@continue) {
        # print(data)
        # print(params$decision)


        # get data for next patient
        params$data = do.call(dataFun, params)

        ###########################################
        # check if ready to analyse
        params$decision = do.call(triggerFun, params)
        if(params$decision@analyse){

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

  object@sims =  .local(object, nsim = object@nSim, ...)

  return(object)
})


setMethod("getData", signature = c(object = "singleDataDesign"), definition = function(object) {

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
