


setMethod("getModel", signature = c(object = "trialDesign"), definition = function(object) {

  validObject(object)

  if(length(object@seed) > 0){
    set.seed(object@seed)
  }

  params = object@p
  params$data = object@data

  modelFun = object@model@fun


  # fit the model at the timepoint the next patient arrives
  params$model = do.call(modelFun, params)

  return(params$model)
})


setMethod("getDecision", signature = c(object = "trialDesign"), definition = function(object, model) {
  validObject(object)

  if(length(object@seed) > 0){
    set.seed(object@seed)
  }

  params = object@p
  params$data = object@data

  decisionFun = object@decision@fun
  params$model= model
  # make decisions as to what happens next
  params$decision = do.call(decisionFun, params)

  return(params$decision)
})
