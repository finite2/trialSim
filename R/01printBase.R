



.printtrialSim = function(object) {


  str(object@p)

  cat("\nNumber of simulations: ", object@nSim, "\n")
  cat("Simulations run: ",length(object@sims) > 0, "\n")
}


setMethod("print", signature = c(x="trialDesign"), definition = function(x) .printtrialSim(x))
setMethod("show", signature = "trialDesign", definition = function(object) .printtrialSim(object))

setGeneric("summary")

setMethod("summary", signature = c(object="trialDesign"), definition = function(object) {

  cat("Simulations run: ",length(object@sims) > 0, "\n")

  if(length(object@sims) > 0) {

    cat("Number of simulations: ", object@nSim, "\n")

    n = sapply(1:object@nSim, function(x) sum(!is.na(object@sims[[x]]$data$cohort)))

    a = as.data.frame(matrix(0,1,6))
    names(a) = c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    i=1
    a[i,1:6]=summary(n)
    rownames(a)[i] = "Number of patients"
    i=i+1

    if(!is.null(object@sims[[1]]$data$exitTime)) {

      et = sapply(1:object@nSim, function(x) max(object@sims[[x]]$data$exitTime, na.rm = TRUE))

      a[i,1:6]=summary(et)
      rownames(a)[i] = "Trial duration"
      i=i+1
    }

    print(a)
  }

})

