

#' UpdateSimsInFolder
#'
#' This folder takes a group of designs and runs them one after the other. There is checking mechanisms in order to prevent rerun
#'
#' @param inFolder Folder of trial designs to run
#' @param outFolder Output folder to save results to
#' @param updateFunctions An object of the same class as the setup object with updated functions. Allows you to update everything wilh new functions and rerun anything which has changed.
#' @param ... Arguments passed to simTrial. In particular you may want to set /code{parallel = TRUE}.
#' @export UpdateSimsInFolder
UpdateSimsInFolder = function(inFolder, outFolder, updateFunctions = NULL, ...) {


  inFiles = list.files(path = inFolder, pattern = ".Rdata")
  outFiles = list.files(path = outFolder, pattern = ".Rdata")
  runFile = rep(FALSE, length(inFiles))



  for(i in 1:length(inFiles)) {
    file = inFiles[i]
    file2 = paste0(gsub(".Rdata","",file),"_results.Rdata")
    test = load(paste0(inFolder,file))
    if(!"setup" %in% test) {
      warning("Input file: '", file, "' did not contain a setup object. No simulations could be run.")
    } else {
      setup2 = setup
      # check for matching results file
      exist = grep(file2, outFiles)
      if(length(exist) == 1) {
        test = load(paste0(outFolder, file2))
        if(!"results" %in% test) {
          warning("Output file: '", file, "' did not contain a results object. Will overwrite with results.")
          runFile[i] = TRUE
        } else {
          # check matched function is the same
          if(!is.null(updateFunctions)) {
            sNames = slotNames(updateFunctions)
            for(j in sNames) {
              if(!j %in% c("p","sims")) {
                if(!isTRUE(all.equal(slot(setup, j), slot(updateFunctions, j)))){
                  runFile[i] = TRUE
                }
              }
            }
          }

          if(!is.null(updateFunctions)) {
            sNames = slotNames(updateFunctions)
            for(j in sNames) {
              if(!j %in% c("sims")) {
                if(!isTRUE(all.equal(slot(setup, j), slot(setup2, j)))){
                  runFile[i] = TRUE
                }
              }
            }
          }
        }
      } else {
        runFile[i] = TRUE
      }
    }

    if(runFile[i]) { # run if required!
      save = FALSE
      # check input matches updateFunctions
      if(!is.null(updateFunctions)) {
        sNames = slotNames(updateFunctions)
        for(j in sNames) {
          if(!j %in% c("p","sims")) {
            if(!isTRUE(all.equal(slot(setup, j), slot(updateFunctions, j)))){
              save = TRUE # flag changes to setup based on updateFunctions
              slot(setup, j) = slot(updateFunctions, j)
            }
          }
        }
      }

      # if there is a change to setup
      if(save) {
        save(setup, file = paste0(inFolder, file))
      }
    }
  }
  print(runFile)

  message("Simulations to run: ", sum(runFile), "/", length(runFile))
  counter = 1
  for(i in 1:length(inFiles)){
    if(runFile[i] != FALSE){
      message("Starting file: ", counter, "/", sum(runFile))
      file = inFiles[i]
      file2 = paste0(gsub(".Rdata","",file),"_results.Rdata")
      load(paste0(inFolder,file))
      # finally run the simulation
      results = simTrial(setup, ...)
      t0 = Sys.time()
      save(results, file = paste0(outFolder, file2))
      message(difftime(Sys.time(), t0, units = "hours"))
      counter = counter + 1
    }
  }
}

