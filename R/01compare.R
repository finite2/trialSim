
#' compare
#'
#' A function to generate summary comparisons of multiple simulations. This is made reasonably flexible by allowing the user to provide a function to extract the information they require from each results object.
#'
#' @param outFolder The folder with all the .Rdata files containing results objects
#' @param p (optional) A list of terms which will be found in the results object. This acts as a filter only keeping those results object which match.
#' @param fun A function to generate a row of the summary data.frame. This function should take at least two arguments: object and file.name.
#' @param colNames The column names to use for the /code{data.frame}.
#' @param paired Whether simulations are in hypothesis pairs. e.g. 1 is the null hypothesis and 2 is the alternative hypothesis
#' @param ... Additional arguments to pass to fun.
#' @export compare
compare = function(outFolder, p = NULL, fun = NULL, colNames = NULL, paired = FALSE, ...) {

  args = list(...)

  if(substr(outFolder, nchar(outFolder),nchar(outFolder)) != "/") {
    outFolder = paste0(outFolder,"/")
  }

  # default function if not provided
  if(is.null(fun)) {
    fun = function(object, file.name, ...) {
      n = mean(sapply(object@sims, function(x) sum(!is.na(x$data$cohort))))
      return(c(file.name, n))
    }
    colNames = c("Filename", "Mean SS")
  }


  files = list.files(path = outFolder, pattern = ".Rdata")

  df = data.frame(matrix(0,nrow = 0, ncol = length(colNames)))
  colnames(df) = colNames

  j = 1
  pa = 1
  for(i in 1:length(files)){


    load(paste0(outFolder,files[i]))
    if(paired & pa == 1){
      results1 = results
    }

    keep = TRUE
    ## filter with p
    if(!is.null(p)){
      print(p)
      nme = names(p)
      for(k in 1:length(nme)){
        if(!isTRUE(all.equal(p[[nme[k]]],results@p[[nme[k]]]))){
          keep = FALSE
        }
      }
    }

    if(keep){
      args$file.name = files[i]
      if(!paired){
        args$object = results
        df[j,] = do.call(fun, args)
        j = j + 1
      } else if(pa == 2){
        args$object1 = results1
        args$object2 = results
        df[j,] = do.call(fun, args)
        j = j + 1
      }
      pa = 3 - pa
    }
  }
  return(df)
}
