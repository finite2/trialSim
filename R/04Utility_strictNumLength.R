
#' strictNumLength
#'
#' A vectorised utility tool for making numbers the same length. This is usefull for getting filenames in the right order since R thinks stuff_2 comes after stuff_10. Instead this will return numbers such as 0002 and 0010.
#'
#' @param i the number to make strict
#' @param d the length of the output number
#'
#'
#' @examples
#'
#' strictNumLength(c(1,2,3))
#'
#'
#' @export strictNumLength
strictNumLength = function(i, d = 4) {

  sapply(i, function(x) paste0(paste0(rep("0",d-nchar(x)),collapse = ""),x))
}
