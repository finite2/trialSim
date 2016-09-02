#' arrivalTimeDiscrete
#'
#' A function for generating event times from a stepwise rate function. For event times leave t0 at the default (0). For arrival times set t0 to the last arrival time.
#'
#' @param data a dataframe containing a time and a rate column
#' @param t0 the time of the last arrival
#'
#' @examples
#' data = data.frame(time = c(0,1,2,3,4,5),rate = c(0.5,2,3,4,6,7))
#' t0 = 0
#' arrivalTime(data, 0)
#' hist(sapply(1:10000, function(x) arrivalTime(data, 0)))
#' @export arrivalTimeDiscrete
arrivalTimeDiscrete = function(data, t0 = 0) {
  cRow = max(which(data$time <= t0))
  dm =dim(data)
  data[dm[1]+1,] = data[dm[1],]
  tNext = Inf
  while(cRow < dm[1] & tNext > data$time[cRow]){
    tNext = t0 + rexp(1,data$rate[cRow])
    cRow = cRow + 1
    t0 = min(tNext,data$time[cRow])
  }
  return(tNext)
}




#' arrivalTimeContinuous
#'
#' A function for generating event times from a continuous rate function. For speed it requires the integral of the rate function. This can easily be zero inflated but will only keep the zero inflation if t0 = 0. It uses a random number from the uniform distribution followed by a binary search for the event time.
#'
#' @param fun A function for the integral of the rate parameter. This should take exactly one input value for the time
#' @param t0 The time of the last arrival
#' @param tmax The length of time to do the search in it will return a value in the interval t0 + tmax - acc < time < t0 + tmax if the time is right censored by tmax.
#' @param acc The size of the interval to search for. The middle value of the interval is returned
#'
#' @examples
#' # start time of 5 showing that for function(x) = x it matches the exponential distribution.
#' integralFun = function(x) x
#' par(mfrow = c(1,2))
#' hist(sapply(1:10000,function(x) arrivalTimeContinuous(integralFun,5,15)), freq = FALSE, breaks = 20,
#'      main = "Using arrivalTimeContinuous")
#' hist(rexp(10000,1), freq = FALSE, breaks = 20, main = "Using rexp")
#'
#' integralFun = function(x) x^2/2
#' par(mfrow = c(1,2))
#' hist(sapply(1:10000,function(x) arrivalTimeContinuous(integralFun)), freq = FALSE, breaks = 20,
#'      main = "Using arrivalTimeContinuous")
#' hist(rexp(10000,1), freq = FALSE, breaks = 20, main = "Using rexp")
#' @export arrivalTimeContinuous
arrivalTimeContinuous = function(fun, t0 = 0, tmax = 10, acc = 0.05){

  tLower = t0
  tUpper = t0 + tmax

  pr =runif(1)
  while(tUpper - tLower > acc) {
    nxt = (tUpper + tLower) / 2

    irate = if(t0==0){
      fun(nxt)
    } else {
      fun(nxt) - fun(t0)
    }

    if(1 - exp(-irate) > pr){
      tUpper = nxt
    } else {
      tLower = nxt
    }
  }
  return((tUpper + tLower) / 2)
}




