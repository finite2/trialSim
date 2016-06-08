# define the function and then use the class generator:
minTimeBetween=function(Data,param){
  return(max(Data@arrivalTime)+param$minTimeBetween)
}

nextArrivalFun(nextArrivalFunction = minTimeBetween, minTimeBetween = 1)
nextArrivalFun(minTimeBetween, minTimeBetween = 0)
# note that all arguments need to be named

# define a new function to generate these class objects directly:
arrivalFunMinTimeBetween=function(minTimeBetween = 1){
  nextArrivalFunction=function(Data,params){
    return(max(Data@arrivalTime)+params$minTimeBetween)
  }
  return(nextArrivalFun(nextArrivalFunction=nextArrivalFunction,minTimeBetween = 1))
}

arrivalFunMinTimeBetween(minTimeBetween = 0)
arrivalFunMinTimeBetween(1)
# no need to name arguments

