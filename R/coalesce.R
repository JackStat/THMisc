#' Coalesce vectors
#' 
#' @description Returns the expression with the highest precedence that is nor missing. If all expressions are missing, the result is NA.
#'
#' @examples 
#' coalesce(c(NA,2,NA), c(1, NA, NA), c(NA, NA, 3))






coalesce<-function(...){
  
  Reduce(function(x,y){
    i<-which(is.na(x))
    x[i]<-y[i]
    x
  },
  list(...))
  
}

