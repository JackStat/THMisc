#' Calculate difference between 2 dates in years.
#' 
#' @description Calculated the difference in years between two dates.  The function calulates the number of days that fall in 365-day years and divides that by 365.  Then adds the number of days that fall in 366-day years and divides that by 366.
#'
#'
#' @param startdate The first date
#' @param stopdate The second date
#' @param type TTTTTTT 
#' 
#' @import lubridate
#' 

yrdif<-function(startdate, stopdate, type='actual'){
  
  if(type=='actual'){
    ys<-year(startdate)
    ye<-year(stopdate)
    
    yslast<-as.Date(paste0(year(startdate)+1,'-01-', '01'))
    yelast<-as.Date(paste0(year(stopdate)+1,'-01-', '01'))
    
    if(ys==ye){
      
      if(leap_year(ys)){
        return((stopdate-startdate)/366)
      }
      
      if(!leap_year(ys)){
        return((stopdate-startdate)/365)
      }
      
    }
    
    if(ys!=ye){
      
      denom<-rep(1, length(ys:ye))
      
      if(leap_year(ys)) {denom[1]<-(yslast-startdate)/366}
      if(!leap_year(ys)) {denom[1]<-(yslast-startdate)/365}
      
      if(leap_year(ye)) {denom[length(ys:ye)]<-abs(1-(yelast-stopdate)/366)}
      if(!leap_year(ye)) {denom[length(ys:ye)]<-abs(1-(yelast-stopdate)/365)}
      
      return(sum(denom))
      
    }
  }
  
  if(type=='365'){
    return(as.numeric(as.Date(stopdate)-as.Date(startdate))/365)
  }
    
}

