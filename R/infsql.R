#' Informative SQL via RODBC
#' 
#' @description A simple wrapper around the sqlQuery function in RODBC.  It prints out dimensions of the table and the system time information for logging purposes.
#'
#'
#' @param channel The ODBC channel
#' @param query The SQL query
#' 
#' @import RODBC



infsql<-function(channel, query, UniqueID=1, include.time=TRUE){
  
  if(include.time==TRUE){
    cat("SQL via RODBC used (Total process time): \n")
    print(system.time(Q<-sqlQuery(channel, query)))
  }
  
  else Q<-sqlQuery(channel, query)
    
  cat(paste("\n Table created from SQL, with", 
            length(unique(Q[,UniqueID])),
            "unique IDs",
            nrow(Q), 
            "rows and", 
            ncol(Q), 
            "columns"
            ))
  
  Q
}