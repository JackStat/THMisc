#' Informative SQL via RODBC
#' 
#' @description A simple wrapper around the sqlQuery function in RODBC.  It prints out dimensions of the table and the system time information for logging purposes.
#'
#'
#' @param channel The ODBC channel
#' @param query The SQL query
#' 
#' @import RODBC



infsql<-function(channel, query){
  
  
  cat("SQL via RODBC used (Total process time): \n")
  print(system.time(Q<-sqlQuery(channel, query)))
  
  
  cat(paste("\n Table created from SQL, with", 
            nrow(Q), 
            "rows and", 
            ncol(Q), 
            "columns"))
  
  
  Q
}