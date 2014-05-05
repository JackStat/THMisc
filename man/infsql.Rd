\name{infsql}
\alias{infsql}
\title{Informative SQL via RODBC}
\usage{
infsql(channel, query, UniqueID = 1, include.time = TRUE)
}
\arguments{
  \item{channel}{The ODBC channel}

  \item{query}{The SQL query}
}
\description{
A simple wrapper around the sqlQuery function in RODBC.  It
prints out dimensions of the table and the system time
information for logging purposes.
}

