\name{coalesce}
\alias{coalesce}
\title{Coalesce vectors}
\usage{
coalesce(...)
}
\description{
Returns the expression with the highest precedence that is
nor missing. If all expressions are missing, the result is
NA.
}
\examples{
coalesce(c(NA,2,NA), c(1, NA, NA), c(NA, NA, 3))
}

