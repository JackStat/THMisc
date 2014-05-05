\name{sas.get}
\alias{sas.get}
\title{Read in sas datasets}
\usage{
sas.get(libraryName, member, variables = character(0), ifs = character(0),
  format.library = libraryName, id, dates. = c("sas", "yymmdd", "yearfrac",
  "yearfrac2"), keep.log = TRUE, log.file = "_temp_.log",
  macro = sas.get.macro, data.frame.out = existsFunction("data.frame"),
  clean.up = FALSE, quiet = FALSE, temp = tempfile("SaS"),
  formats = TRUE, recode = formats, special.miss = FALSE,
  sasprog = "sas", as.is = 0.5, check.unique.id = TRUE,
  force.single = FALSE, pos, uncompress = FALSE,
  defaultencoding = "latin1", var.case = "lower")
}
\description{
Read in sas datasets
}

