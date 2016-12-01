.onLoad <- function(libname,pkgname)
{
  suppressPackageStartupMessages(library(mtsdi))
}

.onAttach <- function(...)
{
  version <- packageVersion("GeomComb")
  packageStartupMessage(paste("This is GeomComb",version,"\n"))
}


