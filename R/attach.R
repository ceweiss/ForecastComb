.onAttach <- function(...)
{
  version <- packageVersion("GeomComb")
  packageStartupMessage(paste("This is GeomComb",version,"\n"))
}


