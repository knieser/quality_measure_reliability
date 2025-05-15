#' Package startup Message
#' @importFrom utils packageVersion
#' @noRd

QualityMeasureStartupMessage <- function()
{
  msg <- c(paste0(
    "This is QualityMeasure version ",
    packageVersion("QualityMeasure")),
    "\n\nThis package is a work-in-progress. If you have issues or feedback, please email me at nieser@stanford.edu, so I can make this package better!",
    "\n\nFor more information about this package, see https://github.com/knieser/quality_measure_reliability.",
    "\n\nType 'citation(\'QualityMeasure\')' for citing this R package in publications.")
  return(msg)
}

.onAttach <- function(lib, pkg)
{

  msg <- QualityMeasureStartupMessage()
  if(!interactive())
    msg[1] <- paste("Package 'QualityMeasure' version", packageVersion("QualityMeasure"))
  packageStartupMessage(msg)
  invisible()
}
