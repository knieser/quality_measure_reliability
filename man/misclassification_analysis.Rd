% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misclassification_analysis.R
\name{misclassification_analysis}
\alias{misclassification_analysis}
\title{Calculate misclassification probabilities}
\usage{
misclassification_analysis(
  df = NULL,
  model = NULL,
  entity = "entity",
  y = "y",
  ctrPerf = controlPerf(),
  ctrRel = controlRel(),
  output.dir,
  filename.add = NULL
)
}
\arguments{
\item{df}{dataframe; if null, will use the dataframe in the model object}

\item{model}{model; if null, will use an unadjusted model}

\item{entity}{variable to use as the accountable entity; default = "entity"}

\item{y}{variable to use as the outcome; default = "y"}

\item{ctrPerf}{parameters to control performance measure calculation}
}
\value{
Estimated measure performance by accountable entity
}
\description{
This function runs the misclassification functions
}
\examples{
# TBD
}
\references{
None
}
\author{
Kenneth Nieser (nieser@stanford.edu)
}
