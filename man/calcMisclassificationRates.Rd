% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcMisclassificationRates.R
\name{calcMisclassificationRates}
\alias{calcMisclassificationRates}
\title{Calculate misclassification probabilities}
\usage{
calcMisclassificationRates(rs, rs.boot, d.steps)
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
