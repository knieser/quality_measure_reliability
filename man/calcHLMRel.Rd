% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcHLMRel.R
\name{calcHLMRel}
\alias{calcHLMRel}
\title{Calculate reliability using a hierarchical linear regression model}
\usage{
calcHLMRel(
  df = NULL,
  model = NULL,
  entity = "entity",
  y = "y",
  ctrPerf = controlPerf()
)
}
\arguments{
\item{df}{observation-level data; if null, will use the dataframe from the model object}

\item{model}{model; if null, will use an unadjusted model}

\item{entity}{data column containing the accountable entity identifier}

\item{y}{data column containing the outcome variable}

\item{ctrPerf}{parameters to control performance measure calculation}
}
\value{
Estimated parameters and reliability
\item{var.b.aov}{between-entity variance}
}
\description{
This function estimates reliability using a hierarchical linear regression model.
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
