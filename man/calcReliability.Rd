% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcReliability.R
\name{calcReliability}
\alias{calcReliability}
\title{Calculate reliability of quality measure performance}
\usage{
calcReliability(
  df = NULL,
  model = NULL,
  entity = "entity",
  y = "y",
  data.type = "binary",
  show.all = FALSE,
  ctrPerf = controlPerf(),
  ctrRel = controlRel()
)
}
\arguments{
\item{df}{observation-level data; if null, will use the dataframe from the model object}

\item{model}{model; if null, will use an unadjusted model}

\item{entity}{data column containing the accountable entity identifier}

\item{y}{data column containing the outcome variable}

\item{data.type}{acceptable values are "binary" for 0/1 data and "continuous" for continuous data (default: 'binary')}

\item{show.all}{logical indicator for whether full list of reliability method estimates should be calculated (default: FALSE)}

\item{ctrPerf}{parameters to control performance measure calculation}

\item{ctrRel}{parameters to control reliability estimation}
}
\description{
This function calculates several estimates of quality measure performance.
}
\references{
Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
}
\author{
Kenneth Nieser (nieser@stanford.edu)
}
