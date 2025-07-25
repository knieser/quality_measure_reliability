% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotCalibration.R
\name{plotCalibration}
\alias{plotCalibration}
\title{Plot calibration curve for risk-adjustment model}
\usage{
plotCalibration(model.performance = model.performance, quantiles = 10)
}
\arguments{
\item{model.performance}{results from model_performance()}

\item{quantiles}{number of quantiles to bin data; default is 10.}
}
\description{
This function creates a plot of the model calibration curve
}
\author{
Kenneth Nieser (nieser@stanford.edu)
}
