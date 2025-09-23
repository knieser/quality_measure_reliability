#' Colonoscopy follow-up data
#'
#' These data are from the Centers for Medicare and Medicaid Services (CMS). They contain the percentage of patients receiving appropriate recommendation for follow-up screening colonoscopy
#' in calendar year 2023.
#'
#' Data can be accessed from this link: \link{https://data.cms.gov/provider-data/dataset/yv7e-xc69}
#'
#' \code{entity} indicates the accountable health care entity;
#' \code{p} indicates the follow-up rate;
#' \code{n} indicates the denominator count for the measure;
#' \code{x} indicates the numerator count for the measure.
#'
#' @format A data frame with five variables:
#'  \code{entity}, \code{p},
#'   \code{n}, \code{x}.
"colonoscopy"


#' Psychiatric 30-day, unplanned readmission data
#'
#' These data are from the Centers for Medicare and Medicaid Services (CMS) and show the percentage of patients who return to a hospital for an unplanned inpatient stay after discharge. Data are from 07/01/2021 to 06/30/2023
#'
#' Data can be accessed from this link: \link{https://data.cms.gov/provider-data}
#'
#'
#' @format A data frame with six variables:
#'  \code{entity}, \code{category},
#'   \code{n}, \code{rate}, \code{rate.lwr}, \code{rate.upr}.
"psychreadmission"



