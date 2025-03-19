#' Calculate measure performance
#' @description
#' This function calculates measure performance by accountable entity.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated risk-standardized measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom stats aggregate predict
#' @importFrom lme4 glmer
#' @export

profiling_analysis <- function(df, model = NULL, entity = 'entity', y = 'y', ctrPerf = controlPerf()){

  # clean data
  df = cleanData(df, entity = entity, y = y, ctrPerf = ctrPerf)

  # calculate measure performance
  perf.out <- calcPerformance(df = df, model = model, entity = entity, y = y, ctrPerf = ctrPerf)
  df.perf <- perf.out$df
  fit <- perf.out$fit
  perf.results <- perf.out$perf.results
  marg.p = perf.out$marg.p
  rate.summary = rbind(summary(perf.results$p),
                       summary(perf.results$rs.oe),
                       summary(perf.results$rs.pe),
                       summary(perf.results$rs.direct)
                       )
  perf.summary = cbind(Method = c('Unadjusted', 'OE standardized', 'PE standardized', 'Direct standardized'),
                       round(rate.summary, 3))
  perf.summary = as.data.frame(perf.summary)
  category.table = table(perf.results$category.oe, perf.results$category.pe)
  corr.oe.randint = cor(perf.results$intercept.OR, perf.results$oe)
  corr.pe.randint = cor(perf.results$intercept.OR, perf.results$pe)
  icc.oe.randint = psych::ICC(perf.results[,c('intercept.OR', 'oe')])
  icc.pe.randint = psych::ICC(perf.results[,c('intercept.OR', 'pe')])

  results = list(df = df.perf,
                 model = model,
                 fit = fit,
                 marg.p = marg.p,
                 perf.results = perf.results,
                 perf.summary = perf.summary,
                 category.table = category.table,
                 corr.oe.randint = corr.oe.randint,
                 corr.pe.randint = corr.pe.randint,
                 icc.oe.randint = icc.oe.randint,
                 icc.pe.randint = icc.pe.randint
                )

  return(results)
}
