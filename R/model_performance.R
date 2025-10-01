#' Calculate model performance
#' @description
#' This function calculates risk model performance.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param data.type acceptable values are `binary` for 0/1 data and `continuous` for continuous data (default: `binary`)
#' @param predictor.clean optional list of formatted names of predictors in the model
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated risk-standardized measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @importFrom stats aggregate predict qnorm pnorm wilcox.test
#' @export

model_performance <- function(df, model, entity = 'entity', y = 'y', data.type = 'binary', predictor.clean = NULL, ctrPerf = controlPerf()){
  if(data.type != 'binary' & data.type != 'continuous') stop('The only valid values for data.type are binary or continuous.')

  cl <- match.call()
  alpha = ctrPerf$alpha
  z = stats::qnorm(1 - alpha/2)

  # clean data
  df = cleanData(df, entity = entity, y = y, ctrPerf = ctrPerf)

  data.out <- calcDataSummary(df, model, entity, y, data.type, ctrPerf)
  df = data.out$df
  model = data.out$model
  fit = data.out$fit
  coef.estimates <- as.data.frame(summary(fit)$coef)

  if (data.type == 'binary'){
    marg.p <- data.out$marg.p
    coef.estimates$exp.estimate <- exp(coef.estimates$Estimate)
    coef.estimates$lb <-  exp(coef.estimates$Estimate - z * coef.estimates$`Std. Error`)
    coef.estimates$ub <-  exp(coef.estimates$Estimate + z * coef.estimates$`Std. Error`)

    model.results <- data.frame(
      predictor = row.names(coef.estimates)[-1],
      est = coef.estimates$exp.estimate[-1],
      lb = coef.estimates$lb[-1],
      ub = coef.estimates$ub[-1],
      p = coef.estimates$`Pr(>|z|)`[-1]
    )
  }

  if (data.type == 'continuous'){
    coef.estimates$lb <-  coef.estimates$Estimate - z * coef.estimates$`Std. Error`
    coef.estimates$ub <-  coef.estimates$Estimate + z * coef.estimates$`Std. Error`
    coef.estimates$z  <-  coef.estimates$Estimate / coef.estimates$`Std. Error`
    coef.estimates$p  <- 2*stats::pnorm(abs(coef.estimates$z), lower.tail = FALSE)

    model.results <- data.frame(
      predictor = row.names(coef.estimates)[-1],
      est = coef.estimates$Estimate[-1],
      lb = coef.estimates$lb[-1],
      ub = coef.estimates$ub[-1],
      p = coef.estimates$p[-1]
    )
  }

  model.results$sig = as.factor(ifelse(model.results$p < alpha, 1, 0))
  model.results$rank <- rank(model.results$est, ties.method = 'random')
  model.results$predictor.clean <- model.results$predictor
  predictor.original = model.results$predictor
  if (is.null(predictor.clean)){predictor.clean = model.results$predictor}
  if (length(predictor.original) != length(predictor.clean)) stop('The length of predictor.clean does not match the number of predictors in the model.')
  for (k in 1:length(predictor.clean)){
    model.results$predictor.clean[model.results$predictor == predictor.original[k]] <- predictor.clean[k]
  }
  model.results$predictor.clean <- as.factor(model.results$predictor.clean)
  model.results$predictor.clean <- factor(model.results$predictor.clean, levels = model.results$predictor.clean[order(model.results$rank, decreasing = T)])

  if (data.type == 'binary'){
    # calculate c statistic using wilcox
    p1 = df$predict[df$y==1]
    p0 = df$predict[df$y==0]
    n1 = length(p1)
    n0 = length(p0)
    pairs = n0 * n1
    wilcox.out <- stats::wilcox.test(predict ~ y, data = df)
    c.statistic = max(wilcox.out$statistic / pairs, 1 - wilcox.out$statistic / pairs)

    results = list(call = cl,
                   data.type = data.type,
                   df = df,
                   model = model,
                   fit = fit,
                   marg.p = marg.p,
                   c.statistic = c.statistic,
                   model.results = model.results)
  }

  if (data.type == 'continuous'){
    results = list(call = cl,
                   data.type = data.type,
                   df = df,
                   model = model,
                   fit = fit,
                   model.results = model.results)
  }


  return(results)
}
