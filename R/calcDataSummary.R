calcDataSummary <- function(df, model = NULL, entity = 'entity', y = "y", ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')

  alpha <- ctrPerf$alpha

  df <- cleanData(df, entity, y, ctrPerf)
  if (is.null(model)){model = paste0(y, ' ~ (1|', entity, ')')}
  fit <- lme4::glmer(formula = model, data = df, family = 'binomial', control = glmerControl(optimizer = "bobyqa"), nAGQ = 0)
  df$expect  <- predict(fit, newdata = df, type = 'response', re.form = ~0)
  df$predict <- predict(fit, newdata = df, type = 'response')
  df$predict.var <- df$predict * (1 - df$predict)

  n        <- aggregate(y ~ entity, data = df, length)$y
  agg      <- aggregate(y ~ entity, data = df, sum)
  entities <- agg$entity
  obs      <- agg$y
  p        <- obs / n
  p.ci     <- t(apply(cbind(obs, n), 1, function(x) prop.test(x[1], x[2], conf.level = 1-alpha)$conf.int))
  p.lwr    <- p.ci[,1]
  p.upr    <- p.ci[,2]
  pred     <- aggregate(predict ~ entity, data = df, sum)$predict
  p.re     <- pred / n
  exp      <- aggregate(expect ~ entity, data = df, sum)$expect
  rank     <- rank(p, ties.method = "random")

  marg.p = mean(df$y)
  marg.p.model = mean(df$predict)

  output = list(df = df, model = model, fit = fit, marg.p = marg.p, marg.p.model = marg.p.model, entities = entities, n = n, obs = obs, p = p, p.lwr = p.lwr,
                p.upr = p.upr, pred = pred, p.re = p.re, exp = exp, rank = rank)
  return(output)
}
