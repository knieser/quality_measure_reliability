calcDataSummary <- function(df, model = NULL, entity = 'entity', y = "y", data.type = 'binary', ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  alpha <- ctrPerf$alpha

  df <- cleanData(df, entity, y, ctrPerf)
  if(data.type == 'binary' && !all(unique(df$y) %in% c(0, 1))) stop ('Data include values outside of 0 and 1; please change data.type to continuous.')
  if(is.null(model)){model = paste0(y, ' ~ (1|', entity, ')')}

  n        <- aggregate(y ~ entity, data = df, length)$y
  agg      <- aggregate(y ~ entity, data = df, sum)
  entities <- agg$entity

  if(data.type=='continuous'){
    fit <- lme4::lmer(formula = model, data = df)
    df$expect  <- predict(fit, newdata = df, type = 'response', re.form = ~0)
    df$predict <- predict(fit, newdata = df, type = 'response')

    mean <- agg$y / n

    pred     <- aggregate(predict ~ entity, data = df, sum)$predict
    exp      <- aggregate(expect ~ entity, data = df, sum)$expect
    rank     <- rank(mean, ties.method = "random")

    output = list(df = df, model = model, fit = fit, entities = entities, n = n, mean = mean, pred = pred, exp = exp, rank = rank)
  }

  if(data.type=='binary'){
    fit <- lme4::glmer(formula = model, data = df, family = 'binomial', control = lme4::glmerControl(optimizer = "bobyqa"), nAGQ = 0)
    df$expect  <- predict(fit, newdata = df, type = 'response', re.form = ~0)
    df$predict <- predict(fit, newdata = df, type = 'response')
    df$predict.var <- df$predict * (1 - df$predict)

    obs      <- agg$y
    p        <- obs / n
    p.ci <- tryCatch({
      t(apply(cbind(obs, n), 1, function(x) stats::prop.test(x[1], x[2], conf.level = 1-alpha, correct = FALSE)$conf.int))
    },
    warning = function(w){
      message('...using Clopper-Pearson intervals instead of Wilson score intervals for performance CIs...')
      t(apply(cbind(obs, n), 1, function(x) stats::binom.test(x[1], x[2], conf.level = 1-alpha)$conf.int))
    }
    )
    p.lwr <- p.ci[,1]
    p.upr <- p.ci[,2]
    pred  <- aggregate(predict ~ entity, data = df, sum)$predict
    p.re  <- pred / n
    exp   <- aggregate(expect ~ entity, data = df, sum)$expect
    rank  <- rank(p, ties.method = "random")

    marg.p = mean(df$y)
    marg.p.model = mean(df$predict)

    output = list(df = df, model = model, fit = fit, marg.p = marg.p, marg.p.model = marg.p.model, entities = entities, n = n, obs = obs, p = p, p.lwr = p.lwr,
                  p.upr = p.upr, pred = pred, p.re = p.re, exp = exp, rank = rank)
  }
  return(output)
}
