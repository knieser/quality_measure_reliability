#' Calculate measure performance by accountable entity
#' @description
#' This function calculates measure performance by accountable entity.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aggregate predict rbinom
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @export

calcPerformance <- function(df = NULL, model = NULL, entity = "entity", y = "y", data.type = 'binary', ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  cl <- match.call()
  alpha = ctrPerf$alpha
  ci.lwr = alpha/2
  ci.upr = 1 - alpha/2
  z = qnorm(1 - alpha/2)

  if(data.type !='binary') stop('This function currently works with binary outcome data only.')

  data.out <- calcDataSummary(df, model, entity, y, data.type, ctrPerf)
  df = data.out$df
  model = data.out$model
  fit = data.out$fit
  marg.p <- data.out$marg.p
  marg.p.model <- data.out$marg.p.model
  entities <- data.out$entities
  n  <- data.out$n
  obs <- data.out$obs
  p <- data.out$p
  p.lwr <- data.out$p.lwr
  p.upr <- data.out$p.upr
  p.re <- data.out$p.re
  pred <- data.out$pred
  exp <- data.out$exp
  rank <- data.out$rank

  # direct standardization
  rs.direct = vector(length = length(entities))
  df.ds <- df
  for (i in 1:length(entities)){
    df.ds$sta3n = entities[i]
    rs.direct[i] = mean(predict(fit, newdata = df.ds, type = 'response'))
  }

  # indirect standardization
  oe      <- obs / exp
  pe      <- pred / exp
  rs.oe   <- oe * marg.p
  rs.pe   <- pe * marg.p
  rank.oe <- rank(oe, ties.method = "random")
  rank.pe <- rank(pe, ties.method = "random")

  # parametric bootstrapping
  n.boots = ctrPerf$n.boots
  n.cores = ctrPerf$n.cores
  df.pb = df

  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)

  out <- foreach::foreach(s = 1:n.boots, .combine = rbind, .packages = c('lme4')) %dopar% {
    estOEPE <- function(df, model, entities, entity = "entity", y = "y"){
      df$y         <- df[[y]]
      df$entity    <- df[[entity]]

      # refit model
      fit <- lme4::glmer(model, data = df, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'), nAGQ = 0)

      # calculate predictions w/ and w/o random intercept
      df$expect    <- predict(fit, newdata = df, type = 'response', re.form = ~0)
      df$predict   <- predict(fit, newdata = df, type = 'response')

      # calculate oe and pe ratios
      obs    <- aggregate(y ~ entity, data = df, sum)$y
      pred   <- aggregate(predict ~ entity, data = df, sum)$predict
      exp    <- aggregate(expect ~ entity, data = df, sum)$expect
      oe     <- obs / exp
      pe     <- pred / exp

      # calculate OE and PE risk-standardized rates
      marg.p <- mean(df$y)
      rs.oe  <- oe * marg.p
      rs.pe  <- pe * marg.p

      # calculate direct standardized rates
      rs.direct = vector(length = length(entities))
      df.ds <- df
      for (j in 1:length(entities)){
        df.ds$sta3n = entities[j]
        rs.direct[j] = mean(predict(fit, newdata = df.ds, type = 'response'))
      }

      out <- list(oe = oe, pe = pe, rs.oe = rs.oe, rs.pe = rs.pe, rs.direct = rs.direct)
      return(out)
    }

    df.pb[[y]] = rbinom(nrow(df.pb), 1, df.pb$predict)
    est.boot <- estOEPE(df.pb, model, entities, entity, y)
    list(
      oe.boot = est.boot$oe,
      pe.boot = est.boot$pe,
      rs.oe.boot = est.boot$rs.oe,
      rs.pe.boot = est.boot$rs.pe,
      rs.direct.boot = est.boot$rs.direct
    )
  }
  parallel::stopCluster(cl)

  out <- as.data.frame(out)

  output = list(
    oe.boot = matrix(unlist(out$oe.boot), nrow = length(entities), ncol = n.boots),
    pe.boot = matrix(unlist(out$pe.boot), nrow = length(entities), ncol = n.boots),
    rs.oe.boot = matrix(unlist(out$rs.oe.boot), nrow = length(entities), ncol = n.boots),
    rs.pe.boot = matrix(unlist(out$rs.pe.boot), nrow = length(entities), ncol = n.boots),
    rs.direct.boot = matrix(unlist(out$rs.direct.boot), nrow = length(entities), ncol = n.boots)
  )
  oe.boot = output$oe.boot
  pe.boot = output$pe.boot
  rs.oe.boot = output$rs.oe.boot
  rs.pe.boot = output$rs.pe.boot
  rs.direct.boot = output$rs.direct.boot

  oe.se <- apply(oe.boot, 1, sd)
  pe.se <- apply(pe.boot, 1, sd)
  rs.oe.se <- apply(rs.oe.boot, 1, sd)
  rs.pe.se <- apply(rs.pe.boot, 1, sd)
  rs.direct.se <- apply(rs.direct.boot, 1, sd)

  oe.lwr = apply(oe.boot, 1, quantile, ci.lwr)
  oe.upr = apply(oe.boot, 1, quantile, ci.upr)

  pe.lwr = apply(pe.boot, 1, quantile, ci.lwr)
  pe.upr = apply(pe.boot, 1, quantile, ci.upr)

  rs.oe.lwr = apply(rs.oe.boot, 1, quantile, ci.lwr)
  rs.oe.upr = apply(rs.oe.boot, 1, quantile, ci.upr)

  rs.pe.lwr = apply(rs.pe.boot, 1, quantile, ci.lwr)
  rs.pe.upr = apply(rs.pe.boot, 1, quantile, ci.upr)

  rs.direct.lwr = apply(rs.direct.boot, 1, quantile, ci.lwr)
  rs.direct.upr = apply(rs.direct.boot, 1, quantile, ci.upr)

  category.oe = rep('No different than average', length(entities))
  category.oe[rs.oe.upr < marg.p] <- 'Lower than average'
  category.oe[rs.oe.lwr > marg.p] <- 'Higher than average'

  category.pe = rep('No different than average', length(entities))
  category.pe[rs.pe.upr < marg.p] <- 'Lower than average'
  category.pe[rs.pe.lwr > marg.p] <- 'Higher than average'

  m.re = as.data.frame(ranef(fit))
  m.re.intercept <- data.frame(est = m.re$condval[m.re$term == '(Intercept)'],
                               sd = m.re$condsd[m.re$term == '(Intercept)'],
                               entity = m.re$grp[m.re$term == '(Intercept)'])

  perf.results <- data.frame(
    entities = entities,
    n = n,
    observed = obs,
    predicted = pred,
    expected = exp,
    p = p,
    p.lwr = p.lwr,
    p.upr = p.upr,
    p.re = p.re,
    rank.p = rank,
    oe = oe,
    oe.lwr = oe.lwr,
    oe.upr = oe.upr,
    rank.oe = rank.oe,
    pe = pe,
    pe.lwr = pe.lwr,
    pe.upr = pe.upr,
    rank.pe = rank.pe,
    rs.oe = rs.oe,
    rs.oe.lwr = rs.oe.lwr,
    rs.oe.upr = rs.oe.upr,
    category.oe,
    rs.pe = rs.pe,
    rs.pe.lwr = rs.pe.lwr,
    rs.pe.upr = rs.pe.upr,
    category.pe,
    rs.direct = rs.direct,
    rs.direct.lwr = rs.direct.lwr,
    rs.direct.upr = rs.direct.upr,
    intercept.OR = exp(m.re.intercept$est),
    intercept.OR.lwr = exp(m.re.intercept$est - z*m.re.intercept$sd),
    intercept.OR.upr = exp(m.re.intercept$est + z*m.re.intercept$sd)
  )
  perf.results$intercept.sig = as.factor(ifelse(perf.results$intercept.OR.lwr > 1 | perf.results$intercept.OR.upr < 1, 1, 0))

  results = list(call = cl, df = df, model = model, fit = fit, marg.p = marg.p, marg.p.model = marg.p.model, perf.results = perf.results)

  return(results)
}
