#' Calculate measure performance by accountable entity
#' @description
#' This function calculates measure performance by accountable entity.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param data.type acceptable values are `binary` for 0/1 data (default: `binary`)
#' @param ctrPerf parameters to control performance measure calculation
#' @returns A list including:
#' *`df`: a cleaned dataframe used to calculate measure performance
#' *`model`: the model used to calculate measure performance
#' *`fit`: the fitted model results
#' *`marg.p`: overall, unadjusted average performance across all entities
#' *`perf.results`: performance results by entity
#'
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @examples
#' # simulate data
#' df <- simulateData(n.entity = 50, n.obs = 100, mu = .2, r = .7)
#'
#' # calculate measure performance
#' out <- calcPerformance(df = df, entity = 'entity', y = 'y')
#'
#' # plot performance
#' plotPerformance(out$perf.results)
#'
#' @importFrom stats aggregate predict rbinom var sd
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach  %dopar%
#' @export

calcPerformance <- function(df = NULL, model = NULL, entity = "entity", y = "y", data.type = 'binary', ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if(data.type !='binary') stop('This function currently works with binary outcome data only.')

  cl <- match.call()
  alpha = ctrPerf$alpha
  ci.lwr = alpha/2
  ci.upr = 1 - alpha/2
  z = stats::qnorm(1 - alpha/2)
  n.boots = ctrPerf$n.boots
  if(is.null(model)) {risk.adj = 0} else {risk.adj = 1}
  if(risk.adj == 1){
    message(paste0('
    Note: OE and PE risk-standardized rates are being calculated using the specified risk-adjustment model. Confidence intervals for PE-standardized rates are being calculated with ', n.boots, ' parametric bootstraps. This could take a few mintues.

    You can change the number of bootstraps using the `controlPerf()` function, which is a parameter within `calcPerformance()`.'))
    }

  data.out <- calcDataSummary(df, model, entity, y, data.type, ctrPerf)
  df = data.out$df
  model = data.out$model
  fit = data.out$fit
  var.b <- lme4::VarCorr(fit)[[entity]][1,1]
  marg.p <- data.out$marg.p
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

  # random effects
  ranef <- as.data.frame(ranef(fit))
  names(ranef) <- c('grpvar', 'term', 'entity', 'ranef', 'ransd')
  df <- merge(df, ranef[,c('entity', 'ranef', 'ransd')], by = 'entity')

  m.re = as.data.frame(ranef(fit))
  m.re.intercept <- data.frame(est = m.re$condval[m.re$term == '(Intercept)'],
                               sd = m.re$condsd[m.re$term == '(Intercept)'],
                               entity = m.re$grp[m.re$term == '(Intercept)'])

  if (risk.adj==0){
    category.p = rep('No different than average', length(entities))
    category.p[p.upr < marg.p] <- 'Lower than average'
    category.p[p.lwr > marg.p] <- 'Higher than average'

    perf.results <- data.frame(
      entities = entities,
      n = n,
      observed = obs,
      p = p,
      p.lwr = p.lwr,
      p.upr = p.upr,
      p.re = p.re,
      rank.p = rank,
      category.p = category.p,
      intercept.OR = exp(m.re.intercept$est),
      intercept.OR.lwr = exp(m.re.intercept$est - z*m.re.intercept$sd),
      intercept.OR.upr = exp(m.re.intercept$est + z*m.re.intercept$sd)
    )

  } else{
    # direct standardization
    rs.direct = vector(length = length(entities))
    df.ds <- df
    for (i in 1:length(entities)){
      df.ds$entity = entities[i]
      rs.direct[i] = mean(predict(fit, newdata = df.ds, type = 'response'))
    }

    # indirect standardization O/E
    oe      <- obs / exp
    oe.lwr = n * p.lwr / exp
    oe.upr = n * p.upr / exp
    rs.oe   <- oe * marg.p
    rs.oe.lwr = oe.lwr * marg.p
    rs.oe.upr = oe.upr * marg.p
    rank.oe <- rank(oe, ties.method = "random")

    category.oe = rep('No different than average', length(entities))
    category.oe[rs.oe.upr < marg.p] <- 'Lower than average'
    category.oe[rs.oe.lwr > marg.p] <- 'Higher than average'

    # indirect standardization P/E
    pe      <- pred / exp
    rs.pe   <- pe * marg.p
    rank.pe <- rank(pe, ties.method = "random")

    # parametric bootstrapping
    n.boots = ctrPerf$n.boots
    n.cores = ctrPerf$n.cores
    df.pb = df

    cl <- parallel::makeCluster(n.cores)
    doParallel::registerDoParallel(cl)

    out <- foreach::foreach(s = 1:n.boots, .combine = rbind, .packages = c('lme4')) %dopar% {

      estPE <- function(df, model, entities, entity = "entity", y = "y"){
        df$y         <- df[[y]]
        df$entity    <- df[[entity]]

        # refit model
        fit <- lme4::glmer(model, data = df, family = 'binomial', control = lme4::glmerControl(optimizer = 'bobyqa'), nAGQ = 0)

        # calculate predictions w/ and w/o random intercept
        df$expect    <- predict(fit, newdata = df, type = 'response', re.form = ~0)
        df$predict   <- predict(fit, newdata = df, type = 'response')

        # calculate PE ratios
        pred   <- aggregate(predict ~ entity, data = df, sum)$predict
        exp    <- aggregate(expect ~ entity, data = df, sum)$expect
        pe     <- pred / exp

        # calculate PE risk-standardized rates
        marg.p <- mean(df$y)
        rs.pe  <- pe * marg.p

        # calculate direct standardized rates
        rs.direct = vector(length = length(entities))
        df.ds <- df
        for (j in 1:length(entities)){
          df.ds$entity = entities[j]
          rs.direct[j] = mean(predict(fit, newdata = df.ds, type = 'response'))
        }

        out <- list(pe = pe, rs.pe = rs.pe, rs.direct = rs.direct)
        return(out)
      }

      # Sample for PE estimates
      # sample new random effects
      ranef$alpha_i = stats::rnorm(length(entities), ranef$ranef, ranef$ransd)
      ranef$alpha_i.rescaled = ranef$alpha_i * sqrt(var.b / stats::var(ranef$alpha_i))

      # create new prediction for each observation
      df.pb.pe <- merge(df.pb, ranef[, c('entity', 'alpha_i.rescaled')], by = 'entity')
      df.pb.pe$expect.logit <- stats::qlogis(df.pb.pe$expect)
      df.pb.pe$predict.new <- stats::plogis(df.pb.pe$expect.logit + df.pb.pe$alpha_i.rescaled)

      # simulate new outcome data based on estimated model
      df.pb.pe[[y]] = rbinom(nrow(df.pb.pe), 1, df.pb.pe$predict.new)

      # get bootstrapped estimates
      est.boot.PE <- estPE(df.pb.pe, model, entities, entity, y)

      list(
        pe.boot = est.boot.PE$pe,
        rs.pe.boot = est.boot.PE$rs.pe,
        rs.direct.boot = est.boot.PE$rs.direct
      )
    }
    parallel::stopCluster(cl)

    out <- as.data.frame(out)

    output = list(
      pe.boot = matrix(unlist(out$pe.boot), nrow = length(entities), ncol = n.boots),
      rs.pe.boot = matrix(unlist(out$rs.pe.boot), nrow = length(entities), ncol = n.boots),
      rs.direct.boot = matrix(unlist(out$rs.direct.boot), nrow = length(entities), ncol = n.boots)
    )
    pe.boot = output$pe.boot
    rs.pe.boot = output$rs.pe.boot
    rs.direct.boot = output$rs.direct.boot

    pe.se <- apply(pe.boot, 1, stats::sd)
    rs.pe.se <- apply(rs.pe.boot, 1, stats::sd)
    rs.direct.se <- apply(rs.direct.boot, 1, stats::sd)

    pe.median = apply(pe.boot, 1, quantile, 0.5)
    pe.lwr = apply(pe.boot, 1, quantile, ci.lwr)
    pe.upr = apply(pe.boot, 1, quantile, ci.upr)

    rs.pe.lwr = apply(rs.pe.boot, 1, quantile, ci.lwr)
    rs.pe.upr = apply(rs.pe.boot, 1, quantile, ci.upr)

    rs.direct.lwr = apply(rs.direct.boot, 1, quantile, ci.lwr)
    rs.direct.upr = apply(rs.direct.boot, 1, quantile, ci.upr)

    category.pe = rep('No different than average', length(entities))
    category.pe[rs.pe.upr < marg.p] <- 'Lower than average'
    category.pe[rs.pe.lwr > marg.p] <- 'Higher than average'

    category.direct = rep('No different than average', length(entities))
    category.direct[rs.direct.upr < marg.p] <- 'Lower than average'
    category.direct[rs.direct.lwr > marg.p] <- 'Higher than average'

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
        pe = pe, # from observed data
        pe.median = pe.median,
        pe.lwr = pe.lwr,
        pe.upr = pe.upr,
        rank.pe = rank.pe, # from observed data
        rs.oe = rs.oe,
        rs.oe.lwr = rs.oe.lwr,
        rs.oe.upr = rs.oe.upr,
        category.oe = category.oe,
        rs.pe = rs.pe, # from observed data
        rs.pe.lwr = rs.pe.lwr,
        rs.pe.upr = rs.pe.upr,
        category.pe = category.pe,
        rs.direct = rs.direct,
        rs.direct.lwr = rs.direct.lwr,
        rs.direct.upr = rs.direct.upr,
        category.direct = category.direct,
        intercept.OR = exp(m.re.intercept$est),
        intercept.OR.lwr = exp(m.re.intercept$est - z*m.re.intercept$sd),
        intercept.OR.upr = exp(m.re.intercept$est + z*m.re.intercept$sd)
      )

    rate.summary = rbind(summary(perf.results$p),
                         summary(perf.results$rs.oe),
                         summary(perf.results$rs.pe),
                         summary(perf.results$rs.direct)
    )
    perf.summary = cbind(Method = c('Unadjusted', 'OE standardized', 'PE standardized', 'Direct standardized'), round(rate.summary, 3))
    perf.summary = as.data.frame(perf.summary)
    category.table = table(perf.results$category.oe, perf.results$category.pe)
    corr.oe.randint = stats::cor(perf.results$intercept.OR, perf.results$oe)
    corr.pe.randint = stats::cor(perf.results$intercept.OR, perf.results$pe)
    icc.oe.randint = calcICC(perf.results[,c('intercept.OR', 'oe')])
    icc.pe.randint = calcICC(perf.results[,c('intercept.OR', 'pe')])
  }

  perf.results$intercept.sig = as.factor(ifelse(perf.results$intercept.OR.lwr > 1 | perf.results$intercept.OR.upr < 1, 1, 0))

  if(risk.adj==0){
    results = list(call = cl,
                   df = df,
                   model = model,
                   fit = fit,
                   marg.p = marg.p,
                   perf.results = perf.results)
  } else{
    results = list(call = cl,
                   df = df,
                   model = model,
                   fit = fit,
                   marg.p = marg.p,
                   perf.results = perf.results,
                   perf.summary = perf.summary,
                   category.table = category.table,
                   corr.oe.randint = corr.oe.randint,
                   corr.pe.randint = corr.pe.randint,
                   icc.oe.randint = icc.oe.randint,
                   icc.pe.randint = icc.pe.randint)
  }

  return(results)
}
