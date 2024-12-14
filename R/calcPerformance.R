#' Calculates measure performance by accountable entity
#' @description
#' This function calculates measure performance by accountable entity.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param y variable to use as the outcome
#' @param provider variable to use as the accountable entity
#' @param ctrPerf parameters to control performance measure calculation
#' @returns A list with the following components:
#'  \item{var.b.aov}{between-entity variance}
#' @returns The plot function can be used to plot the provider-level reliability estimates.
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aggregate setNames predict
#' @importFrom lme4 bootMer
#' @export

calcPerformance <- function(df = NULL, model = NULL, y, provider, ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')

  data.out <- calcDataSummary(df, model, y, provider, ctrPerf)
  df <- data.out$df
  model = data.out$model
  marg.p <- data.out$marg.p
  marg.p.model <- data.out$marg.p.model
  entity <- data.out$provider
  n  <- data.out$n
  obs <- data.out$obs
  p <- data.out$p
  p.lwr <- data.out$p.lwr
  p.upr <- data.out$p.upr
  p.re <- data.out$p.re
  pred <- data.out$pred
  exp <- data.out$exp
  rank <- data.out$rank

  oe      <- obs / exp
  pe      <- pred / exp
  rs.p    <- oe * marg.p
  rank.oe <- rank(oe, ties.method = "random")
  rank.pe <- rank(pe, ties.method = "random")
  pred.se <- sqrt(aggregate(predict.var ~ provider, data = df, sum)$predict.var)
  obs.se  <- sqrt(pred.se * (1 + 1/n))
  oe.lwr  <- (obs - 1.96*obs.se) / exp
  oe.upr  <- (obs + 1.96*obs.se) / exp

  # parametric bootstrapping for O/E estimates
  n.sims = ctrPerf$n.sims
  n.cores = ctrPerf$n.cores
  g <- function(x) {
    data <- x@frame
    data$y <- data[[y]]
    data$provider <- data[[provider]]
    data$expect <- predict(x, newdata = data, type = 'response', re.form = ~0)
    data$predict <- predict(x, newdata = data, type = 'response')
    marg.p  <- mean(data$y)
    obs     <- aggregate(y ~ provider, data = data, sum)$y
    pred    <- aggregate(predict ~ provider, data = data, sum)$predict
    exp     <- aggregate(expect ~ provider, data = data, sum)$expect
    oe      <- obs / exp
    pe      <- pred / exp
    rs.p    <- oe * marg.p
    return(oe)
  }
  b <- lme4::bootMer(model, FUN = g, seed = 110, nsim = n.sims,
               use.u = TRUE, parallel = 'multicore', ncpus = n.cores)
  bootmer_res <- t(apply(as.data.frame(b), 2, quantile, c(0.5, 0.025, 0.975)))
  bootmer_res <- setNames(as.data.frame(bootmer_res), c('est', 'lwr', 'upr'))

  perf.results <- data.frame(
    provider = entity,
    n = n,
    observed = obs,
    predicted = pred,
    expected = exp,
    p = p,
    p.lwr = p.lwr,
    p.upr = p.upr,
    p.re = p.re,
    rank = rank,
    oe = oe,
    oe.lwr = oe.lwr,
    oe.upr = oe.upr,
    oe.boot = bootmer_res$est,
    oe.boot.lwr = bootmer_res$lwr,
    oe.boot.upr = bootmer_res$upr,
    pe = pe,
    rank.oe = rank.oe,
    rank.pe = rank.pe,
    rs.p = rs.p
  )
  output = list(df = df, model = model, marg.p = marg.p, marg.p.model = marg.p.model, perf.results = perf.results)

  return(output)
}
