#' Calculate measure performance by accountable entity
#' @description
#' This function calculates measure performance by accountable entity.
#' @param df dataframe; if null, will use the dataframe in the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity variable to use as the accountable entity; default = "entity"
#' @param y variable to use as the outcome; default = "y"
#' @param ctrPerf parameters to control performance measure calculation
#' @returns Estimated measure performance by accountable entity
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references None
#' @examples
#' # TBD
#' @importFrom stats aggregate setNames predict
#' @importFrom lme4 bootMer
#' @export

calcPerformance <- function(df = NULL, model = NULL, entity = "entity", y = "y", ctrPerf = controlPerf()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  df <- data.out$df
  model = data.out$model
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

  oe      <- obs / exp
  pe      <- pred / exp
  rs      <- oe * marg.p
  rank.oe <- rank(oe, ties.method = "random")
  rank.pe <- rank(pe, ties.method = "random")
  rank.rs <- rank(rs, ties.method = "random")
  pred.se <- sqrt(aggregate(predict.var ~ entity, data = df, sum)$predict.var)
  obs.se  <- sqrt(pred.se * (1 + 1/n))
  oe.lwr  <- (obs - 1.96*obs.se) / exp
  oe.upr  <- (obs + 1.96*obs.se) / exp

  # parametric bootstrapping for O/E estimates
  n.sims = ctrPerf$n.sims
  n.cores = ctrPerf$n.cores
  g <- function(x) {
    data <- x@frame
    data$y <- data[[y]]
    data$entity <- data[[entity]]
    data$expect <- predict(x, newdata = data, type = 'response', re.form = ~0)
    data$predict <- predict(x, newdata = data, type = 'response')
    marg.p <- mean(data$y)
    obs    <- aggregate(y ~ entity, data = data, sum)$y
    pred   <- aggregate(predict ~ entity, data = data, sum)$predict
    exp    <- aggregate(expect ~ entity, data = data, sum)$expect
    oe     <- obs / exp
    pe     <- pred / exp
    rs     <- oe * marg.p
    out    <- c(oe, pe, rs)
    return(out)
  }
  b <- lme4::bootMer(model, FUN = g, nsim = n.sims, use.u = TRUE, parallel = 'multicore', ncpus = n.cores)
  oe.boot <- as.data.frame(b$t[,1:length(n)])
  pe.boot <- as.data.frame(b$t[,(length(n) + 1):(2*length(n))])
  rs.boot <- as.data.frame(b$t[,(2*length(n)+1):ncol(b$t)])

  bootmer.res.oe <- t(apply(oe.boot, 2, quantile, c(0.5, 0.025, 0.975)))
  bootmer.res.pe <- t(apply(pe.boot, 2, quantile, c(0.5, 0.025, 0.975)))
  bootmer.res.rs <- t(apply(rs.boot, 2, quantile, c(0.5, 0.025, 0.975)))

  bootmer.res.oe <- setNames(as.data.frame(bootmer.res.oe), c('est', 'lwr', 'upr'))
  bootmer.res.pe <- setNames(as.data.frame(bootmer.res.pe), c('est', 'lwr', 'upr'))
  bootmer.res.rs <- setNames(as.data.frame(bootmer.res.rs), c('est', 'lwr', 'upr'))

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
    rank = rank,
    oe = oe,
    oe.lwr = oe.lwr,
    oe.upr = oe.upr,
    oe.boot.lwr = bootmer.res.oe$lwr,
    oe.boot.upr = bootmer.res.oe$upr,
    rank.oe = rank.oe,
    pe = pe,
    pe.lwr = bootmer.res.pe$lwr,
    pe.upr = bootmer.res.pe$upr,
    rank.pe = rank.pe,
    rs = rs,
    rs.lwr = bootmer.res.rs$lwr,
    rs.upr = bootmer.res.rs$upr,
    rank.rs = rank.rs
  )
  output = list(df = df, model = model, marg.p = marg.p, marg.p.model = marg.p.model, perf.results = perf.results)

  return(output)
}
