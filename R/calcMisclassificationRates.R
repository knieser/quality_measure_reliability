#' Calculate misclassification probabilities
#' @description
#' This function runs the misclassification functions
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
#' @export

calcMisclassificationRates <- function(df = NULL, model = NULL, entity = "entity", y = "y", ctrPerf = controlPerf(), percentile.misclassification.fig.file, misclassification.oe.fig.file, misclassification.pe.fig.file){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')

  data.out <- calcDataSummary(df, model, entity, y, ctrPerf)
  df = data.out$df
  model = data.out$model
  fit = data.out$fit
  marg.p <- data.out$marg.p
  marg.p.model <- data.out$marg.p.model
  entities <- data.out$entities
  n       <- data.out$n
  obs     <- data.out$obs
  p       <- data.out$p
  p.re    <- data.out$p.re
  pred    <- data.out$pred
  exp     <- data.out$exp
  rank    <- data.out$rank
  oe      <- obs / exp
  pe      <- pred / exp
  rs.oe   <- oe * marg.p
  rs.pe   <- pe * marg.p
  rank.oe <- rank(oe, ties.method = "random")
  rank.pe <- rank(pe, ties.method = "random")

  # these are what we will assume are the true ranks
  entity.results <- data.frame(
    entities = entities,
    n = n,
    observed = obs,
    predicted = pred,
    expected = exp,
    p = p,
    p.re = p.re,
    oe = oe,
    pe = pe,
    rs.oe = rs.oe,
    rs.pe = rs.pe,
    rank.p = rank,
    rank.oe = rank.oe,
    rank.pe = rank.pe
    )

  # obtain bootstrapped ranks
  output <- parametricBootstrap(df, model, entities, entity, y, ctrPerf)
  oe.boot = output$oe.boot
  pe.boot = output$pe.boot
  rs.oe.boot = output$rs.oe.boot
  rs.pe.boot = output$rs.pe.boot
  rs.direct.boot = output$rs.direct.boot

  percentile.misclassification.oe <- plotPercentileMisclassification(entities, rs.oe, rs.oe.boot, out.file = percentile.misclassification.fig.file)
  misclassification.oe <- plotMisclassification(entities, rs.oe, rs.oe.boot, 4, misclassification.oe.fig.file)
  misclassification.pe <- plotMisclassification(entities, rs.pe, rs.pe.boot, 4, misclassification.pe.fig.file)

  results = list(percentile.misclassification.oe = percentile.misclassification.oe,
                 misclassification.oe = misclassification.oe,
                 misclassification.pe = misclassification.pe)
}
