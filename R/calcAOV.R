#' Calculate reliability using one-way ANOVA method
#'
#' @description
#' This function estimates reliability using the one-way ANOVA method.
#' @details
#' This function uses the `aov()` function from the `stats` package to calculate mean squares between
#' and mean squares within entities.
#'
#' @param df dataframe (assumed to be observation-level unless `df.aggregate` is changed below)
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param df.aggregate set this to `TRUE` if the data have already been aggregated to include only summary data (sample size, means, and standard deviations) for each entity; default is `FALSE`.
#' @param n if using aggregated data, data column containing the sample sizes for each entity; default is `n`.
#' @param mean if using aggregated data, data column containing the sample means for each entity; default is `mean`.
#' @param std.dev if using aggregated data, data column containing the sample standard deviations for each entity entity; default is `sd`.
#' @param ctrPerf parameters to control performance measure calculation
#' @returns A list containing:
#' * `MSB`: mean squares between entities
#' * `MSW`: mean squares within entities
#' * `F.stat`: F-statistic from one-way ANOVA
#' * `entity`: list of entities
#' * `n`: sample sizes for each entity
#' * `n0`: aggregate sample size used in reliability calculation
#' * `var.b.aov`: between-entity variance
#' * `var.w.aov`: within-entity variance
#' * `est.aov`: entity-level reliability estimates
#'
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @examples
#' # Simulate data
#' df <- simulateData(n.entity = 50, n.obs = 100, mu = 25, r = .7, data.type = 'normal')
#'
#' # Calculate reliability
#' out <- calcAOV(df = df, entity = 'entity', y = 'y')
#' summary(out$est.aov)
#'
#' # Plot reliability by entity sample size
#' plot(out$n, out$est.aov)
#'
#'
#' ## Reliability can also be calculated with data aggregated by entity
#' df.agg <- data.frame(
#'           entity = aggregate(y ~ entity, data = df, length)$entity,
#'           n = aggregate(y ~ entity, data = df, length)$y,
#'           mean = aggregate(y ~ entity, data = df, mean)$y,
#'           sd = aggregate(y ~ entity, data = df, sd)$y
#'           )
#'
#' out2 <- calcAOV(df = df.agg, df.aggregate = T, n = 'n', mean = 'mean', std.dev = 'sd')
#' summary(out2$est.aov)
#'
#' @importFrom stats aov
#' @export

calcAOV <- function(df, entity = 'entity', y = 'y', df.aggregate = FALSE, n = 'n', mean = 'mean', std.dev = 'sd', ctrPerf = controlPerf()){
  cl <- match.call()

  if (isFALSE(df.aggregate)){
    df <- cleanData(df, entity, y, ctrPerf)
    agg  <- aggregate(y ~ entity, data = df, length)
    n <- agg$y
    entities <- agg$entity

    aov.out <- aov(y ~ entity, data = df)
    aov.summary <- matrix(unlist(summary(aov.out)), ncol=2, byrow = T)
    MSB = aov.summary[3,1]
    MSW = aov.summary[3,2]
    } else{
    n <- df[[n]]
    entities <- df[[entity]]
    mu <- df[[mean]]
    sd <- df[[std.dev]]

    m = nrow(df) # number of entities
    n.total = sum(n)
    grand.mean = 1 / n.total * sum(n * mu)
    MSB = 1 / (m - 1) * sum(n * (mu - grand.mean)^2)
    MSW = 1 / (n.total - m) * sum((n-1)*sd^2)
  }

  n0 <- 1 / (length(n) - 1) * (sum(n) - sum(n^2) / sum(n))
  F.stat = MSB / MSW
  var.b.aov = (MSB - MSW) / n0
  var.w.aov = MSW / n
  est.aov   = var.b.aov / (var.b.aov + var.w.aov)

  output = list(call = cl,
                MSB = MSB,
                MSW = MSW,
                F.stat = F.stat,
                entity = as.vector(entities),
                n = n,
                n0 = n0,
                var.b.aov = var.b.aov,
                var.w.aov = var.w.aov,
                est.aov = est.aov
                )

  return(output)
}
