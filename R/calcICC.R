#' @importFrom stats qf

calcICC <- function(df, SB = TRUE, alpha = 0.05){
  x1 = df[,1]
  x2 = df[,2]
  n = nrow(df)

  df <- data.frame(
    grp = rep(c(1:n), 2),
    y = c(x1, x2)
    )
  df$grp <- as.factor(df$grp)

  aov.out <- aov(y ~ grp, data = df)
  aov.summary <- matrix(unlist(summary(aov.out)), ncol=2, byrow = T)
  MSB = aov.summary[3,1]
  MSW = aov.summary[3,2]
  df.n = aov.summary[1,1]
  df.d = aov.summary[1,2]
  ICC = (MSB - MSW) / (MSB + MSW)
  F.stat = MSB / MSW
  F.lwr <- F.stat / qf(1 - alpha/2, df.n, df.d)
  F.upr <- F.stat * qf(1 - alpha/2, df.d, df.n)
  ICC.lwr <- (F.lwr - 1)/(F.lwr + 1)
  ICC.upr <- (F.upr - 1)/(F.upr + 1)

  if (SB == TRUE){
    ICC = 2*ICC / (1 + ICC)
    ICC.lwr <- 2*ICC.lwr / (1 + ICC.lwr)
    ICC.upr <- 2*ICC.upr / (1 + ICC.upr)
  }

  results <- list(ICC = ICC, ICC.lwr = ICC.lwr, ICC.upr = ICC.upr)
  return(results)
}

