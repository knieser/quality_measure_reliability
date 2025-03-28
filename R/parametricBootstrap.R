parametricBootstrap <- function(df, model, entities, entity, y, ctrPerf){
  n.boots = ctrPerf$n.boots
  n.cores = ctrPerf$n.cores
  df.pb = df

  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)

  out <- foreach::foreach(s = 1:n.boots, .combine = rbind, .packages = c('lme4')) %dopar% {
    estOEPE <- function(df, model, entities, entity, y){
      df$y         <- df[[y]]
      df$entity    <- df[[entity]]

      # refit model
      fit <- lme4::glmer(model, data = df, family = 'binomial', control = glmerControl(optimizer = 'bobyqa'), nAGQ = 0)

      # calculate predictions w/ and w/o random intercept
      df$expect    <- predict(fit, newdata = df, type = 'response', re.form = ~0)
      df$predict   <- predict(fit, newdata = df, type = 'response')

      # calculate oe and pe ratios
      n      <- aggregate(y ~ entity, data = df, length)$y
      obs    <- aggregate(y ~ entity, data = df, sum)$y
      pred   <- aggregate(predict ~ entity, data = df, sum)$predict
      exp    <- aggregate(expect ~ entity, data = df, sum)$expect
      p      <- obs / n
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

      out <- list(p = p, oe = oe, pe = pe, rs.oe = rs.oe, rs.pe = rs.pe, rs.direct = rs.direct)
      return(out)
    }

    df.pb[[y]] = rbinom(nrow(df.pb), 1, df.pb$predict)
    est.boot <- estOEPE(df.pb, model, entities, entity, y)
    list(
      p.boot = est.boot$p,
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
    entities = entities,
    p.boot = matrix(unlist(out$p.boot), nrow = length(entities), ncol = n.boots),
    oe.boot = matrix(unlist(out$oe.boot), nrow = length(entities), ncol = n.boots),
    pe.boot = matrix(unlist(out$pe.boot), nrow = length(entities), ncol = n.boots),
    rs.oe.boot = matrix(unlist(out$rs.oe.boot), nrow = length(entities), ncol = n.boots),
    rs.pe.boot = matrix(unlist(out$rs.pe.boot), nrow = length(entities), ncol = n.boots),
    rs.direct.boot = matrix(unlist(out$rs.direct.boot), nrow = length(entities), ncol = n.boots)
  )
  return(output)
}
