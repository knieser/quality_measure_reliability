#' Calculate reliability using split-sample method
#' @description
#' This function estimates reliability using the split-sample method.
#' @param df observation-level data; if null, will use the dataframe from the model object
#' @param model model; if null, will use an unadjusted model
#' @param entity data column containing the accountable entity identifier
#' @param y data column containing the outcome variable
#' @param data.type acceptable values are "binary" for 0/1 data and "continuous" for continuous data (default: 'binary')
#' @param ctrPerf parameters to control performance measure calculation
#' @param ctrRel parameters to control reliability estimation
#' @returns SSR reliability estimates
#' @author Kenneth Nieser (nieser@stanford.edu)
#' @references Nieser KJ, Harris AH. Split‐sample reliability estimation in health care quality measurement: Once is not enough. Health Services Research. 2024 Aug;59(4):e14310.
#' @references Nieser KJ, Harris AH. Comparing methods for assessing the reliability of health care quality measures. Statistics in Medicine. 2024 Oct 15;43(23):4575-94.
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom foreach foreach
#' @importFrom psych ICC
#' @export

calcSSR <- function(df = NULL, model = NULL, entity = 'entity', y = 'y', data.type = 'binary', ctrPerf = controlPerf(), ctrRel = controlRel()){
  if (is.null(df) & is.null(model)) stop ('Please provide either a dataframe or a model object')
  if (is.null(df)){df <- model@frame}

  fn <- ctrRel$fn
  if(is.na(fn)){fn = function(x){mean(x)}}
  cl <- match.call()
  n.cores     <- ctrPerf$n.cores
  n.resamples <- ctrRel$n.resamples
  method      <- ctrRel$SSRmethod

  data.out <- calcDataSummary(df, model, entity, y, data.type, ctrPerf)
  df <- data.out$df

  entities = unique(df$entity)
  n.entity = length(entities)

  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)

  out <- foreach::foreach(s = 1:n.resamples, .combine = rbind, .packages = c('psych')) %dopar% {

  if (method=='permutation'){
    # randomly assign each record into either s=1 or s=2 for each entity
    df$s <- 1
    for (j in 1:n.entity){
      entity.df <- df[df$entity == entities[j], ]
      entity.df$s[sample(nrow(entity.df), nrow(entity.df)/2, replace = F)] <- 2
      df$s[df$entity == entities[j]] <- entity.df$s
      }
    df$s <- as.factor(df$s)

    # calculate performance by entity and split-half
    entity.means <- aggregate(y ~ entity + s, data = df, function(x) fn(x))
    entity.means.wide <- reshape(entity.means, idvar = "entity", timevar = "s", direction = "wide")

    agg   <- aggregate(y ~ entity + s, data = df, sum)
    agg$obs   <- agg$y

    if (data.type == 'binary'){
      agg$pred  <- aggregate(predict ~ entity + s, data = df, sum)$predict
      agg$exp   <- aggregate(expect ~ entity + s, data = df, sum)$expect
      agg$oe    <- agg$obs / agg$exp
      agg$pe    <- agg$pred / agg$exp
    }
    entity.means.wide.adj <- reshape(agg, idvar = "entity", timevar = "s", direction = "wide")

    # calculate ICCs using psych package
    aov.out  <- psych::ICC(entity.means.wide[,-1], lmer = F)
    aov.ICC = aov.out$results$ICC
    aov.ICC.lb = aov.out$results$`lower bound`
    aov.ICC.ub = aov.out$results$`upper bound`
    aov.summary = matrix(unlist(aov.out$summary), ncol = 3, byrow = T)
    aov.var.w = (aov.summary[2,2] + aov.summary[2,3])/(aov.summary[1,2] + aov.summary[1,3])
    aov.var.b = (aov.summary[3,1] - aov.var.w) / 2

    if (data.type == 'binary'){
      aov.out.oe  <- psych::ICC(entity.means.wide.adj[,c('oe.1', 'oe.2')], lmer = F)
      aov.ICC.oe = aov.out.oe$results$ICC
      aov.ICC.oe.lb = aov.out.oe$results$`lower bound`
      aov.ICC.oe.ub = aov.out.oe$results$`upper bound`
      aov.oe.summary = matrix(unlist(aov.out.oe$summary), ncol = 3, byrow = T)
      aov.oe.var.w = (aov.oe.summary[2,2] + aov.oe.summary[2,3])/(aov.oe.summary[1,2] + aov.oe.summary[1,3])
      aov.oe.var.b = (aov.oe.summary[3,1] - aov.oe.var.w) / 2

      aov.out.pe  <- psych::ICC(entity.means.wide.adj[,c('pe.1', 'pe.2')], lmer = F)
      aov.ICC.pe = aov.out.pe$results$ICC
      aov.ICC.pe.lb = aov.out.pe$results$`lower bound`
      aov.ICC.pe.ub = aov.out.pe$results$`upper bound`
      aov.pe.summary = matrix(unlist(aov.out.pe$summary), ncol = 3, byrow = T)
      aov.pe.var.w = (aov.pe.summary[2,2] + aov.pe.summary[2,3])/(aov.pe.summary[1,2] + aov.pe.summary[1,3])
      aov.pe.var.b = (aov.pe.summary[3,1] - aov.pe.var.w) / 2
    }
  }

  if(method == 'bootstrap'){
    # resample data within each entity with replacement
    df.boots <- data.frame(matrix(ncol = 4, nrow = 0))
    names(df.boots) <- c('id', 'entity', 'boot', 'y')

    for (j in 1:n.entity){
      entity.df <- df[df$entity == entities[j], ]
      entity.boots <- replicate(boots, {entity.df$y[sample(nrow(entity.df), nrow(entity.df), replace = T)]})
      df.boots <- rbind(df.boots,
                        data.frame(id = rep(entity.df$id, boots),
                                   entity = rep(entities[j], boots),
                                   boot = rep(1:boots, each = nrow(entity.df)),
                                   y = c(entity.boots)
                                   )
                        )
    }
    entity.means <- aggregate(y ~ entity + boot, data = df.boots, function(x) fn(x))
    entity.means.wide <- reshape(entity.means, idvar = "entity", timevar = "boot", direction = "wide")
  }

  if (method == 'permutation'){
    icc.aov       = c(aov.ICC[4]) # Spearman-Brown correction
    icc.aov.lb    = c(aov.ICC.lb[4])
    icc.aov.ub    = c(aov.ICC.ub[4])

    icc.aov.oe = NA
    icc.aov.oe.lb = NA
    icc.aov.oe.ub = NA
    aov.oe.var.b = NA
    aov.oe.var.w = NA
    icc.aov.pe = NA
    icc.aov.pe.lb = NA
    icc.aov.pe.ub = NA
    aov.pe.var.b = NA
    aov.pe.var.w = NA

    if (data.type == 'binary'){
      icc.aov.oe    = c(aov.ICC.oe[4])
      icc.aov.oe.lb = c(aov.ICC.oe.lb[4])
      icc.aov.oe.ub = c(aov.ICC.oe.ub[4])
      icc.aov.pe    = c(aov.ICC.pe[4])
      icc.aov.pe.lb = c(aov.ICC.pe.lb[4])
      icc.aov.pe.ub = c(aov.ICC.pe.ub[4])
    }

    list(icc.aov       = icc.aov,
         icc.aov.lb    = icc.aov.lb,
         icc.aov.ub    = icc.aov.ub,
         var.b.aov     = aov.var.b,
         var.w.aov     = aov.var.w,
         icc.aov.oe    = icc.aov.oe,
         icc.aov.oe.lb = icc.aov.oe.lb,
         icc.aov.oe.ub = icc.aov.oe.ub,
         var.b.aov.oe  = aov.oe.var.b,
         var.w.aov.oe  = aov.oe.var.w,
         icc.aov.pe    = icc.aov.pe,
         icc.aov.pe.lb = icc.aov.pe.lb,
         icc.aov.pe.ub = icc.aov.pe.ub,
         var.b.aov.pe  = aov.pe.var.b,
         var.w.aov.pe  = aov.pe.var.w
    )
  } else {
    icc.aov = c(aov.ICC[1]) # no Spearman-Brown correction
    icc.aov.lb = c(aov.ICC.lb[1])
    icc.aov.ub = c(aov.ICC.ub[1])
  }
  }
  parallel::stopCluster(cl)

  out <- as.data.frame(out)

  if (data.type == 'binary'){
    output = list(
      call = cl,
      icc = as.vector(unlist(out$icc.aov)),
      icc.lb = as.vector(unlist(out$icc.aov.lb)),
      icc.ub = as.vector(unlist(out$icc.aov.ub)),
      var.b.aov = as.vector(unlist(out$var.b.aov)),
      var.w.aov = as.vector(unlist(out$var.w.aov)),
      icc.oe = as.vector(unlist(out$icc.aov.oe)),
      icc.oe.lb = as.vector(unlist(out$icc.aov.oe.lb)),
      icc.oe.ub = as.vector(unlist(out$icc.aov.oe.ub)),
      var.b.aov.oe = as.vector(unlist(out$var.b.aov.oe)),
      var.w.aov.oe = as.vector(unlist(out$var.w.aov.oe)),
      icc.pe = as.vector(unlist(out$icc.aov.pe)),
      icc.pe.lb = as.vector(unlist(out$icc.aov.pe.lb)),
      icc.pe.ub = as.vector(unlist(out$icc.aov.pe.ub)),
      var.b.aov.pe = as.vector(unlist(out$var.b.aov.pe)),
      var.w.aov.pe = as.vector(unlist(out$var.w.aov.pe))
    )
    output$est.SSR = output$icc[1]
    output$est.PSSR = mean(output$icc)
    output$est.SSR.oe = output$icc.oe[1]
    output$est.PSSR.oe = mean(output$icc.oe)
    output$est.SSR.pe  <- output$icc.pe[1]
    output$est.PSSR.pe <- mean(output$icc.pe)
  }

  if (data.type == 'continuous'){
    output = list(
      call = cl,
      icc = as.vector(unlist(out$icc.aov)),
      icc.lb = as.vector(unlist(out$icc.aov.lb)),
      icc.ub = as.vector(unlist(out$icc.aov.ub)),
      var.b.aov = as.vector(unlist(out$var.b.aov)),
      var.w.aov = as.vector(unlist(out$var.w.aov))
    )
    output$est.SSR = output$icc[1]
    output$est.PSSR = mean(output$icc)
  }


  return(output)
}
