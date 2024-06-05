calcSSR <- function(df, reps, fn = NA, method, boots = NA, n.cores){
  
  names(df) <- c('id', 'provider', 'y')
  df$provider <- as.factor(df$provider)
  providers = unique(df$provider)
  n.providers = length(providers)
  
  if(is.na(fn)){fn = function(x){mean(x)}}
  
  if (method == 'bootstrap.alt'){
  boots = reps
   # resample data within each provider with replacement
      df.boots <- data.frame(matrix(ncol = 4, nrow = 0))
      names(df.boots) <- c('id', 'provider', 'boot', 'y')
      
      for (j in 1:n.providers){
        provider.df <- df[df$provider == providers[j], ]
        provider.boots <- replicate(boots, {provider.df$y[sample(nrow(provider.df), nrow(provider.df), replace = T)]})
        df.boots <- rbind(df.boots, data.frame(id = rep(provider.df$id, boots), 
                               provider = rep(providers[j], boots), 
                               boot = rep(1:boots, each = nrow(provider.df)), 
                               y = c(provider.boots)
                               )
                          )
      }
      provider.means <- aggregate(y ~ provider + boot, data = df.boots, function(x) fn(x)) 
      provider.means.wide <- reshape(provider.means, idvar = "provider", timevar = "boot", direction = "wide")
  
   aov.out  <- psych::ICC(provider.means.wide[,-1], lmer = F)
    aov.ICC = aov.out$results$ICC
    aov.ICC.lb = aov.out$results$`lower bound`
    aov.ICC.ub = aov.out$results$`upper bound`
    aov.summary = matrix(unlist(aov.out$summary), ncol = 3, byrow = T)
    aov.var.w = (aov.summary[2,2] + aov.summary[2,3])/(aov.summary[1,2] + aov.summary[1,3])
    aov.var.b = (aov.summary[3,1] - aov.var.w) / 2
	
      icc.aov = c(aov.ICC[1]) # ICC(1,1)
      icc.aov.lb = c(aov.ICC.lb[1])
      icc.aov.ub = c(aov.ICC.ub[1])
	  
  return(list(icc = icc.aov,
		  icc.lb = icc.aov.lb,
		  icc.ub = icc.aov.ub,
		  var.b.aov = aov.var.b,
		  var.w.aov =  aov.var.w)
		  )
  } else{
  
  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)
  
  out <- foreach::foreach(s = 1:reps, .combine = rbind, .packages = c('psych')) %dopar% {
    
    if (method=='permutation'){
    # randomly assign each provider into either s=1 or s=2
      df$s <- 1
      for (j in 1:n.providers){
        provider.df <- df[df$provider == providers[j], ]
        provider.df$s[sample(nrow(provider.df), nrow(provider.df)/2, replace = F)] <- 2
        df$s[df$provider == providers[j]] <- provider.df$s
      }
      df$s <- as.factor(df$s)
      
      provider.means <- aggregate(y ~ provider + s, data = df, function(x) fn(x)) 
      provider.means.wide <- reshape(provider.means, idvar = "provider", timevar = "s", direction = "wide")
    }
    
    if(method == 'bootstrap'){
    # resample data within each provider with replacement
      df.boots <- data.frame(matrix(ncol = 4, nrow = 0))
      names(df.boots) <- c('id', 'provider', 'boot', 'y')
      
      for (j in 1:n.providers){
        provider.df <- df[df$provider == providers[j], ]
        provider.boots <- replicate(boots, {provider.df$y[sample(nrow(provider.df), nrow(provider.df), replace = T)]})
        df.boots <- rbind(df.boots, data.frame(id = rep(provider.df$id, boots), 
                               provider = rep(providers[j], boots), 
                               boot = rep(1:boots, each = nrow(provider.df)), 
                               y = c(provider.boots)
                               )
                          )
      }
      provider.means <- aggregate(y ~ provider + boot, data = df.boots, function(x) fn(x)) 
      provider.means.wide <- reshape(provider.means, idvar = "provider", timevar = "boot", direction = "wide")
    }
    
    # calculate ICCs using psych package
    aov.out  <- psych::ICC(provider.means.wide[,-1], lmer = F)
    aov.ICC = aov.out$results$ICC
    aov.ICC.lb = aov.out$results$`lower bound`
    aov.ICC.ub = aov.out$results$`upper bound`
    aov.summary = matrix(unlist(aov.out$summary), ncol = 3, byrow = T)
    aov.var.w = (aov.summary[2,2] + aov.summary[2,3])/(aov.summary[1,2] + aov.summary[1,3])
    aov.var.b = (aov.summary[3,1] - aov.var.w) / 2
    
   if (method == 'permutation'){
      icc.aov = c(aov.ICC[4]) # Spearman-Brown correction
      icc.aov.lb = c(aov.ICC.lb[4])
      icc.aov.ub = c(aov.ICC.ub[4])
   } else {
      icc.aov = c(aov.ICC[1]) # no Spearman-Brown correction
      icc.aov.lb = c(aov.ICC.lb[1])
      icc.aov.ub = c(aov.ICC.ub[1])
      }
    
    list(icc.aov = icc.aov,
         icc.aov.lb = icc.aov.lb,
         icc.aov.ub = icc.aov.ub,
         var.b.aov = aov.var.b,
         var.w.aov = aov.var.w
    )
  }
  parallel::stopCluster(cl)
  
  out <- as.data.frame(out)
  
  return(list(icc = as.vector(unlist(out$icc.aov)),
              icc.lb = as.vector(unlist(out$icc.aov.lb)),
              icc.ub = as.vector(unlist(out$icc.aov.ub)),
              var.b.aov = as.vector(unlist(out$var.b.aov)),
              var.w.aov = as.vector(unlist(out$var.w.aov))
              )
         )
  }
  }