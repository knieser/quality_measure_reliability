analyze_subsamples <- function(df, type, q.range, n.splits, n.resamples, n.draws, n.cores){
  
  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)
  
  names(df) <- c("id", "site", "y")
  sites = unique(df$site)
  
  results.median = array(data = NA, dim = c(14, length(q.range), n.draws))
  results.a.mean = array(data = NA, dim = c(14, length(q.range), n.draws))
  results.h.mean = array(data = NA, dim = c(14, length(q.range), n.draws))
  
  for (q in 1:length(q.range)){
    
    # if q = 100%, we only need to calculate reliability once
    if (q == length(q.range)){
      out.varying.sample <- calcReliability(
        df,
        type = type,
        reps = n.splits,
        resamples = n.resamples,
        n.cores = n.cores)
      
      results.median[,q,] = rep(out.varying.sample$results$median.estimate, n.draws)
      results.a.mean[,q,] = rep(out.varying.sample$results$arithmetic.mean.estimate, n.draws)
      results.h.mean[,q,] = rep(out.varying.sample$results$harmonic.mean.estimate, n.draws)
      
    }
    else {
      
      r <- foreach::foreach(k = 1:n.draws, .combine = cbind, .packages = c('lme4', 'psych')) %dopar% {
        library(doParallel)
        library(foreach)
        source('reliability_functions.R')
        source('calcSSR.R')
        
        # sample a fraction of data frame
        df.reduced.sample = data.frame(data = NA)
        for (i in 1:length(sites)){
          site.df <- df[df$site == sites[i], ]
          site.df.reduced = site.df[sample(nrow(site.df), q.range[q]*nrow(site.df), replace = F),]
          if (i == 1){df.reduced <- site.df.reduced} else {df.reduced <- rbind(df.reduced, site.df.reduced)}
        }
        
        # calculate reliability on reduced data frame
        out.varying.sample <- calcReliability(
          df = df.reduced,
          type = type,
          reps = n.splits,
          resamples = n.resamples,
          n.cores = n.cores)
        
        c(out.varying.sample$results$median.estimate,
          out.varying.sample$results$arithmetic.mean.estimate,
          out.varying.sample$results$harmonic.mean.estimate)
      }
      
      results.median[,q,] = r[1:14]
      results.a.mean[,q,] = r[15:28]
      results.h.mean[,q,] = r[29:42]
    }
  }
  parallel::stopCluster(cl)
  
  mean.results.median = apply(results.median, c(1,2), mean)
  mean.results.a.mean = apply(results.a.mean, c(1,2), mean)
  mean.results.h.mean = apply(results.h.mean, c(1,2), mean)
  
  return(list(mean.results.median = mean.results.median,
              mean.results.a.mean = mean.results.a.mean,
              mean.results.h.mean = mean.results.h.mean)
  )
}