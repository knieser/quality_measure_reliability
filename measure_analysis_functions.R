
calcPerformance <- function(df){
  
  names(df) <- c('id', 'provider', 'y')
  n.pts = aggregate(id ~ provider, data = df, length)$id
  provider.df <- data.frame(
    provider = unique(df$provider)
  )

  # calculate performance by facility
  n = aggregate(y ~ provider, data = df, length)
  x = aggregate(y ~ provider, data = df, sum)
  provider.df2 <- merge(provider.df, n, by = "provider")
  provider.df3 <- merge(provider.df2, x, by = "provider")
  names(provider.df3) <- c('provider', 'n', 'x')
  provider.df3$p = provider.df3$x/provider.df3$n
  z.test = qnorm(0.025)
  provider.df3$lwr = (provider.df3$p + z.test^2/(2*provider.df3$n) + 
    z.test * sqrt(provider.df3$p * (1 - provider.df3$p) / provider.df3$n + z.test^2/(4*provider.df3$n^2)))/
    (1 + z.test^2/provider.df3$n)
  provider.df3$upr = (provider.df3$p + z.test^2/(2*provider.df3$n) -
    z.test * sqrt(provider.df3$p * (1 - provider.df3$p) / provider.df3$n + z.test^2/(4*provider.df3$n^2)))/
    (1 + z.test^2/provider.df3$n)
  
  provider.df3$lwr[provider.df3$x == 1] <- -log(0.975)/provider.df3$n[provider.df3$x==1]
  provider.df3$upr[provider.df3$x == (provider.df3$n - 1)] <-  1 + log(0.975)/provider.df3$n[provider.df3$x == (provider.df3$n - 1)]
  provider.df3$rank <- rank(provider.df3$p, ties.method = 'random')
  
  return(provider.df3)
}


# histogram of provider sample sizes
plotN <- function(n){
  df <- data.frame(n)
  fig <- ggplot(data = df, aes(n)) +
    geom_histogram(binwidth = 100, color = 'white', fill = 'black') +
    scale_y_continuous(expand = c(0,0)) +
    xlab('Number of cases') +
    ylab('Providers') +
    ggtitle('Sample size distribution') +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text.y = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.text.x = element_text(size = 16),
      axis.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      legend.position = 'top',
      legend.title = element_blank(),
      legend.text = element_text(size = 18)
    )
  return(fig)
}

analyze_subsamples <- function(df, type, q.range, n.splits, n.resamples, n.draws, n.cores){
  
  cl <- parallel::makeCluster(n.cores)
  doParallel::registerDoParallel(cl)
  
  names(df) <- c("id", "site", "y")
  sites = unique(df$site)
  
  results = array(data = NA, dim = c(11, length(q.range), n.draws))
  
  for (q in 1:length(q.range)){
    
    # if q = 100%, we only need to calculate reliability once
    if (q == length(q.range)){
      out.varying.sample <- calcReliability(
        df,
        type = type,
        reps = n.splits,
        resamples = n.resamples,
        n.cores = n.cores)
      
      results[,q,] = rep(out.varying.sample$estimate, n.draws)
    }
    else {
      
      r <- foreach::foreach(k = 1:n.draws, .combine = c, .packages = c('lme4', 'psych')) %dopar% {
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
        
        out.varying.sample$estimate
      }
      
      results[,q,] = matrix(r, nrow = 11, ncol = n.draws)
    }
  }
  parallel::stopCluster(cl)
  
  mean.results = apply(results, c(1,2), mean)
  
  return(mean.results)
}
