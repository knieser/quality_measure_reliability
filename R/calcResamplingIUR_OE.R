calcResamplingIUR.OE <- function(df, resamples){
  
  providers = unique(df$provider)
  n.providers = length(providers)
  n.pts <- aggregate(id ~ provider, data = df, length)[,2]
  n0 = 1 / (n.providers - 1) * (sum(n.pts) - sum(n.pts^2) / sum(n.pts))
  
  overall.logit <- glm(y ~ age + sex, data = df, family = 'binomial')
  df$expected <- predict.glm(overall.logit, df, type = 'response')
  
  gm = mean(df$y)
  provider.means0 = aggregate(y ~ provider, data = df, mean)$y
  var.total = 1/(n0*(n.providers - 1)) * sum(n.pts * (provider.means0 - gm)^2)
  
  provider.means = matrix(data = NA, nrow = n.providers, ncol = resamples)
  
  for (j in 1:resamples){
    # take a bootstrap resample within each provider separately
    df.resample = data.frame()
    for (i in 1:n.providers){
      provider.df <- df[df$provider == providers[i], ]
      provider.df.resample <- provider.df[sample(nrow(provider.df), nrow(provider.df), replace = T), ]
      df.resample <- rbind(df.resample, provider.df.resample)
    }
    
    # calculate measure by provider
    provider.obs <- aggregate(y ~ provider, data = df.resample, sum)
    provider.exp <- aggregate(expected ~ provider, data = df.resample, sum)
    provider.OE <- merge(provider.obs, provider.exp, by = 'provider')
    
    provider.means[,j] = provider.OE$y / provider.OE$expected
  }
  
  bootstrap.means = apply(provider.means, 1, mean)
  bootstrap.sqrd.resid = apply(provider.means, 2, function(x) (x - bootstrap.means)^2)
  bootstrap.var = 1/(resamples - 1) * apply(bootstrap.sqrd.resid, 1, sum)
  var.w = sum((n.pts - 1) * bootstrap.var) / (sum(n.pts) - n.providers)
  
  IUR = (var.total - var.w) / var.total
  
  results = list(var.b = var.total - var.w,
                 var.w = var.w,
                 var.total = var.total,
                 IUR = IUR)
  
  return(results)
}