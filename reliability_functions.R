calcBetaBin <- function(n, x){

  # negative log-likelihood
  neg.loglik <- function(v){
    -sum(lgamma(v[1] + v[2]) - lgamma(v[1]) - lgamma(v[2]) +
        lchoose(n, x) + lgamma(v[1] + x) + lgamma(v[2] + n - x) - 
        lgamma(v[1] + v[2] + n))
  }
   
  # calculate method of moments estimates for starting values
  m1 = mean(x) 
  m2 = mean(x^2)
  median.n = mean(n)
  a.mom = (median.n*m1 - m2) / (median.n *(m2/m1 - m1 - 1) + m1)
  b.mom = ((median.n - m1) * (median.n - m2/m1)) / (median.n*(m2/m1 - m1 - 1) + m1)
  a0 = max(a.mom, .5)
  b0 = max(b.mom, .5)
  
  # maximize negative log-likelihood
  theta = optim(c(a0, b0), neg.loglik, method = 'L-BFGS-B', lower = c(1e-6, 1e-6))$par
  a = theta[1]
  b = theta[2]
  
  # calculate variance ratio
  var.b = a * b / (a + b + 1) / (a + b)^2
  est = n / (a + b + n)
  
  results <- list(est.reliability = est,
                  var.b = var.b,
                  alpha = a, 
                  beta = b)
  return(results)
}

calcResamplingIUR <- function(df, resamples){
  
  providers = unique(df$provider)
  n.providers = length(providers)
  n.pts <- aggregate(id ~ provider, data = df, length)[,2]
  n0 = 1 / (n.providers - 1) * (sum(n.pts) - sum(n.pts^2) / sum(n.pts))
  
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
    provider.means[,j] = aggregate(y ~ provider, data = df.resample, mean)$y
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

calcReliability <- function(df0, type, reps, resamples, n.cores){
  
  # standardize column names
  names(df0) <- c("id", "provider", "y")
  
  n.providers = length(unique(df0$provider))
  sample.size <- aggregate(id ~ provider, data = df0, length)
  
  # drop providers with less than 2 observations
  small.providers <- sample.size[sample.size[,2] < 2, 1]
  if(length(small.providers) > 0){cat(paste0('Dropping providers with not enough data...', small.providers))}
  df <- df0[!(df0$provider %in% small.providers), ]
  
  df$provider = as.factor(df$provider)
  
  n.pts <- sample.size[sample.size[,2] >= 2, 2]
  n0 = 1 / (n.providers - 1) * (sum(n.pts) - sum(n.pts^2) / sum(n.pts))

  # initialize
  est.HLGM.model = NA
  est.HLGM.FE.model = NA
  est.HLGM.RE.model = NA
  est.HLGM = NA
  est.HLGM.FE = NA
  est.HLGM.RE = NA
  est.BB  = NA
  est.BB.FE = NA
  est.BB.RE = NA
  est.BB.J = NA
  
  # custom coded SSR
  SSR.out.PSSR <- calcSSR(df = df, reps = reps, method = 'permutation', n.cores = n.cores)
  var.b.PSSR = SSR.out.PSSR$var.b.aov
  var.w.PSSR = SSR.out.PSSR$var.w.aov
  est.PSSR = mean(SSR.out.PSSR$icc)
  
  # resampling IUR method from He et al 2019
  RIUR.results <- calcResamplingIUR(df, resamples)
  var.b.RIUR = RIUR.results$var.b
  var.w.RIUR = RIUR.results$var.w
  est.RIUR = RIUR.results$IUR
  
  # anova
  aov.out <- aov(y ~ provider, data = df)
  aov.summary <- matrix(unlist(summary(aov.out)), ncol=2, byrow = T)
  MSB = aov.summary[3,1]
  MSW = aov.summary[3,2]
  var.b.aov = (MSB - MSW) / n0
  var.w.aov = MSW 
  est.aov = var.b.aov / (var.b.aov + var.w.aov / n.pts)
  
  # hierarchical linear regression
  mod.HLM <- lme4::lmer(y ~ (1|provider), data = df)
  vc <- lme4::VarCorr(mod.HLM)
  var.b.HLM = vc$provider[1,1]
  var.w.HLM = (attributes(vc)$sc)^2 
  est.HLM = var.b.HLM / (var.b.HLM + var.w.HLM / n.pts)

  # hierarchical logistic regression
  if (type == 'binary'){
    mod.HLGM.RE <- lme4::glmer(y ~ (1|provider), data = df, family = 'binomial')
    var.b.HLGM <- lme4::VarCorr(mod.HLGM.RE)$provider[1,1]
    
    # calculate based on model
    marg.p.model <- plogis(mean(predict(mod.HLGM.RE, newdata = df, re.form = NA)))
    var.b.HLGM.pscale.model <- marg.p.model^2 * (1 - marg.p.model)^2 * var.b.HLGM
    var.w.HLGM.model = marg.p.model * (1 - marg.p.model) / n.pts
    est.HLGM.model = var.b.HLGM.pscale.model / (var.b.HLGM.pscale.model + var.w.HLGM.model)
    
    # use simple estimate for overall proportion
    marg.p <- mean(df$y)
    var.b.HLGM.pscale <- marg.p^2 * (1 - marg.p)^2 * var.b.HLGM
    var.w.HLGM = marg.p * (1 - marg.p) / n.pts
    est.HLGM = var.b.HLGM.pscale / (var.b.HLGM.pscale + var.w.HLGM)
    
    # within variance from random effects model
    logit.re <- coef(mod.HLGM.RE)$provider[,1]
    p.re <- 1 / (1 + exp(-logit.re))
    var.w.RE <- p.re * (1 - p.re) / n.pts
    est.HLGM.RE.model <- var.b.HLGM.pscale.model / (var.b.HLGM.pscale.model + var.w.RE)
    est.HLGM.RE <- var.b.HLGM.pscale / (var.b.HLGM.pscale + var.w.RE)
    
    # within variance from fixed effects model
    mod.HLGM.FE <- glm(y ~ provider, data = df, family = 'binomial')
    p.fe <- as.vector(predict.glm(mod.HLGM.FE, data.frame(provider = levels(df$provider)), type = 'response'))
    var.w.FE <- p.fe * (1 - p.fe) / n.pts
    est.HLGM.FE.model <- var.b.HLGM.pscale.model / (var.b.HLGM.pscale.model + var.w.FE)
    est.HLGM.FE <- var.b.HLGM.pscale / (var.b.HLGM.pscale + var.w.FE)
    
    # Beta-Binomial
    den <- aggregate(y ~ provider, data = df, length)$y
    num <- aggregate(y ~ provider, data = df, sum)$y
    out.betabin <- calcBetaBin(den, num)
    var.b.BB = out.betabin$var.b
    est.BB = out.betabin$est
    est.BB.FE = var.b.BB / (var.b.BB + var.w.FE)
    est.BB.RE = var.b.BB / (var.b.BB + var.w.RE)
    
    # within variance using Jeffreys prior
    p.J = (0.5 + num) / (1 + den)
    var.w.J = p.J * (1 - p.J) / n.pts
    est.BB.J = var.b.BB / (var.b.BB + var.w.J)
  }
  
  results = data.frame(
    method = c(
      'Perm-SSR', 
      'ANOVA',
      'Hier. Linear', 
      'Hier. Logit 1', 
      'Hier. Logit - FE 1',
      'Hier. Logit - RE 1',
      'Hier. Logit 2', 
      'Hier. Logit - FE 2',
      'Hier. Logit - RE 2',
      'Beta-Binomial',
      'Beta-Binomial - FE',
      'Beta-Binomial - RE',
      'Beta-Binomial - Jeffreys',
      'Resampling IUR'), 
    estimate = c(
      est.PSSR,
      median(est.aov),
      median(est.HLM), 
      median(est.HLGM.model), 
      median(est.HLGM.FE.model), 
      median(est.HLGM.RE.model), 
      median(est.HLGM), 
      median(est.HLGM.FE), 
      median(est.HLGM.RE), 
      median(est.BB),
      median(est.BB.FE),
      median(est.BB.RE),
      median(est.BB.J),
      est.RIUR
      )
  )
  
  return(results)
}