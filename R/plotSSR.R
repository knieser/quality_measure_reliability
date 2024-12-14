SSRplot <- function(df0, m, y, provider){
  
  df0$y <- df0[[y]]
  df0$provider <- df0[[provider]]
  
  # drop providers with less than 2 observations
  sample.size <- aggregate(y ~ provider, data = df0, length)
  small.providers <- sample.size[sample.size[,2] < 2, 1]
  if(length(small.providers) > 0){cat(paste0('Dropping providers with not enough data...', small.providers, '\n'))}
  df <- df0[!(df0$provider %in% small.providers), ]
  
  df$provider = factor(df$provider)
  providers = unique(df$provider)
  n.providers = length(providers)
  
  df$expect <- predict(m, newdata = df, type = 'response', re.form = ~0)
  df$predict <- predict(m, newdata = df, type = 'response')
  
  # split samples
  df$s <- 1
  for (j in 1:n.providers){
    provider.df <- df[df$provider == providers[j], ]
    provider.df$s[sample(nrow(provider.df), nrow(provider.df)/2, replace = F)] <- 2
    df$s[df$provider == providers[j]] <- provider.df$s
  }
  df$s <- as.factor(df$s)
  
  # calculate risk standardized metrics in each half-sample
  agg    <- aggregate(y ~ provider + s, data = df, sum)
  agg$obs    <- agg$y
  agg$pred   <- aggregate(predict ~ provider + s, data = df, sum)$predict
  agg$exp    <- aggregate(expect ~ provider + s, data = df, sum)$expect
  agg$oe <- agg$obs / agg$exp  
  agg$pe <- agg$pred / agg$exp 
  
  # pivot from long to wide format
  provider.means.wide <- reshape(agg, idvar = "provider", timevar = "s", direction = "wide")
  
  # make plot dataframe
  rsSSR.plot.df <- data.frame(
    method = rep(c('OE', 'PE'), each = nrow(provider.means.wide)),
    splithalf1 = c(provider.means.wide$oe.1, provider.means.wide$pe.1),
    splithalf2 = c(provider.means.wide$oe.2, provider.means.wide$pe.2)
  )
  
  # make plot
  rsSSR.fig <- ggplot(data = rsSSR.plot.df, 
                      aes(x = splithalf1, y = splithalf2, group = method)) +
    geom_point(aes(color = method), size = 2.5 ) +
    geom_abline(slope = 1, intercept = 0, lty = 2) +
    theme_classic() +
    theme(
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      strip.text = element_text(size = 18, face = "bold"),
      panel.grid.major = element_line(),
      panel.grid.minor = element_line(),
      legend.position = 'top'
    )
  rsSSR.fig
}