## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(ggplot2)
library(doParallel)
library(lme4)
library(psych)

library(devtools)
load_all()
#devtools::install_github('knieser/quality_measure_reliability')
#library(QualityMeasure)


## -----------------------------------------------------------------------------
# number of accountable entities
n.entity = 100  

# average number of patients/cases per accountable entity
n.pts = 50

# parameters of the Beta distribution
alpha = 1
beta = 50

# sample the number of patients/cases per accountable entity
n = rpois(n.entity, n.pts) 
total.n = sum(n)

# sample the true performance for each entity
p = rbeta(n.entity, alpha, beta) 

# make sample data
entity = rep(1:n.entity, times = n)
y = rbinom(total.n, 1, rep(p, times = n))
df1 = data.frame(
  id = 1:total.n,
  entity = entity,
  y = y
)
head(df1)


## -----------------------------------------------------------------------------
# number of accountable entities
n.entity = 100  

# average number of patients/cases per accountable entity
avg.n = 50

# marginal probability of the outcome
marg.p = .2 
mu = log(marg.p / (1 - marg.p))

# between-entity variance
var.btwn = 0.04 
tau = c(mu, sqrt(var.btwn))

# parameters from risk-adjustment model
theta1 = log(1)   
theta2 = log(1.5)
theta = c(theta1, theta2)

df2 <- simulateData(n.entity = n.entity, avg.n = avg.n, tau = tau, theta = theta)
head(df2)

## -----------------------------------------------------------------------------

# adjust number of bootstraps and cores for parallel processing.
n.boots = 20
n.cores = 3

# change this to the directory that you want plots saved to:
output.dir = paste0(getwd(), '/test_output/')

# run profiling analysis
profiling.results <- profiling_analysis(df = df1, ctrPerf = controlPerf(n.boots = n.boots, n.cores = n.cores), output.dir = output.dir)


## -----------------------------------------------------------------------------
plotN(profiling.results$perf.results$n)


## -----------------------------------------------------------------------------
# unadjusted performance
plotPerformance(profiling.results$perf.results)

# OE-standardized performance
plotPerformance(profiling.results$perf.results, 'oe')


## -----------------------------------------------------------------------------
BB.results <- calcBetaBin(df = df1)
message(paste0('Estimated parameters from the Beta-Binomial model are: alpha = ', round(BB.results$alpha, 3), '; beta = ', round(BB.results$beta, 3)))

BB.plot.df <- data.frame(
    method = rep(c('Beta-binomial',
                   'Beta-binomial, FE',
                   'Beta-binomial, RE',
                   'Beta-binomial, Jeffreys'), each = length(BB.results$est.BB)),
    est = c(BB.results$est.BB, BB.results$est.BB.FE, BB.results$est.BB.RE, BB.results$est.BB.J)
  )

  BB.fig <- ggplot(data = BB.plot.df, aes(est, factor(method))) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(alpha = 0.6, position = position_jitter(height = 0.1, width = 0)) +
    xlab('Entity-specific reliability estimate') +
    ylab('Method') +
    theme_classic() +
    theme(
      panel.grid.major = element_line(linewidth = 1),
      plot.title = element_text(size = 16, face ="bold"),
      axis.text = element_text(size = 16),
      axis.ticks.length = unit(.25,"cm"),
      axis.title = element_text(size = 18, face = "bold"),
      legend.position = 'bottom'
    )
BB.fig

## -----------------------------------------------------------------------------

# number of resamples to use for the permutation Ssplit-sample reliability estimate
n.resamples = 100
n.cores = 3

rel.results <- calcReliability(df = df1, ctrPerf = controlPerf(n.cores = n.cores), ctrRel = controlRel(n.resamples = n.resamples))
rel.results

