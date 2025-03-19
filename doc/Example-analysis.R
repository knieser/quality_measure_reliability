## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8, 
  fig.height=6
)
library(knitr)


## ----setup, include = FALSE---------------------------------------------------
library(ggplot2) # for plots
library(doParallel) # for parallel processing
library(lme4) # for fitting GLMMs
library(psych) # for calculating ICCs
library(devtools) # R package development

load_all()
library(QualityMeasure)

## -----------------------------------------------------------------------------
set.seed(123)

# number of accountable entities
n.entity = 100  

# average number of patients/cases per accountable entity
n.pts = 50

# parameters of the Beta distribution
alpha = 1
beta = 30

# sample the number of patients/cases per accountable entity
n = rpois(n.entity, n.pts) 
total.n = sum(n)

# sample the true performance for each entity
p = rbeta(n.entity, alpha, beta) 

# make sample data
entity = rep(1:n.entity, times = n)
y = rbinom(total.n, 1, rep(p, times = n))
df1 = data.frame(
  entity = entity,
  y = y
)

## ----echo = FALSE, results = 'asis'-------------------------------------------
kable(head(df1, 10), caption = 'Simulated data 1')

## -----------------------------------------------------------------------------
# number of accountable entities
n.entity = 100  

# average number of patients/cases per accountable entity
avg.n = 100

# marginal probability of the outcome
marg.p = .3 
mu = log(marg.p / (1 - marg.p))

# between-entity variance
var.btwn = 0.04 
tau = c(mu, sqrt(var.btwn))

# parameters from risk-adjustment model
theta1 = log(1)   
theta2 = log(1.5)
theta = c(theta1, theta2)

df2 <- simulateData(n.entity = n.entity, avg.n = avg.n, tau = tau, theta = theta)


## ----echo = FALSE, results = 'asis'-------------------------------------------
kable(head(df2, 10), caption = 'Simulated data 2')

## -----------------------------------------------------------------------------
# adjust number of bootstraps and cores for parallel processing.
n.boots = 25
n.cores = 5

# run profiling analysis
profiling.results <- profiling_analysis(df = df1, ctrPerf = controlPerf(n.boots = n.boots, n.cores = n.cores))
perf.results <- profiling.results$perf.results

## ----echo = FALSE, results = 'asis'-------------------------------------------
kable(profiling.results$perf.summary, caption = 'Performance summary statistics across entities')

## -----------------------------------------------------------------------------
plotN(perf.results$n)

## -----------------------------------------------------------------------------
# Unadjusted performance
plotPerformance()

## -----------------------------------------------------------------------------
plotPerformance(plot.type = 'OR')

## -----------------------------------------------------------------------------
plotPerformance(plot.type = 'correlation')

## -----------------------------------------------------------------------------
BB.results <- calcBetaBin(df = df1)

## ----echo = FALSE-------------------------------------------------------------
summary(BB.results$est.BB)

## ----echo = FALSE-------------------------------------------------------------
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
# Aggregated data
df.agg <- data.frame(n = aggregate(y ~ entity, data = df1, length)$y,
                     x = aggregate(y ~ entity, data = df1, sum)$y)


BB.agg.results <- calcBetaBin(df = df.agg, df.aggregate = T, n = 'n', x = 'x')

## ----echo = FALSE-------------------------------------------------------------
summary(BB.agg.results$est.BB)

## -----------------------------------------------------------------------------
# number of resamples to use for the permutation Ssplit-sample reliability estimate
n.resamples = 100
n.cores = 5

rel.out <- calcReliability(df = df1, ctrPerf = controlPerf(n.cores = n.cores), ctrRel = controlRel(n.resamples = n.resamples))

## ----echo = FALSE, results = 'asis'-------------------------------------------
rel.results <- rel.out$rel.results
rel.results.sub <- rel.results[,c('method', 'reliability', 'reliability_min', 'reliability_max')]
rel.results.sub$reliability <- round(rel.results.sub$reliability, 3)
rel.results.sub$reliability_min <- round(rel.results.sub$reliability_min, 3)
rel.results.sub$reliability_max <- round(rel.results.sub$reliability_max, 3)
names(rel.results.sub) <- c('Method', 'Reliability', 'Min Reliability', 'Max Reliability')

kable(rel.results.sub, caption = 'Reliability estimates')

## ----echo = FALSE-------------------------------------------------------------
HLGM.out <- rel.out$HLGM.out
BB.out <- rel.out$BB.out
rel.plot.df <- data.frame(
    method = rep(c('HLGM, latent scale',
                   'HLGM, delta approx.',
                   'HLGM, FE',
                   'HLGM, RE',
                   'Beta-binomial',
                   'Beta-binomial, FE',
                   'Beta-binomial, RE',
                   'Beta-binomial, Jeffreys'), each = length(HLGM.out$n)),
    est = c(HLGM.out$est.HLGM.latent, HLGM.out$est.HLGM.delta, HLGM.out$est.HLGM.FE, HLGM.out$est.HLGM.RE,
            BB.out$est.BB, BB.out$est.BB.FE, BB.out$est.BB.RE, BB.out$est.BB.J)
  )

fig.rel <- ggplot(data = rel.plot.df, aes(est, factor(method))) +
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
fig.rel

