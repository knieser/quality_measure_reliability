## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8, 
  fig.height=6
)
library(knitr)


## ----setup--------------------------------------------------------------------
library(devtools)
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
beta = 9

# sample the number of patients/cases per accountable entity
n = rpois(n.entity, n.pts) 
total.n = sum(n)

# sample the true performance for each entity
p = rbeta(n.entity, alpha, beta) 

# make sample data
entity = rep(1:n.entity, times = n)
y = rbinom(total.n, 1, rep(p, times = n))
df1 = data.frame(entity = entity, y = y)

## ----echo = FALSE, results = 'asis'-------------------------------------------
knitr::kable(head(df1, 10), caption = 'Simulated data 1')

## -----------------------------------------------------------------------------
# adjust number of bootstraps and cores for parallel processing.
n.boots = 1000
n.cores = 5

# run profiling analysis for the first dataset without risk-adjustment
profiling.results <- profiling_analysis(df = df1, ctrPerf = controlPerf(n.boots = n.boots, n.cores = n.cores))
perf.results <- profiling.results$perf.results

## ----echo = FALSE, results = 'asis'-------------------------------------------
knitr::kable(profiling.results$perf.summary, caption = 'Performance summary statistics across entities')

## -----------------------------------------------------------------------------
plotN(perf.results$n)

## -----------------------------------------------------------------------------
# Unadjusted performance
plotPerformance(df = perf.results)

## ----echo = FALSE, results = 'asis'-------------------------------------------
knitr::kable(table(perf.results$category.oe), col.names = c('Category', 'Number of entities'), caption = 'Categorization of entities based on their outcome rates')

## -----------------------------------------------------------------------------
plotPerformance(df = perf.results, plot.type = 'OR')

## -----------------------------------------------------------------------------
BB.results <- calcBetaBin(df = df1)

## ----echo = FALSE-------------------------------------------------------------
summary(BB.results$est.BB)

## -----------------------------------------------------------------------------
# Aggregated data
df.agg <- data.frame(n = aggregate(y ~ entity, data = df1, length)$y,
                     x = aggregate(y ~ entity, data = df1, sum)$y)


BB.agg.results <- calcBetaBin(df = df.agg, df.aggregate = T, n = 'n', x = 'x')

## -----------------------------------------------------------------------------
# number of resamples to use for the permutation split-sample reliability estimate
n.resamples = 200

rel.out <- calcReliability(df = df1, ctrPerf = controlPerf(n.cores = n.cores), ctrRel = controlRel(n.resamples = n.resamples))

## ----echo = FALSE, results = 'asis'-------------------------------------------
rel.results <- rel.out$rel.results
rel.results.sub <- rel.results[,c('method', 'reliability', 'reliability_min', 'reliability_max')]
rel.results.sub$reliability <- round(rel.results.sub$reliability, 3)
rel.results.sub$reliability_min <- round(rel.results.sub$reliability_min, 3)
rel.results.sub$reliability_max <- round(rel.results.sub$reliability_max, 3)
names(rel.results.sub) <- c('Method', 'Reliability', 'Min Reliability', 'Max Reliability')

knitr::kable(rel.results.sub, caption = 'Reliability estimates')

## -----------------------------------------------------------------------------
plotSNRReliability(rel.out)

## -----------------------------------------------------------------------------
# number of accountable entities
n.entity = 100  

# average number of patients/cases per accountable entity
avg.n = 50

# marginal probability of the outcome
marg.p = .1 

# reliability for entity with an average number of patients
r = .6

# implied between-entity variance
var.btwn = r/(1 - r) * (1/(avg.n*marg.p*(1 - marg.p)))

mu = log(marg.p / (1 - marg.p))
tau = c(mu, sqrt(var.btwn))

# parameter for risk-adjustment model (i.e., coefficient for x1)
theta = log(1.5)

df2 <- simulateData(n.entity = n.entity, avg.n = avg.n, tau = tau, theta = theta)


## ----echo = FALSE, results = 'asis'-------------------------------------------
knitr::kable(head(df2, 10), caption = 'Simulated data with a covariate')

## -----------------------------------------------------------------------------
model = 'y ~ x1 + (1 | entity)'

## -----------------------------------------------------------------------------
model.perf <- model_performance(df = df2, model = model)
plotEstimates(model.perf$model.results)

## -----------------------------------------------------------------------------
plotPredictedDistribution(df = model.perf$df)

## -----------------------------------------------------------------------------
plotCalibration(df = model.perf$df, quantiles = 10)

## -----------------------------------------------------------------------------
# run profiling analysis for the first dataset without risk-adjustment
profiling.results2 <- profiling_analysis(df = df2, model = model, ctrPerf = controlPerf(n.boots = n.boots, n.cores = n.cores))
perf.results2 <- profiling.results2$perf.results

## ----echo = FALSE, results = 'asis'-------------------------------------------
knitr::kable(profiling.results2$perf.summary, caption = 'Performance summary statistics across entities')

## -----------------------------------------------------------------------------
plotN(perf.results2$n)

## -----------------------------------------------------------------------------
plotPerformance(df = perf.results2)

## -----------------------------------------------------------------------------
plotPerformance(df = perf.results2, plot.y = 'oe')

## ----echo = FALSE, results = 'asis'-------------------------------------------
knitr::kable(table(perf.results2$category.oe), col.names = c('Category', 'Number of entities'), caption = 'Categorization of entities based on OE-risk-standardized rates')

## -----------------------------------------------------------------------------
plotPerformance(df = perf.results2, plot.y = 'pe')

## ----echo = FALSE, results = 'asis'-------------------------------------------
knitr::kable(table(perf.results2$category.pe), col.names = c('Category', 'Number of entities'), caption = 'Categorization of entities based on PE-risk-standardized rates')

## -----------------------------------------------------------------------------
plotPerformance(df = perf.results2, plot.type = 'OR')

## -----------------------------------------------------------------------------
rel.out2 <- calcReliability(df = df2, model = model, ctrPerf = controlPerf(n.cores = n.cores), ctrRel = controlRel(n.resamples = n.resamples))

## ----echo = FALSE, results = 'asis'-------------------------------------------
rel.results2 <- rel.out2$rel.results
rel.results.sub2 <- rel.results2[,c('method', 'reliability', 'reliability_min', 'reliability_max')]
rel.results.sub2$reliability <- round(rel.results.sub2$reliability, 3)
rel.results.sub2$reliability_min <- round(rel.results.sub2$reliability_min, 3)
rel.results.sub2$reliability_max <- round(rel.results.sub2$reliability_max, 3)
names(rel.results.sub2) <- c('Method', 'Reliability', 'Min Reliability', 'Max Reliability')

knitr::kable(rel.results.sub2, caption = 'Reliability estimates')

## -----------------------------------------------------------------------------
plotSNRReliability(rel.out2)

