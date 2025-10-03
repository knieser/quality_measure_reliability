test_that("function works for aggregated data", {
  df1 <- simulateData(n.entity = 30, n.obs = 60, mu = .3, r = .6)
  BB.results <- suppressMessages(calcBetaBin(df = df1))

  df.agg <- data.frame(n = aggregate(y ~ entity, data = df1, length)$y,
                       x = aggregate(y ~ entity, data = df1, sum)$y)
  BB.agg.results <- suppressMessages(calcBetaBin(df = df.agg, df.aggregate = T, n = 'n', x = 'x'))

  expect_equal(BB.results$alpha, BB.agg.results$alpha)
  expect_equal(BB.results$beta, BB.agg.results$beta)
})

test_that('example analysis works',{
  set.seed(123)
  df <- simulateData(n.entity = 100, n.obs = 50, mu = 0.3, r = 0.6)
  BB.results <- suppressMessages(calcBetaBin(df = df))
  expect_equal(round(BB.results$alpha,3), 10.884)
  expect_equal(round(BB.results$beta,3), 23.934)
  expect_equal(round(mean(BB.results$est.BB),3), 0.588)
})
