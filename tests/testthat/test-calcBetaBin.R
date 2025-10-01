test_that("function works for aggregated data", {
  df1 <- simulateData(n.entity = 30, n.obs = 60, mu = .3, r = .6)
  BB.results <- suppressMessages(calcBetaBin(df = df1))

  df.agg <- data.frame(n = aggregate(y ~ entity, data = df1, length)$y,
                       x = aggregate(y ~ entity, data = df1, sum)$y)
  BB.agg.results <- suppressMessages(calcBetaBin(df = df.agg, df.aggregate = T, n = 'n', x = 'x'))

  expect_equal(BB.results$alpha, BB.agg.results$alpha)
  expect_equal(BB.results$beta, BB.agg.results$beta)
})
