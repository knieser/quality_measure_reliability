test_that("function works for aggregated data", {
  df1 <- simulateData(n.entity = 30, n.obs = 60, mu = .3, r = .6)
  AOV.results <- calcAOV(df = df1)

  df.agg <- data.frame(n = aggregate(y ~ entity, data = df1, length)$y,
                       mean = aggregate(y ~ entity, data = df1, mean)$y,
                       sd = aggregate(y ~ entity, data = df1, sd)$y)
  AOV.agg.results <- calcAOV(df = df.agg, df.aggregate = T, std.dev = 'sd')

  expect_equal(AOV.results$var.b.aov, AOV.agg.results$var.b.aov)
  expect_equal(AOV.results$var.w.aov, AOV.agg.results$var.w.aov)
  expect_equal(AOV.results$est.aov, AOV.agg.results$est.aov)
})
