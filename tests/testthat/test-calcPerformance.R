test_that("error checking works", {
  expect_error(calcPerformance(df = psychreadmission, data.type = 'continuous'))
})

test_that('function works', {
  set.seed(123)
  df <- simulateData(n.entity = 100, n.obs = 80, mu = 0.3, r = 0.7)

  out <- calcPerformance(df = df)
  expect_equal(round(out$marg.p, 3), 0.311)
  expect_equal(round(mean(out$perf.results$p),3), 0.31)

  df.x <- simulateData(n.entity = 100, n.obs = 80, mu = 0.3, r = 0.7, beta1 = log(1.6))

  out.x <- suppressMessages(calcPerformance(df = df.x, model = 'y ~ x1 + (1 | entity)', ctrPerf = controlPerf(n.boots = 3)))
  expect_equal(round(out.x$marg.p, 3), 0.29)
  expect_equal(round(mean(out.x$perf.results$p),3), 0.29)
  expect_equal(round(mean(out.x$perf.results$oe),3), 1.011)

})
