test_that('example analysis works',{
  set.seed(123)
  df <- simulateData(n.entity = 100, n.obs = 50, mu = 0.3, r = 0.6)
  HLGM.results <- suppressMessages(calcHLGMRel(df = df, show.all = T))

  expect_equal(round(HLGM.results$marg.p, 3), 0.313)
  expect_equal(round(HLGM.results$var.b.HLGM.latent, 3), 0.136)
  expect_equal(round(mean(HLGM.results$est.HLGM.latent), 3), 0.671)
  expect_equal(round(mean(HLGM.results$est.HLGM.delta), 3), 0.59)
  expect_equal(round(mean(HLGM.results$est.HLGM.MC), 3), 0.587)
  expect_equal(round(mean(HLGM.results$est.HLGM.FE), 3), 0.604)
  expect_equal(round(mean(HLGM.results$est.HLGM.RE), 3), 0.593)
})
