test_that('example analysis works',{
  set.seed(123)
  df <- simulateData(n.entity = 100, n.obs = 50, mu = 0.3, r = 0.6)
  HLGM.results <- suppressMessages(calcHLGMRel(df = df, show.all = TRUE))

  expect_equal(round(HLGM.results$marg.p, 3), 0.313)
  expect_equal(round(HLGM.results$var.b.HLGM.latent, 3), 0.136)
  expect_equal(round(mean(HLGM.results$est.HLGM.latent), 3), 0.671)
  expect_equal(round(mean(HLGM.results$est.HLGM.delta), 3), 0.59)
  expect_equal(round(mean(HLGM.results$est.HLGM.MC), 3), 0.587, tolerance = 0.01)
  expect_equal(round(mean(HLGM.results$est.HLGM.FE), 3), 0.604)
  expect_equal(round(mean(HLGM.results$est.HLGM.RE), 3), 0.593)

  df.x <- simulateData(n.entity = 100, n.obs = 50, mu = 0.3, r = 0.6, beta1 = log(1.5))
  model = 'y ~ x1 + (1 | entity)'
  HLGM.x.results <- suppressMessages(calcHLGMRel(df = df.x, model = model, show.all = TRUE))
  expect_equal(round(HLGM.x.results$marg.p, 3), 0.329)
  expect_equal(round(HLGM.x.results$var.b.HLGM.latent, 3), 0.148)
  expect_equal(round(mean(HLGM.x.results$est.HLGM.latent), 3), 0.684)
  expect_equal(round(mean(HLGM.x.results$est.HLGM.delta), 3), 0.604)
  expect_equal(round(mean(HLGM.x.results$est.HLGM.MC), 3), 0.599, tolerance = 0.01)
  expect_equal(round(mean(HLGM.x.results$est.HLGM.FE), 3), 0.611)
  expect_equal(round(mean(HLGM.x.results$est.HLGM.RE), 3), 0.601)
})
