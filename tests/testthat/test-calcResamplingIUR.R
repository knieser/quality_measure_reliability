test_that('example analysis works',{
  set.seed(123)
  df <- simulateData(n.entity = 100, n.obs = 80, mu = 0.3, r = 0.7)
  RIUR.results <- calcResamplingIUR(df = df, ctrRel = controlRel(n.resamples = 200))

  expect_equal(round(RIUR.results$var.b, 2), 0.01, tolerance = 0.02)
  expect_equal(round(RIUR.results$IUR, 3), 0.741, tolerance = 0.05)

})
