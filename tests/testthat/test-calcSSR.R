test_that('example analysis works',{
  set.seed(123)
  df <- simulateData(n.entity = 100, n.obs = 80, mu = 0.3, r = 0.7)
  SSR.results <- calcSSR(df = df, ctrRel = controlRel(n.resamples = 200))

  expect_equal(round(SSR.results$est.PSSR,2),0.75, tolerance = 0.05)

})
