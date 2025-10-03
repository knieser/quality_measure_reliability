test_that('example analysis works',{
  set.seed(123)
  df <- simulateData(n.entity = 100, n.obs = 50, mu = 30, r = 0.6, data.type = 'normal')
  HLM.results <- suppressMessages(calcHLMRel(df = df))

  expect_equal(round(HLM.results$var.b,3), 0.032)
  expect_equal(round(HLM.results$var.w,3), 0.97)
  expect_equal(round(mean(HLM.results$est.HLM),3), 0.621)
})
