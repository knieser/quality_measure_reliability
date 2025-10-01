test_that("error checking works", {
  expect_error(simulateData(n.entity = 50, n.obs = c(20, 40, 50), mu = .3, r = .2))
  expect_error(simulateData(n.entity = 50, n.obs = 50, mu = .3, r = 2))
  expect_error(simulateData(n.entity = 50, n.obs = 50, mu = 3, r = .2))
  expect_error(simulateData(n.entity = 50, n.obs = 50, mu = .3, r = .2, dist = 'binomial'))
})
