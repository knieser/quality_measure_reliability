test_that("error checking works", {
  expect_error(profiling_analysis(df = psychreadmission, data.type = 'continuous'))
})
