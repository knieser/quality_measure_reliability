## code to prepare `tutorial_reliability_results_2` dataset goes here

load('data/example_df_2.rda')

model = 'y ~ x1 + (1 | entity)'
tutorial_reliability_results_2 <- calcReliability(df = example_df_2, model = model)

usethis::use_data(tutorial_reliability_results_2, overwrite = TRUE)
