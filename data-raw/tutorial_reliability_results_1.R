## code to prepare `tutorial_reliability_results_1` dataset goes here

load('data/example_df_1.rda')

tutorial_reliability_results_1 <- calcReliability(df = example_df_1, ctrRel = controlRel(n.resamples = 100))

usethis::use_data(tutorial_reliability_results_1, overwrite = TRUE)
