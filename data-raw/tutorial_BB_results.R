## code to prepare `tutorial_BB_results` dataset goes here

load('data/example_df_1.rda')

## Beta-Binomial reliability ##
tutorial_BB_results <- calcBetaBin(df = example_df_1)

usethis::use_data(tutorial_BB_results, overwrite = TRUE)
