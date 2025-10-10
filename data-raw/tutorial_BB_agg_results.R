## code to prepare `tutorial_BB_agg_results` dataset goes here

load('data/example_df_1.rda')

# Aggregated data
example.df.agg <- data.frame(n = aggregate(y ~ entity, data = example_df_1, length)$y,
                             x = aggregate(y ~ entity, data = example_df_1, sum)$y)

tutorial_BB_agg_results <- calcBetaBin(df = example.df.agg, df.aggregate = T, n = 'n', x = 'x')


usethis::use_data(tutorial_BB_agg_results, overwrite = TRUE)
