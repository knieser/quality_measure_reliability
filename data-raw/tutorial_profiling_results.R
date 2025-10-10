## code to prepare `tutorial_profiling_results` dataset goes here

load('data/example_df_2.rda')

## Profiling results ##
model = 'y ~ x1 + (1 | entity)'
perf.out.2 <- calcPerformance(df = example_df_2, model = model, ctrPerf = controlPerf(n.boots = 1000, n.cores = 2))
tutorial_profiling_results <- perf.out.2$perf.results

usethis::use_data(tutorial_profiling_results, overwrite = TRUE)
