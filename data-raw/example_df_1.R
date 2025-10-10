## code to prepare `example-data-1` dataset goes here

set.seed(123)

#### No risk-adjustment ####

## Data parameters ##
n.entity = 100  # number of accountable entities
n.obs = 50      # average number of observations per accountable entity
mu = .2         # marginal probability of the outcome
r = .7          # median reliability

## Simulate data ##
example_df_1 <- simulateData(n.entity = n.entity, n.obs = n.obs, mu = mu, r = r)


usethis::use_data(example_df_1, overwrite = TRUE)
