## code to prepare `example_df_2` dataset goes here

set.seed(123)

## Data parameters ##
n.entity = 100  # number of accountable entities
n.obs = 50 # average number of patients/cases per accountable entity
mu = .2 # marginal probability of the outcome
r = .7 # reliability for entity with an median number of patients
beta1 = log(1.5) # parameter for risk-adjustment model---coefficient for x1 which is simulated from a standard Normal

## Simulate data ##
example_df_2 <- simulateData(n.entity = n.entity, n.obs = n.obs, mu = mu, r = r, beta1 = beta1)


usethis::use_data(example_df_2, overwrite = TRUE)
