# Quality measure reliability

This repository contains code for calculating reliability of health care quality measures. More detail on each method can be found in 
[Comparing methods for assessing the reliability of health care quality measures](https://doi.org/10.1002/sim.10197).

The function `calcReliability()` within `reliability_functions.R` estimates reliability 14 different ways for
measures with binary outcomes (e.g. follow-up visit occurred or not) and 4 different ways for measures with continuous outcomes. This function takes 5 parameters:
- df0 = data frame with 3 columns: de-identified patient id, provider id, outcome
- type = data type of the outcome. Use `type = 'binary'` to get estimates for binary outcomes. Any other value yields estimates for continuous outcomes.
- reps = number of splits/permutations to perform and average over for the permutation split-sample reliability estimate
- resamples = number of bootstrap resamples for the resampling IUR estimate
- n.cores = number of cores to use for parallel processing for the permutation split-sample and resampling IUR estimates 

