## code to prepare `psychreadmission` dataset goes here

psych <- read.csv('data-raw/CMS_psychiatric_measure_data.csv')
psychreadmission <- psych[,c('Facility.ID', 'READM.30.IPF.Category', 'READM.30.IPF.Denominator', 'READM.30.IPF.Rate', 'READM.30.IPF.Lower.Estimate', 'READM.30.IPF.Higher.Estimate')]
psychreadmission <- psychreadmission[psychreadmission$READM.30.IPF.Rate != 'Not Available',]
names(psychreadmission) <- c('entity', 'category', 'n', 'rate', 'rate.lwr', 'rate.upr')

psychreadmission$entity <- factor(psychreadmission$entity)

psychreadmission$n <- as.numeric(psychreadmission$n)
psychreadmission$rate <- as.numeric(psychreadmission$rate)
psychreadmission$rate.lwr <- as.numeric(psychreadmission$rate.lwr)
psychreadmission$rate.upr <- as.numeric(psychreadmission$rate.upr)

usethis::use_data(psychreadmission, overwrite = TRUE)
