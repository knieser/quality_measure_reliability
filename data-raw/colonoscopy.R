# CMS measure data for colonoscopy follow-up rates
# these data are from https://data.cms.gov/provider-data/dataset/yv7e-xc69
# Time period: 01/01/2023 to 12/31/2023

colonoscopy <- read.csv('data-raw/CMS_colonoscopy_followup_data.csv')

colonoscopy <- colonoscopy[colonoscopy$Sample != 'Not Available',c('Facility.ID', 'Score', 'Sample')]
names(colonoscopy) <- c('entity', 'p', 'n')
colonoscopy$entity <- factor(colonoscopy$entity)
colonoscopy$p <- as.numeric(colonoscopy$p) / 100
colonoscopy$n <- as.numeric(colonoscopy$n)
colonoscopy$x <- round(colonoscopy$p * colonoscopy$n)

usethis::use_data(colonoscopy, overwrite = TRUE)
