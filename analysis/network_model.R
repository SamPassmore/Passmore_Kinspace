# /usr/bin/Rscript

suppressPackageStartupMessages({
  library(brms)
  library(dplyr)
})

# model the effect of networks on frequency and on diversity

## Import data
files = list.files('results/global/data/', full.names = TRUE)

data = files %>% 
  lapply(., read.csv) %>%
  do.call(rbind, .)


fit.1 = brm(diversity ~ strength + (1|type), data = data)
fit.2 = brm(diversity ~ strength + (strength|type), data = data)

coef(fit.1)
coef(fit.2)