# Load CRAN Packages
req_pkgs = c('dplyr')
lapply(req_pkgs, require, character.only = TRUE)

# Load Project Packages
source("src/zscore_normalize.R")
source("src/zero_one_normalize.R")

# Use Project Packages
random_distr = rnorm(1000, mean = 100, sd = 10)

random_distr_norm_01 = random_distr %>% zero_one_normalize()

random_distr_norm_z = random_distr %>% zscore_normalize()