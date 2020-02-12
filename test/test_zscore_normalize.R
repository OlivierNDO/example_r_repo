# Load Modules
req_pkgs = c('dplyr', 'testthat')
lapply(req_pkgs, require, character.only = TRUE)
source("src/zscore_normalize.R")

# Execute Unit Test
test_that('Test zscore_normalize(c(10,50,100))',{
  #' Unit test of zscore_normalize() function
  result = c(10,50,100) %>%
    zscore_normalize() %>%
    median() %>%
    round(2)
  testthat::expect_equal(result, -0.07)
})