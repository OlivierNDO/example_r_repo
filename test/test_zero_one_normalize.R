# Load Modules
req_pkgs = c('dplyr', 'testthat')
lapply(req_pkgs, require, character.only = TRUE)
require(testthat)
source("src/zero_one_normalize.R")

# Execute Unit Test
test_that('Test zero_one_noramlize(c(10,50,100))',{
  #' Unit test of zero_one_normalize() function
  result = c(10,50,100) %>%
    zero_one_normalize() %>%
    median() %>%
    round(2)
  testthat::expect_equal(result, 0.44)
})