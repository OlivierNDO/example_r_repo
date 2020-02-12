# Load Modules
req_pkgs = c('devtools', 'dplyr', 'testthat')
lapply(req_pkgs, require, character.only = TRUE)

devtools::document()