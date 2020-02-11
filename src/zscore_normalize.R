zscore_normalize = function(input_vector, relative_to = 'mean'){
  #' Normalize vector of numbers between 0 and 1
  #'
  #' @param input_vector Vector of numbers
  #' @param relative_to  Use mean or median in z-score calculation.
  #' Defaults to 'mean'. Allowed values: {'mean', 'median'}
  #' @return Vector of z-scores
  #' @example
  #' zscore_normalize
  relative_value = ifelse(relative_to == 'mean',
                          mean(input_vector),
                          median(input_vector))
  stdev_input = sd(input_vector)
  z_score_output = (input_vector - relative_value) / stdev_input
  return(z_score_output)
}