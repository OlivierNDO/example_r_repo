#' Normalize vector of numbers between 0 and 1
#' 
#' @param input_vector Vector of numbers
#' @return Input vector scaled between 0 and 1
#' @examples
#' zero_one_normalize(1:100)
zero_one_normalize = function(input_vector){
  vector_range = max(input_vector) - min(input_vector)
  norm_vector = (input_vector - min(input_vector)) / vector_range
  return(norm_vector)
}