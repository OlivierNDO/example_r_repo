split_vector = function(in_vector, part_size){
  #' Split a vector into parts of a specific size 
  #' 
  #' @param in_vector vector object to be spit
  #' @param part_size size of each chunk returned
  #' @return list of vectors
  #' 
  #' 
  chunk_list = split(in_vector, ceiling(seq_along(in_vector) / part_size))
  return(chunk_list)
}