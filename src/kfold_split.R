# Load Project Configuration and Packages
source('src/split_vector.R')

get_kfold_indices = function(dt_df, k, shuffle = TRUE){
  #' Get vector of indices to use as row-wise splits for k-fold CV
  #' Dependency on split_vector() function
  #' @param dt_df data.table or data.frame object
  #' @param k integer representing number of splits
  #' @param shuffle boolean. if TRUE, shuffle row indices before splits
  #' @example 
  #' k_fold_rows = get_kfold_indices(df, 10)
  n_records_per = dt_df %>% nrow() / k %>% ceiling()
  if (shuffle){
    row_i = sample(1:nrow(dt_df))
  } else {
    row_i = 1:nrow(dt_df)
  }
  index_nums = row_i %>% split_vector(part_size = n_records_per)
  return(index_nums)
}