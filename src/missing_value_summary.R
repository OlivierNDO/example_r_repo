missing_value_summary = function(dt_df){
  #' Generate dataframe with counts of NA in each column
  #' of a data.frame or data.table object
  #' 
  #' @param dt_df data.table or data.frame
  #' @return data.frame with columns 'field' and 'n_missing'
  #' if at least one field has missing values
  missing_vector = colSums(sapply(dt_df, is.na)) %>% as.numeric()
  missing_df = data.frame(field = colnames(dt_df),
                          n_missing = missing_vector) %>%
    dplyr::mutate(n_records = nrow(dt_df)) %>%
    dplyr::mutate(percent_missing = n_missing / n_records) %>%
    dplyr::select(-n_records) %>%
    dplyr::filter(n_missing > 0)
  
  total_missing = missing_df %>% nrow()
  print_msg = paste0(total_missing, ' of ', ncol(dt_df), 
                     ' columns contain NA values')
  if (total_missing < 1){
    print(print_msg)
  } else {
    print(print_msg)
    print(missing_df)
  }
}