count_in_vector = function(element, in_vector){
  #' Return frequency count of <element> within <in_vector> 
  return(in_vector[in_vector == element] %>% length())
}


count_each_unique = function(in_vector, var_name = NULL){
  #' Count frequency of each unique element in a vector
  #' @param in_vector input vector
  #' @param var_name if not null, add column with <var_name> 
  #' to output data.frame object
  #' @returns data.frame with columns 'element' and 'count'
  unique_values = in_vector %>% unique()
  freq_counts = sapply(unique_values, count_in_vector, in_vector = in_vector)
  output_df = data.frame(element = unique_values, count = freq_counts)
  if (!is.null(var_name)){
    output_df[,'variable'] = var_name
  }
  return(output_df)
}


get_categorical_counts = function(dframe, categorical_columns){
  #' Generate counts and percentage frequency for each 
  #' element in every categorical field within a dataframe.
  #' Use case - preliminary step for recoding sparse discrete variables.
  #' @param dframe input data.frame object
  #' @param categorical_columns vector of character strings
  #' representing column names of categorical variables
  #' to output data.frame object
  #' @returns data.frame with columns 'element', 'count', 
  #' 'variable', and 'percent_total'
  freq_df_list = list()
  for (i in 1:length(categorical_columns)){
    freq_df_list[[i]] = count_each_unique(dframe[,categorical_columns[i]],
                                          categorical_columns[i])
  }
  freq_df = do.call(rbind.data.frame, freq_df_list)
  
  variable_agg = freq_df %>%
    dplyr::select(variable, count) %>%
    dplyr::group_by(variable) %>%
    dplyr::summarise_all(.funs = list(sum)) %>%
    data.frame() %>%
    dplyr::ungroup() %>%
    dplyr::rename(total = count)
  
  output_df = freq_df %>%
    dplyr::left_join(variable_agg, 'variable') %>%
    dplyr::mutate(percent_total = count / total) %>%
    dplyr::select(-total) %>%
    dplyr::arrange(variable, desc(count))
  
  return(output_df)
}


recode_sparse_elements = function(in_vector, freq_cutoff,
                                  recode_value = 'sparse recode', return_factor = TRUE){
  #' Recodes the elements in a vector representing a small portion
  #' of the total, specified by <freq_cutoff> arugment
  #' Recoding sparse discrete variables.
  #' @param in_vector input vector representing categorical variable
  #' @param freq_cutoff numeric value specifying the % of total count
  #' an element must make up in order to avoid recoding
  #' @param recode_value the value used to replace sparse elements in the vector
  #' @param return_factor boolean value indicating whether or not to convert output to factor 
  #' representing column names of categorical variables
  #' @returns vector with sparse values recoded
  
  # Generate percentage counts
  unique_value_counts = count_each_unique(in_vector)
  existing_values = data.frame(element = in_vector)
  
  # Recode elements below <freq_cutoff> % of total
  recoded_values = unique_value_counts %>%
    dplyr::mutate(perc_total = count / length(in_vector)) %>%
    dplyr::mutate(new_element = ifelse(perc_total < freq_cutoff,
                                       as.character(recode_value),
                                       as.character(element))) %>%
    dplyr::select(element, new_element)
  
  output_df = data.frame(element = in_vector) %>%
    dplyr::inner_join(recoded_values, 'element')
  
  # Return values
  if (return_factor){
    return(factor(output_df$new_element))
  } else {
    return(output_df$new_element)
  }
}



















