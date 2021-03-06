find_mode = function(in_vector) {
    #' Return most frequently occuring element in vector
    unique_values = unique(in_vector)
    mode_value = unique_values[which.max(tabulate(match(in_vector, unique_values)))]
    return(mode_value)
}

dummy_code = function(in_vector, prefix = 'varname_'){
    #' Convert vector into data.frame object with dummy coded values
    #' using the most frequently occuring value as a reference level
    #' and a specified prefix (generally the variable name)
    modal_value = find_mode(in_vector)
    other_values = unique(in_vector[in_vector != modal_value])
    temp_list = list()
    for (i in 1:length(other_values)){
        temp_list[[i]] = as.numeric(in_vector == other_values[i])
    }
    out_df = do.call(cbind.data.frame, temp_list)
    colnames(out_df) = paste0(prefix, other_values)
    return(out_df)
}
