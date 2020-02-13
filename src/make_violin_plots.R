make_violinplots = function(dframe, categorical_columns, response_column, min_records,
                            tail_cutoff = .999,
                            plot_title_lab = 'Categorical Field Response Distributions',
                            plot_color = rgb(17,56,99, maxColorValue = 255),
                            facet_color = rgb(0, 125, 186, maxColorValue = 255),
                            plot_alpha = 0.2,
                            n_plot_columns = 3){
  #' Generate density plots for continuous columns in a 
  #' data.frame object using ggplot2. If using a large dataset, consider a sample.
  #' 
  #' @param dframe data.frame object
  #' @param categorical_columns vector of column names for categorical fields
  #' @param response_column character string of column name corresponding to response variable
  #' @param min_records minimum number of records for each discrete value to include
  #' @param tail_cutoff percentile below which to not plot response variable (helps with scale)
  #' @param plot_title_lab character string for plot title
  #' @param facet_color color (string or rgb()) for facet labels
  #' @param plot_alpha transparency 0-1 for densityoh plots
  #' @param n_plot_columns number of columns for aggregated plot object
  #' @return ggplot object
  
  # Create tidy dataframe of continuous & character string columns,
  # limited by minimum volume parameter 'min_records'
  df_list = list()
  low_vol_list = list()
  for (i in 1:length(categorical_columns)){
    df_i = data.frame(x = dframe[,categorical_columns[i]],
                      y = dframe[,response_column],
                      z = categorical_columns[i])
    
    df_i_counts = df_i %>%
      dplyr::select(x) %>%
      dplyr::mutate(freq = 1) %>%
      dplyr::group_by(x) %>%
      dplyr::summarise_all(.funs = list(sum)) %>%
      data.frame() %>%
      dplyr::ungroup()
    
    high_vol = df_i_counts %>%
      dplyr::filter(freq >= min_records) %>%
      dplyr::select(x)
    
    df_list[[i]] = df_i %>% dplyr::inner_join(high_vol, 'x')
    
  }
  df_concat = do.call(rbind.data.frame, df_list)
  
  # Determine cutoff point for y-axis
  numeric_cutoff = dframe[,response_column] %>%
    quantile(tail_cutoff) %>%
    as.numeric() %>%
    ceiling()
  
  cutoff_subtitle = paste0('values below ', tail_cutoff, ' percentile excluded for scale')
  
  
  
  # Create and return ggplot object
  df_plot = ggplot(df_concat, aes(x, y, color = x, fill = x)) +
    theme_bw() +
    facet_wrap(~z, ncol = n_plot_columns, scales = 'free') +
    geom_violin(size = 1, alpha = plot_alpha) +
    scale_y_continuous(labels = scales::comma,
                       limits = c(min(df_concat$y), numeric_cutoff)) +
    theme(strip.background = element_rect(fill = facet_color),
          strip.text = element_text(color = 'white', face = 'bold'),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.position = 'none') +
    labs(x = 'Value',
         y = response_column,
         title = plot_title_lab,
         subtitle = cutoff_subtitle,
         caption = paste0('values with fewer than ', min_records, ' records excluded'))
  return(suppressWarnings(df_plot))
}