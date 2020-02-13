make_density_plots = function(dframe, continuous_columns,
                              plot_title_lab = 'Continuous Distributions',
                              plot_color = rgb(17,56,99, maxColorValue = 255),
                              facet_color = rgb(0, 125, 186, maxColorValue = 255),
                              plot_alpha = 0.2,
                              n_plot_columns = 3){
  #' Generate density plots for continuous columns in a 
  #' data.frame object using ggplot2. If using a large dataset, consider a sample.
  #' 
  #' @param dframe data.frame object
  #' @param continuous_columns vector of column names for continuous fields
  #' @param plot_title_lab character string for plot title
  #' @param plot_color color (string or rgb()) for densities
  #' @param facet_color color (string or rgb()) for facet labels
  #' @param plot_alpha transparency 0-1 for density plots
  #' @param n_plot_columns number of columns for aggregated plot object
  #' @return ggplot object
  
  # Create tidy dataframe of continuous values and column character strings
  df_list = list()
  for (i in 1:length(continuous_columns)){
    df_list[[i]] = data.frame(x = dframe[,continuous_columns[i]],
                              y = continuous_columns[i])
  }
  df_concat = do.call(rbind.data.frame, df_list)
  
  # Create and return ggplot object
  df_plot = ggplot(df_concat, aes(x)) +
    theme_bw() +
    facet_wrap(~y,
               ncol = n_plot_columns,
               scales = 'free') +
    geom_density(size = 1,
                 alpha = plot_alpha,
                 color = plot_color,
                 fill = plot_color) +
    theme(strip.background = element_rect(fill = facet_color),
          strip.text = element_text(color = 'white', face = 'bold'),
          plot.title = element_text(hjust = 0.5)) +
    labs(x = 'Value',
         y = 'Density',
         title = plot_title_lab)
  return(df_plot)
}
