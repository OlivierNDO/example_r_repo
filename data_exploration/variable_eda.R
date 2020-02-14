
### Config
#################################################################
# Load CRAN Packages & Turn Off Scientific Notation
req_pkgs = c('data.table', 'dplyr', 'ggplot2', 'gridExtra')
lapply(req_pkgs, require, character.only = TRUE)
options(scipen = 999)

# Load Project Configuration and Packages
source('configuration.R')
source('src/count_elements.R')
source('src/make_density_plots.R')
source('src/make_violin_plots.R')
source('src/missing_value_summary.R')
source('src/split_vector.R')
source("src/zscore_normalize.R")
source("src/zero_one_normalize.R")


### Generate PDFs with Distribution Plots
################################################################
# Load Training Set, Limit to a Sample of 100k Records
df = read.csv(paste0(config_raw_data_folder, config_train_file)) %>% dplyr::sample_n(100000)

# Recode Sparse Categorical Elements
df[,config_categorical_columns] = apply(df[,config_categorical_columns], 2,
                                        recode_sparse_elements,
                                        freq_cutoff = config_min_categ_element_perc)

# Missing Values
missing_value_df = missing_value_summary(df)

# Continuous Variable Distributions
density_plots = make_density_plots(dframe = df,
                                   continuous_columns = config_continuous_columns,
                                   plot_title_lab = 'Training Set Continuous Fields - 100k record sample')
# Save PDF with Image
ggsave(filename = paste0(config_plot_output_folder, 'eda_density_plots.pdf'),
       plot = density_plots)


# Categorical Variable Response Distributions (plot in groups of 4)
plot_iter_chunks = split_vector(1:length(config_categorical_columns), 4)

for (pic in plot_iter_chunks){
  use_values = config_categorical_columns[pic]
  plot_i = make_violinplots(dframe = df,
                            categorical_columns = use_values,
                            response_column = config_response_column,
                            min_records = 1000,
                            plot_title_lab = '')
  
  # Save PDF with Image
  ggsave(filename = paste0(config_plot_output_folder,
                           'eda_violins_plots_', min(pic),
                           '_', max(pic), '.pdf'),
         plot = plot_i,
         width = 10,
         height = 7)
  print(paste0('Plots created: ', max(pic)))
}


### Identify Categorical Columns with Sparse Elements
################################################################
categ_counts = get_categorical_counts(df, config_categorical_columns)



categ_counts_regroup = categ_counts %>%
  dplyr::mutate(element = ifelse(percent_total < config_min_categ_element_perc,
                                 ''))



t = c(rep('a', 500), rep('b', 100), rep('c', 390), rep('d', 5), rep('e', 5))




temp = apply(df[,config_categorical_columns], 2, recode_sparse_elements, freq_cutoff = 0.025)

old = c()
new = c()

for(c in 1:ncol(temp)){
  new[[c]] = length(unique(temp[,c]))
  old[[c]] = length(unique(df[,config_categorical_columns[c]]))
}


recode_categorical_columns = function(dframe, categorical_columns,
                                      recode_value = 'sparse recode', return_factor = TRUE){
  #' Applies recode_sparse_elements() function to a list of 
  #' specified columns in a data.frame object, returning a dataframe
  #' with recoded sparse discrete variables.
  #' @param dframe input data.frame object
  #' @param categorical_columns representing categorical variable
  #' @param recode_value the value used to replace sparse elements in the vector
  #' @param return_factor boolean value indicating whether or not to convert output to factor 
  #' representing column names of categorical variables
  #' @returns data.frame with <categorical_columns> fields after recoding
  
  column_list = 
  
  
  
  
  
  
  
}










temp = recode_sparse_elements(t, 0.01)



for (c in config_categorical_columns){
  print(class(df[1,c]))
}


