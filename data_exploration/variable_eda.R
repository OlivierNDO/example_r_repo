
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


