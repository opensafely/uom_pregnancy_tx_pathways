# library("ggplot2")
# library("data.table")
# library("dplyr")
# library("tidyverse")
# library("MASS")

# ############ this is from ITS plot
# ############ change file names etc
# ########### add this analysis and csvs to project yaml if using

# load files - write csvs in other scripts or combine here? 
# need DF and DF_plot_counter_final
# DF - ITS_estimates_overall_6wk.csv
# DF - ITS_estimates_overall_12wk.csv
# DF - ITS_estimates_overall.csv

# df_plot_counter_final - ITS_estimates_counter_6wk.csv
# df_plot_counter_final - ITS_estimates_counter_12wk.csv
# df_plot_counter_final - ITS_estimates_counter_overall.csv


write_csv(as.data.frame(df_plot_counter_final), here::here("output", "ITS_estimates_counter_12wk.csv"))


# add 6/8/12 var?

# split dfs
# model prediction/rbind
# ITS subgroup code using facet_grid

## would need to run ITS plots again to generate csvs - add to project yaml 