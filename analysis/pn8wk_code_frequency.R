# this doesnt run yet and is only for one month
# need to figure out how to combine months and get mean no per month


# library('tidyverse')

# df_input <- read_csv(
#   here::here("output", "measures", "input_measures_2019-01-01.csv.gz"),
#   col_types = cols(patient_id = col_integer(),pn8wk_code_number = col_number())
# )

# df_input <- df_input %>% filter(pn8wk_code_number >0)

# #might need library('dplyr')


# plot_pn_code_frequency <- ggplot(data=df_input, aes(df_input$pn8wk_code_number)) + 
# geom_histogram() +
# labs (title = "Pn 8wk Code Distribution",
# x = "No. of postnatal codes")


# ggsave(
#   plot= plot_pn8wk_code_frequency,
#   filename="pn8wk_code_histogram.jpeg", path=here::here("output"),
# )


#table_del_codes <- as.data.frame(table(df_input$delivery_code)) 
#table_del_codes <- table_del_codes[order(-table_del_codes$Freq),]

#write_csv(table_del_codes, here::here("output","table_del_codes.csv"))
