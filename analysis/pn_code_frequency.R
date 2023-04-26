library('tidyverse')

df_input <- read_csv(
  here::here("output", "input.csv"),
  col_types = cols(patient_id = col_integer(),delivery_code_number = col_number(), postnatal_8wk_code_present = col_character(), postnatal_code = col_number())
)

df_input <- df_input %>% filter(postnatal_8wk_code_present >0)

#might need library('dplyr')


# plot_delivery_code_frequency <- ggplot(data=df_input, aes(df_input$delivery_code_number)) + 
# geom_histogram() +
# labs (title = "Delivery Code Distribution",
# x = "No. of delivery codes")


# ggsave(
#   plot= plot_delivery_code_frequency,
#   filename="del_code_histogram.jpeg", path=here::here("output"),
# )


table_pn_codes <- as.data.frame(table(df_input$postnatal_code)) 
table_pn_codes <- table_pn_codes[order(-table_pn_codes$Freq),]

write_csv(table_pn_codes, here::here("output","table_pn_codes.csv"))
