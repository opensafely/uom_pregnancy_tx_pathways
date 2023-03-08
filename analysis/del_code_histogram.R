library('tidyverse')

df_input <- read_csv(
  here::here("output", "input.csv.gz"),
  col_types = cols(patient_id = col_integer(),delivery_code_number = col_double())
)

df_input <- df_input %>% filter(delivery_code_number >0)

#might need library('dplyr')

#table(df_input) write as csv write_csv

plot_delivery_code_frequency <- ggplot(data=df_input, aes(df_input$delivery_code_number)) + 
geom_histogram() +
labs (title = "Delivery Code Distribution",
x = "No. of delivery codes")


ggsave(
  plot= plot_delivery_code_frequency,
  filename="del_code_frequency.png", path=here::here("output"),
)
