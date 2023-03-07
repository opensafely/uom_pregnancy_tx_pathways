library('tidyverse')

df_input <- read_csv(
  here::here("output", "input.csv.gz"),
  col_types = cols(patient_id = col_integer(),delivery_code_number = col_double())
)

plot_delivery_code_frequency <- ggplot(data=df_input, aes(df_input$delivery_code_number)) + 
geom_histogram() +
labs (title = "Delivery Code Distribution",
x = "No. of delivery codes")


ggsave(
  plot= plot_delivery_code_frequency,
  filename="del_code_frequency.png", path=here::here("output"),
)

## we could also use this to look at 
## most frequent codes?
## what data do we need for this?