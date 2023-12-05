library('tidyverse')

# df_input <- read_csv(
#   here::here("output", "input_8wk.csv.gz"),
#   col_types = cols(patient_id = col_integer(),delivery_code_number = col_number())
# )

setwd(here::here("output", "joined_8wk"))
df_input<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

#col_types = cols(patient_id = col_integer(),delivery_code_number = col_number())

df_input <- df_input %>% dplyr::filter(delivery_code_number >0)

#might need library('dplyr')

plot_delivery_code_frequency <- ggplot(data=df_input, aes(df_input$delivery_code_number)) + 
geom_histogram() +
labs (title = "Delivery Code Distribution",
x = "No. of delivery codes")


ggsave(
  plot= plot_delivery_code_frequency,
  filename="del_code_histogram_reviewed.jpeg", path=here::here("output"),
)


table_del_codes <- as.data.frame(table(df_input$delivery_code)) 
table_del_codes <- table_del_codes[order(-table_del_codes$Freq),]

write_csv(table_del_codes, here::here("output","table_del_codes_reviewed.csv"))
