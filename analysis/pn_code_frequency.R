library('tidyverse')

df_input <- read_csv(
  here::here("output", "pn8wk"),
  #here::here("output", "pn8wk", "input_*.csv.gz"),
  col_types = cols(patient_id = col_integer(),delivery_code_number = col_number())
)

df<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

df<- df %>% filter(postnatal_8wk_code_present >0)

#might need library('dplyr')

# plot_pn_code_frequency <- ggplot(data=df_input, aes(df_input$delivery_code_number)) + 
# geom_histogram() +
# labs (title = "Delivery Code Distribution",
# x = "No. of delivery codes")


# ggsave(
#   plot= plot_delivery_code_frequency,
#   filename="pn_code_histogram_reviewed.jpeg", path=here::here("output"),
# )
 

table_pn_codes <- as.data.frame(table(df_input$postnatal_code)) 
table_pn_codes <- table_pn_codes[order(-table_pn_codes$Freq),]

write_csv(table_pn_codes, here::here("output","table_pn_codes_reviewed.csv"))
