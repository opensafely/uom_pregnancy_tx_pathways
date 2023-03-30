#### TO DO
# 1. figure out which files we need and how to combine them
# 2. write code to combine files
# 3. write code for counts - table and/or plot - DONE

# from notes     df2<-df1%>%group_by(date)     
# rbind (df1, df2)
# ^ do this for all measures by month?
# then sort/group by patient id?
# what do we want - no of codes per person per month

library('tidyverse')

# df_input <- read_csv(
#   here::here("output", "measures", "input_measures_2019-01-01.csv.gz"),
# input_measures or input_monthly?
# OR
#   this is from del code table
#   here:: ("output", "measures", "input_*.csv.gz"),
#    and then group by month and find mean per person? 
#    and/or do we want distribution
#
#   col_types = cols(patient_id = col_integer(),pn8wk_code_number = col_number())
# )

#have tested code below on measures file for one month

df_input <- df_input %>% filter(pn8wk_code_number >0)

 plot_pn8wk_code_frequency <- ggplot(data=df_input, aes(df_input$pn8wk_code_number)) + 
 geom_histogram() +
 labs (title = "Pn 8wk Code Distribution",
 x = "No. of postnatal codes")

 ggsave(
   plot= plot_pn8wk_code_frequency,
   filename="pn8wk_code_histogram.jpeg", path=here::here("output"),
 )

#table for no. of codes by frequency
table_pn_codes <- as.data.frame(table(df_input$delivery_code)) 

#table above sorted by frequency, rename var1?
table_pn_codes <- table_pn_codes[order(-table_del_codes$Freq),]

write_csv(table_pn_codes, here::here("output","table_pn_codes.csv"))
