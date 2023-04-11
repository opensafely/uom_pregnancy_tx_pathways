library('tidyverse')
library('lubridate')
library('dplyr')

#setwd("C:/Users/mdehsdh7/GitHub/uom_pregnancy_tx_pathways/output/measures")

setwd(here::here("output", "measures"))

#combine all "input_measures" files 
dfmonths<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

#create delcode variable as date of delivery code
dfmonths$delcode<-as.Date(dfmonths$delivery_code_date)

#create month and year variables from delcode
dfmonths$cal_month<-month(dfmonths$delcode)
dfmonths$cal_year<-year(dfmonths$delcode)

#create df_date variable as MM-YYYY
dfmonths$df_date<-paste0(dfmonths$cal_month, "-", dfmonths$cal_year)

#filter del codes >0
dfmonths<-dfmonths%>% filter(delivery_code_present > 0)

#create dfmonths_sum - shows no of del codes by date and patient ID 
dfmonths_sum<-dfmonths%>%group_by(patient_id, df_date)%>%summarise(delivery_code_present)

#arrange by descending date - NEED TO CHECK as v few dates in dummy data
dfmonths_sum<-dfmonths%>%group_by(patient_id, df_date)%>%summarise(delivery_code_present)%>%arrange(desc(df_date))

#filter by first row to get last date in study period
dfmonths_sum<-dfmonths_sum%>% filter(row_number()==1)

#add rules for variables, eg remove v low and v high BMI
#create overall df and df before and after pandemic




#   this is from del code table
#   here:: ("output", "measures", "input_*.csv.gz"),
#    and then group by month and find mean per person? 
#    and/or do we want distribution
#
#   col_types = cols(patient_id = col_integer(),pn8wk_code_number = col_number())
# )

#have tested code below on measures file for one month

#  plot_pn8wk_code_frequency <- ggplot(data=dfmonths, aes(dfmonths$pn8wk_code_number)) + 
#  geom_histogram() +
#  labs (title = "Pn 8wk Code Distribution",
#  x = "No. of postnatal codes")

#  ggsave(
#    plot= plot_pn8wk_code_frequency,
#    filename="pn8wk_code_histogram.jpeg", path=here::here("output"),
#  )

# #table for no. of codes by frequency
# table_pn_codes <- as.data.frame(table(dfmonths$delivery_code)) 

# #table above sorted by frequency, rename var1?
# table_pn_codes <- table_pn_codes[order(-table_del_codes$Freq),]

# write_csv(table_pn_codes, here::here("output","table_pn_codes.csv"))
