#### TO DO
# 1. figure out which files we need and how to combine them - DONE
# 2. write code to combine files - DONE
# 3. write code for counts - table and/or plot

# what do we want - no of codes per person per month
# average isnt useful - number of zeroes?

library('tidyverse')
library('lubridate')
library('dplyr')

#setwd("C:/Users/mdehsdh7/GitHub/uom_pregnancy_tx_pathways/output/measures")

#combine all "input_measures" files 
dfmonths=list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

#create delcode variable as date of delivery code
dfmonths$delcode<-as.Date(dfmonths$delivery_code_date)

#create month and year variables from delcode
dfmonths$cal_month<-month(dfmonths$delcode)
dfmonths$cal_year<-year(dfmonths$delcode)

#create df_date variable as MM-YYYY
dfmonths$df_date<-paste0(dfmonths$cal_month, "-", dfmonths$cal_year)

#filter del codes >0 -- remove?
#dfmonths=dfmonths%>% filter(delivery_code_present > 0)

#Df_sum<- Df %>% group_by(PatientID, Mon-year-var) %>% summarise(xyz)
#creates dfmonths_sum
dfmonths_sum<-dfmonths%>%group_by(patient_id, df_date)%>%summarise(delivery_code_number)
#this shows patient IDs in order
#with a row for each month showing number of del codes

##### DATE IS BASED ON DEL CODE DATE so we don't have 51 rows for each person, only a row for each del code date

#look at how many months (per person) have a zero?
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
