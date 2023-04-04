#### TO DO
# 1. figure out which files we need and how to combine them
# 2. write code to combine files
# 3. write code for counts - table and/or plot


# then sort/group by patient id?
# what do we want - no of codes per person per month
# average isnt useful - number of zeroes

#csvFiles=list.files(pattern = "input", full.names = FALSE)

library('tidyverse')

dfmonths=list.files(pattern = "input", full.names = FALSE) %>%
     lapply(read.csv, stringsAsFactors=F) %>%
     bind_rows()

View(dfmonths)
colnames(dfmonths)

#still need to group by patient ID?

#   this is from del code table
#   here:: ("output", "measures", "input_*.csv.gz"),
#    and then group by month and find mean per person? 
#    and/or do we want distribution
#
#   col_types = cols(patient_id = col_integer(),pn8wk_code_number = col_number())
# )

#have tested code below on measures file for one month

#dfmonths <- dfmonths %>% filter(pn8wk_code_number >0)

 plot_pn8wk_code_frequency <- ggplot(data=dfmonths, aes(dfmonths$pn8wk_code_number)) + 
 geom_histogram() +
 labs (title = "Pn 8wk Code Distribution",
 x = "No. of postnatal codes")

 ggsave(
   plot= plot_pn8wk_code_frequency,
   filename="pn8wk_code_histogram.jpeg", path=here::here("output"),
 )

#table for no. of codes by frequency
table_pn_codes <- as.data.frame(table(dfmonths$delivery_code)) 

#table above sorted by frequency, rename var1?
table_pn_codes <- table_pn_codes[order(-table_del_codes$Freq),]

write_csv(table_pn_codes, here::here("output","table_pn_codes.csv"))
