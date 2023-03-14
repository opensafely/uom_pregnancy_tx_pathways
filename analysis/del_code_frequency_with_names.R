library('tidyverse')

options(scipen=99999)

#load delivery codes
df_1 <- read_csv(
  here::here("codelists", "user-VickiPalin-pregnancy_delivery_snomed.csv"),
  col_types = cols(code = col_number(),term = col_character())
)

#load frequency of codes
df_2 <- read_csv(
  here::here("output", "table_del_codes.csv"),
  col_types = cols(code = col_number(),frequency = col_number())
)

#changed integer to number above - got error

#rename first column in frequency table to match
colnames(df2)[1]<-"code"

#return which codes are in both files
#df1$code %in% df2$code

df3<-merge(df2, df1, by="code")

#do we need to add term to this
table_del_codes_with_names <- as.data.frame(table(df_3$code)) 
table_del_codes_with_names <- table_del_codes_with_names[order(-table_del_codes_with_names$Freq),]

write_csv(table_del_codes_with_names, here::here("output","table_del_codes_with_names.csv"))
