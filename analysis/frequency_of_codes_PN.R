library('tidyverse')
library('lubridate')
library('dplyr')


options(scipen=99999)
#load delivery codes
df_1 <- read_csv(
  here::here("codelists", "user-VickiPalin-pregnancy_postnatal_8wk_snomed_reviewed.csv"),
  col_types = cols(code = col_number(),term = col_character())
)


setwd(here::here("output", "joined_8wk"))

df <-read_rds('cohort_selection_one_random_obvs_8wk.rds')
df <- df %>% dplyr::filter(delivery_code_number >0)


## table PN codes. 
table_PN_codes <- as.data.frame(table(df$postnatal_code)) 

#rename first column in frequency table to match
colnames(table_PN_codes)[1]<-"code"

#merge by code number to return full description of codes which are in both files
df_3<-merge(table_PN_codes, df_1, by="code")
df_3 <- df_3[order(-df_3$Freq),]


### redacting and rounding
df_3$redacted <- df_3$Freq
df_3$redacted[which(df_3$redacted <= 7)] <- NA
df_3$redacted<-round(df_3$redacted/5)*5
df_3<-df_3[,-2]
write_csv(df_3, here::here("output","postnatal_codes_count.csv"))

