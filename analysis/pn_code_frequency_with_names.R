library('tidyverse')

options(scipen=99999)

#load delivery codes
df_1 <- read_csv(
  here::here("codelists", "user-VickiPalin-pregnancy_postnatal_8wk_snomed_reviewed.csv"),
  col_types = cols(code = col_number(),term = col_character())
)

#load frequency of codes
df_2 <- read_csv(
  here::here("output", "table_pn_codes_reviewed.csv"),
  col_types = cols(Var1 = col_number(),Freq = col_number())
)

#rename first column in frequency table to match
colnames(df_2)[1]<-"code"

#merge by code number to return full description of codes which are in both files
df_3<-merge(df_2, df_1, by="code")
df_3 <- df_3[order(-df_3$Freq),]


write_csv(df_3, here::here("output","table_pn_codes_reviewed_with_names.csv"))
