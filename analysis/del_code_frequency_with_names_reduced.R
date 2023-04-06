library('tidyverse')

options(scipen=99999)

df1 <- read_csv(
  here::here("output", "table_del_codes_with_names.csv"),
  col_types = cols(code = col_number(),Freq=col_number(), term = col_character())
)

#changes counts under 6 to "[REDACTED]"
df2 <- df1 
df2$Freq <- ifelse(df2$Freq <= 7, "[REDACTED]", df2$Freq)
df2$Freq <- as.numeric(df2$Freq)

#rounding to nearest 5
df3<-df2
df3$Freq <- round(df3$Freq/5)*5

## example code
#mutate
#    n = case_when(n>7 ~ n),
#        n = round(n/5)*5,
#    total = case_when (total<7 ~ total),
#        total = round(total/5)*5,
#    pcent_total = n/total*100)

write_csv(df3, here::here("output","table_del_codes_with_names_reduced.csv"))

