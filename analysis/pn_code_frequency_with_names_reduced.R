library('tidyverse')

options(scipen=99999)

df1 <- read_csv(
  here::here("output", "table_pn_codes_reviewed_with_names.csv"),
  col_types = cols(code = col_number(),Freq=col_number(), term = col_character())
)

#changes counts under 6 to "[REDACTED]" (now -9)
df2 <- df1 
df2$Freq <- ifelse(df2$Freq <= 7, -9, df2$Freq)
df2$Freq <- as.numeric(df2$Freq)

#rounding to nearest 5
df3<-df2
df3$Freq <- round(df3$Freq/5)*5

write_csv(df3, here::here("output","table_pn_codes_reviewed_with_names_reduced.csv"))

