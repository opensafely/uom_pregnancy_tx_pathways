library('tidyverse')

options(scipen=99999)

df_1 <- read_csv(
  here::here("output", "table_del_codes_with_names.csv"),
  col_types = cols(code = col_number(),term = col_character())
)

#changes counts under 6 to "n" - what is best notation
#have checked this works in R 
df_2 <- df_1 %>% mutate(Freq = case_when(Freq < 6 ~ "n"))

## example code
#mutate
#    n = case_when(n>7 ~ n),
#        n = round(n/5)*5,
#    total = case_when (total<7 ~ total),
#        total = round(total/5)*5,
#    pcent_total = n/total*100)

write_csv(df_2, here::here("output","table_del_codes_with_names_reduced.csv"))