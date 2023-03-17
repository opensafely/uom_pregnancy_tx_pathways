library('tidyverse')

options(scipen=99999)

df_1 <- read_csv(
  here::here("output", "table_del_codes_with_names.csv"),
  col_types = cols(code = col_number(),term = col_character())
)

df_2=df_1%>% mutate(
Freq = case_when( Freq > 5 ~ ),
)

write_csv(df_2, here::here("output","table_del_codes_with_names_reduced.csv"))