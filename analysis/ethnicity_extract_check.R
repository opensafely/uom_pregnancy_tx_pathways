library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')
#library('fs')

#dir_create(here::here("output", "joined_8wk"), showWarnings = FALSE, recurse = TRUE)


setwd(here::here("output"))


df <- read_csv('input_ethnicity.csv.gz')

df$ethnicity <- as.factor(df$ethnicity)
df$eth <- as.factor(df$eth)
df$ethnicity_sus <- as.factor(df$ethnicity_sus)
df$ethnicity2 <- as.factor(df$ethnicity2)
df$eth_old <- as.factor(df$eth_old)

## ethnicity (based on snomed codelist)
## https://www.opencodelists.org/codelist/opensafely/ethnicity-snomed-0removed/2e641f61/
#df2$ethnicity=ifelse(is.na(df2$ethnicity), "6", df2$ethnicity)
df <- df %>% 
  dplyr::mutate(ethnicity_labs = case_when(is.na(ethnicity) ~ "Unknown",
                                           ethnicity == 1 ~ "White",
                                           ethnicity == 2  ~ "Mixed",
                                           ethnicity == 3  ~ "Asian or Asian British",
                                           ethnicity == 4  ~ "Black or Black British",
                                           ethnicity == 5  ~ "Chinese or Other Ethnic Groups",
                                           ethnicity == 6  ~ "Unknown"))
df$ethnicity_labs <- as.factor(df$ethnicity_labs)


df <- df %>% 
  dplyr::mutate(ethnicity2_labs = case_when(is.na(ethnicity2) ~ "Unknown",
                                           ethnicity2 == 1 ~ "White",
                                           ethnicity2 == 2  ~ "Mixed",
                                           ethnicity2 == 3  ~ "Asian or Asian British",
                                           ethnicity2 == 4  ~ "Black or Black British",
                                           ethnicity2 == 5  ~ "Chinese or Other Ethnic Groups",
                                           ethnicity2 == 0  ~ "Unknown"))
df$ethnicity2_labs <- as.factor(df$ethnicity2_labs)


# columns for baseline table
colsfortab <- colnames(df)
#bltab_vars %>% summary_factorlist(explanatory = colsfortab, cont_cut = 10) -> t
df %>% summary_factorlist(explanatory = colsfortab) -> t
t<-(t[-28,])
write_csv(t, here::here("output", "blt_new_ethnicity_extract_check.csv"))



