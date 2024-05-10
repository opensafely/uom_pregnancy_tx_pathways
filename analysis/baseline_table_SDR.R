library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')

setwd(here::here("output"))

df <- read_csv(here::here("output","input_SDR.csv.gz"))

df$delivery_code_date<-as.Date(df$delivery_code_date)


############ how many unique patients in each cohort
n_pats <- round(length(unique(df$patient_id))/5)*5
n_pats_df <- data.frame(n_pats)
write_csv(n_pats_df, here::here("output", "patient_numbers_SDR.csv"))

rm(n_pats, n_pats_df)


df$imd <- as.factor(df$imd)
df$region <- as.factor(df$region)
df$age_cat <- as.factor(df$age_cat)
df$PN_code <- as.factor(df$PN_code)
df$hbp_pregnancy <- as.factor(df$hbp_pregnancy)
df$hbp_all <- as.factor(df$hbp_all)
df$hbp_any <- as.factor(df$hbp_any)


# select continuous variables for the baseline table
variables_names_contin <- df %>% 
  select(patient_id, age, delivery_code_number,prior_deliveries,
              antenatal_num,postterm_num,blightedovum_num,
              ectopic_num,miscarriage_num,molar_num,
              stillbirth_num,loss_any_num,multips_num,preeclampsia_num,
              top_num,top_probable_num,tops_any_num, lmp_num,edd_num,
              edc_num) 

explanatory=colnames(variables_names_contin)
dependent="region"

df %>%
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE) -> t_continuous_region 
summary_table<-(t_continuous_region[-1,])

write_csv(summary_table, here::here("output", "SDR_table_continuous_vars_by_region.csv"))


dependent="age_cat"
df %>%
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE) -> t_continuous_age
summary_table<-(t_continuous_age[-1,])
write_csv(summary_table, here::here("output", "SDR_table_continuous_vars_by_age.csv"))


## categorical variables and rounding 
# select categorical variables for the baseline table - for rounding counts. 


#region
variables_names_categorical <- df %>% 
  select(patient_id, age_cat, imd, PN_code, 
         hbp_pregnancy, hbp_all,hbp_any)
explanatory=colnames(variables_names_categorical)
dependent="region"

df %>%
  summary_factorlist(dependent, explanatory, na_include = TRUE) -> t_categorical_region 
summary_table2<-(t_categorical_region[-1,])

## round to 5
## split cols out
# Function to round counts to the nearest 5 and recalculate percentages
round_counts <- function(summary_table2, col_range) {
  # Extract counts
  counts <- lapply(summary_table2[, col_range], function(x) as.numeric(sub("\\s*\\(.*", "", x)))
  
  # Round counts to the nearest 5
  rounded_counts <- lapply(counts, function(x) round(x / 5) * 5)
  
  # Update the columns with rounded counts 
  summary_table2[, col_range] <- lapply(rounded_counts, function(x) paste(x))
  
  return(summary_table2)
}

# Apply the function to the data by specifying column numbers
data_region <- round_counts(summary_table2, 3:11)

# Redact any counts < 7 
data_region <- data_region %>%
                  mutate(across(3:11, ~ ifelse(as.numeric(.) <= 7, "redacted", .)))



write_csv(data_region, here::here("output", "SDR_table_categorical_region.csv"))
#write_csv(data, here::here("output", "blt_6v12_weeks_categorical_rounded.csv"))

 

# age_cat
variables_names_categorical <- df %>% 
  select(patient_id, region, imd, PN_code, 
         hbp_pregnancy, hbp_all,hbp_any)
explanatory=colnames(variables_names_categorical)
dependent="age_cat"

df %>%
  summary_factorlist(dependent, explanatory, na_include = TRUE) -> t_categorical_age
summary_table2<-(t_categorical_age[-1,])


## round to 5
## split cols out
# Function to round counts to the nearest 5 and recalculate percentages
round_counts <- function(summary_table2, col_range) {
  # Extract counts
  counts <- lapply(summary_table2[, col_range], function(x) as.numeric(sub("\\s*\\(.*", "", x)))
  
  # Round counts to the nearest 5
  rounded_counts <- lapply(counts, function(x) round(x / 5) * 5)
  
  # Update the columns with rounded counts 
  summary_table2[, col_range] <- lapply(rounded_counts, function(x) paste(x))
  
  return(summary_table2)
}

# Apply the function to the data by specifying column numbers
data_age <- round_counts(summary_table2, 3:9)

# Redact any counts < 7 
data_age <- data_age %>%
  mutate(across(3:9, ~ ifelse(as.numeric(.) <= 7, "redacted", .)))



write_csv(data_age, here::here("output", "SDR_table_categorical_age.csv"))
#write_csv(data, here::here("output", "blt_6v12_weeks_categorical_rounded.csv"))

## overall table:
# columns for baseline table
colsfortab <- colnames(df)
df %>% summary_factorlist(explanatory = colsfortab) -> t_overall
t_overall<-(t_overall[-1,])
write_csv(t_overall, here::here("output", "SDR_table_all.csv"))
