library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')

setwd(here::here("output"))

df <- read_csv(here::here("output","input_SDR.csv.gz"),
               col_types = list(
                delivery_code_date = "D",
                age = "n",
                age_cat = "f",
                sex = "f",
                region = "f",
                imd = "f",
                delivery_code = "c",
                delivery_code_number =  "n",
                prior_deliveries =  "n",
                PN_code = "f",
                postnatal_code =  "c",
                antenatal_num =  "n",
                postterm_num =  "n",
                blightedovum_num =  "n",
                ectopic_num =  "n",
                miscarriage_num =  "n",
                molar_num =  "n",
                stillbirth_num =  "n",
                loss_any_num =  "n",
                multips_num =  "n",
                preeclampsia_num =  "n",
                top_num =  "n",
                top_probable_num =  "n",
                tops_any_num =  "n",
                lmp_num =  "n",
                edd_num =  "n",
                edc_num =  "n",
                hbp_pregnancy = "f",
                hbp_all =  "f",
                hbp_any =  "f",
                patient_id =  "n"))

  #df$delivery_code_date<-as.Date(df$delivery_code_date)


############ how many unique patients in each cohort
n_pats <- round(length(unique(df$patient_id))/5)*5
n_pats_df <- data.frame(n_pats)
write_csv(n_pats_df, here::here("output", "patient_numbers_SDR.csv"))

rm(n_pats, n_pats_df)


# select continuous variables for the baseline table
variables_names_contin <- df %>% 
  select(patient_id, age, delivery_code_number,prior_deliveries,
              antenatal_num,postterm_num,blightedovum_num,
              ectopic_num,miscarriage_num,molar_num,
              stillbirth_num,loss_any_num,multips_num,preeclampsia_num,
              top_num,top_probable_num,tops_any_num, lmp_num,edd_num,
              edc_num) 

colsfortab <- colnames(variables_names_contin)

df %>%
  summary_factorlist(explanatory=colsfortab) -> t_continuous_overall 
summary_table<-(t_continuous_overall[-1,])

write_csv(summary_table, here::here("output", "SDR_table_continuous_vars_overall.csv"))



## categorical variables and rounding 
# select categorical variables for the baseline table - for rounding counts. 

#overall categorical
variables_names_categorical <- df %>% 
  select(patient_id, age_cat, imd, PN_code, 
         hbp_pregnancy, hbp_all,hbp_any)
colsfortab=colnames(variables_names_categorical)

df %>%
  summary_factorlist(explanatory=colsfortab) -> t_categorical_overall
summary_table2<-(t_categorical_overall[-1,])

## round to 5
## split cols out
# Function to round counts to the nearest 5 and recalculate percentages
round_counts <- function(summary_table2, col_range) {
  # Extract counts
  counts <- lapply(summary_table2[, col_range, drop = FALSE], function(x) as.numeric(sub("\\s*\\(.*", "", x)))
  
  # Round counts to the nearest 5
  rounded_counts <- lapply(counts, function(x) round(x / 5) * 5)
  
  # Update the columns with rounded counts 
  summary_table2[, col_range] <- lapply(rounded_counts, function(x) paste(x))
  
  return(summary_table2)
}

# Apply the function to the data by specifying column numbers
data_overall <- round_counts(summary_table2, 3)

# Redact any counts < 7 
data_overall <- data_overall %>%
                  mutate(across(3, ~ ifelse(as.numeric(.) <= 7, "redacted", .)))

write_csv(data_overall, here::here("output", "SDR_table_categorical_overall.csv"))

 

