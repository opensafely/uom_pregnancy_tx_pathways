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
  select(age, delivery_code_number,prior_deliveries,
         antenatal_num,postterm_num,blightedovum_num,
         ectopic_num,miscarriage_num,molar_num,
         stillbirth_num,loss_any_num,multips_num,preeclampsia_num,
         top_num,top_probable_num,tops_any_num, lmp_num,edd_num,
         edc_num) 

# Function to summarise each variable
summary_variable <- function(variables_names_contin) {
  variables_names_contin %>%
    summarise(across(everything(), list(
      min = ~min(.),
      mean = ~mean(.),
      max = ~max(.),
      sd = ~sd(.)
    ), .names = "{col}_{fn}"))
}


summary_df <- summary_variable(variables_names_contin)
summary_df <- t(summary_df)
summary_df <- as.data.frame(summary_df)
colnames(summary_df)[1]<- "Value"
summary_df$Value <- round(summary_df$Value,3)
summary_df$variable <- row.names(summary_df)
summary_df <- summary_df[,c(2,1)]
write_csv(summary_df, here::here("output", "SDR_table_minmax.csv"))