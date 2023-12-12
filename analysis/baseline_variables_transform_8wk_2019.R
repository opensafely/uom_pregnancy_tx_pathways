## Import libraries---
library("tidyverse") 
library('dplyr')
library('lubridate')

rm(list=ls())
setwd(here::here("output", "joined_8wk"))

# file list
csvFiles_19 = list.files(pattern="input_updated_2019", full.names = FALSE)

# date list
date_19= seq(as.Date("2019-01-01"), as.Date("2019-12-01"), "month")

temp <- vector("list", length(csvFiles_19))
for (i in seq_along(csvFiles_19)){
  temp_df <- read_csv((csvFiles_19[i]),
                      col_types = cols_only(
                        #bmi_date_measured = col_date(format = "")
                        delivery_code_date = col_date(format = ""),
                        age = col_integer(),
                        age_cat = col_factor(),
                        sex = col_factor(),
                        #practice = col_integer(),
                        region = col_factor(),
                        imd = col_factor(),
                        bmi = col_number(),
                        gp_count = col_number(),
                        delivery_code_number = col_number(),
                        delivery_code_present = col_factor(),
                        delivery_code = col_character(),
                        pn8wk_code_number = col_number(),
                        postnatal_8wk_code_present = col_factor(),
                        #postnatal_code = col_character(),
                        #postnatal_other_code_present = col_factor(),
                        postnatal_antenatal_code_present = col_factor(),
                        Covid_test_result_sgss = col_factor(),
                        gp_covid = col_factor(), 
                        cancer_comor = col_integer(),
                        cardiovascular_comor = col_integer(),
                        chronic_obstructive_pulmonary_comor = col_integer(),
                        heart_failure_comor = col_integer(),
                        connective_tissue_comor = col_integer(),
                        dementia_comor = col_integer(),
                        diabetes_comor = col_integer(),
                        diabetes_complications_comor = col_integer(),
                        hemiplegia_comor = col_integer(),
                        hiv_comor = col_integer(),
                        metastatic_cancer_comor = col_integer(),
                        mild_liver_comor = col_integer(),
                        mod_severe_liver_comor = col_integer(),
                        mod_severe_renal_comor = col_integer(),
                        mi_comor = col_integer(),
                        peptic_ulcer_comor = col_integer(),
                        peripheral_vascular_comor = col_integer(),
                        hbp_pregnancy = col_integer(),
                        hbp_all = col_integer(),
                        hbp_any = col_integer(),
                        bp = col_number(),
                        patient_id = col_integer(),
                        ethnicity = col_factor(),
                        eth = col_factor(),
                        ethnicity_sus = col_factor(),
                        ethnicity2 = col_factor()
                      ),
                      na = character()
  )
  
  temp_df$date=date_19[i]
  
  #add df to list
  temp[[i]] <- temp_df
  rm(temp_df)
}

dat=dplyr::bind_rows(temp)
rm(temp,i,date_19,csvFiles_19)

dat$date <- as.Date(dat$date)

saveRDS(dat, "basic_joined_8wk_records_2019.rds")
rm(dat)
