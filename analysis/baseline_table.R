library('tidyverse')
library('lubridate')
library('dplyr')

#setwd("C:/Users/mdehsdh7/GitHub/uom_pregnancy_tx_pathways/output/measures")

setwd(here::here("output", "measures"))

#combine all "input_measures" files 
df<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

df$delivery_code_date<-as.Date(df$delivery_code_date)

##dont need the below as we can use del code date?
## although after code above it is still a character?
##dfmonths$delcode<-as.Date(dfmonths$delivery_code_date)
##dfmonths$cal_month<-month(dfmonths$delcode)
##dfmonths$cal_year<-year(dfmonths$delcode)
##create df_date variable as MM-YYYY
##dfmonths$df_date<-paste0(dfmonths$cal_month, "-", dfmonths$cal_year)

### start from here
# 1. group by patient ID
# 2. filter del codes >0
# 3. arrange by descending del_code _date
# 4. keep first row per group
# 5. define/check/add rules for variables
# 6. create overall/before/after dfs
# 7. summary tables

## 1. group by patient ID and sort in ascending order
df2<-df%>%group_by(patient_id)

## 2. filter del codes >0
df3<-df2%>% filter(delivery_code_present > 0)

## 3. arrange by patient ID then del code date
#arrange(data, by_group = TRUE)
#or just arrange by both? line below works
df4<-df3%>%arrange(patient_id, desc(delivery_code_date))

## 4. filter by first row to get last date in study period
df5<-df4%>% filter(row_number()==1)

## old code - do we need to do this?
#create df_sum - shows no of del codes by date and patient ID 
#df_sum<-df%>%group_by(patient_id, delivery_code_date)%>%summarise(delivery_code_present)

## 5. variables
# we have age (numeric) and age cat (char)
# eth as 1-6, bmi as a number,
# imd as 1-5, number of del codes?

# bmi - numeric
# remove bmi outliers - this replaces <8 or >50 with NA
df5$bmi <- ifelse(df5$bmi <8 | df5$bmi>50, NA, df5$bmi)
# do we want to add categories?
# this works in R, just takes a while to run
df5<-df5%>%mutate(bmi_cat = case_when(is.na(bmi) ~ "unknown",
                                    bmi>=8 & bmi< 18.5 ~ "underweight",
                                    bmi>=18.5 & bmi<=24.9 ~ "healthy weight",
                                    bmi>24.9 & bmi<=29.9 ~ "overweight",
                                    bmi>29.9 ~"obese"))
df5$bmi_cat<-as.factor(df5$bmi_cat)

# imd as a factor
df5$imd<-as.factor(df5$imd)

## ethnicity
# replace NA with 6 ("unknown") then convert to factor
# works in R, also takes a while 
df5$ethnicity=ifelse(is.na(df5$ethnicity),"6",df5$ethnicity)
df5<-df5%>% mutate(ethnicity_6 = case_when(ethnicity == 1 ~ "White",
                                 ethnicity == 2  ~ "Mixed",
                                 ethnicity == 3  ~ "South Asian",
                                 ethnicity == 4  ~ "Black",
                                 ethnicity == 5  ~ "Other",
                                 ethnicity == 6   ~ "Unknown"))
df5$ethnicity_6 <- as.factor(df5$ethnicity_6)

## code for overall counts - use for paper to get number of practices etc
# overall_counts <- as.data.frame(cbind(first_mon, last_mon, num_pats, num_pracs))
# write_csv(overall_counts, here::here("output", "overall_counts_blt_2020.csv"))
# rm(overall_counts) 


## 6. Create before/after pandemic dfs, keep overall
# use del_code_date to split?
df_overall<-df5


###code from other repo for table - check vars and names

# ## select variables for the baseline table
# bltab_vars <- select(df_one_pat, date, age, age_cat, sex, bmi, 
#                      bmi_cat, ethnicity_6, charlsonGrp, smoking_cat, 
#                      flu_vaccine, gp_count, antibacterial_brit,
#                      antibacterial_12mb4, broad_spectrum_antibiotics_prescriptions, 
#                      Covid_test_result_sgss, imd, hx_indications, hx_antibiotics, 
#                      covrx, died_ever) 
# # generate data table 


# # columns for baseline table
# colsfortab <- colnames(bltab_vars)
# bltab_vars %>% summary_factorlist(explanatory = colsfortab) -> t
# #str(t)
# write_csv(t, here::here("output", "blt_one_random_obs_perpat_2020.csv"))


# ## matching variable for covrx extraction
# DF=df_one_pat%>%select(patient_id, date)
# DF$patient_index_date=as.character(DF$date)
# write_csv(DF, here::here("output","measures", "id_2020.csv"))