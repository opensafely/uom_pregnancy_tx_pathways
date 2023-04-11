library('tidyverse')
library('lubridate')
library('dplyr')

#setwd("C:/Users/mdehsdh7/GitHub/uom_pregnancy_tx_pathways/output/measures")

setwd(here::here("output", "measures"))

#combine all "input_measures" files 
df<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

df$delivery_code_date<-as.Date(df$delivery_code_date)

##dont need the below as we can use del code date?

##dfmonths$delcode<-as.Date(dfmonths$delivery_code_date)
##dfmonths$cal_month<-month(dfmonths$delcode)
##dfmonths$cal_year<-year(dfmonths$delcode)

##create df_date variable as MM-YYYY
##dfmonths$df_date<-paste0(dfmonths$cal_month, "-", dfmonths$cal_year)

# 1. group by patient ID
# 2. filter del codes >0
# 3. arrange by descending del_code _date
# 4. keep first row per group

#group by patient ID and sort in ascending order
df<-df%>%group_by(patient_id)%>%arrange(patient_id)
#or group by patient id and arrange by del code date desc?

#filter del codes >0
df<-df%>% filter(delivery_code_present > 0)

#ARRANGE IGNORES GROUPING - check order
#df<-df%>%arrange(desc(delivery_code_date, by_group=TRUE)

#^have tested this with del code number and still arranges by that

##arrange(data, by_group = TRUE)     to override?

#create df_sum - shows no of del codes by date and patient ID 
#df_sum<-df%>%group_by(patient_id, delivery_code_date)%>%summarise(delivery_code_present)

#arrange by descending date - NEED TO CHECK as v few dates in dummy data so not sure if works
#df<-df%>%group_by(patient_id, delivery_code_date)%>%arrange(desc(delivery_code_date))

#df<-df%>%arrange(desc(delivery_code_date))

#%>%group_by(patient_id, df_date)%>%summarise(delivery_code_present)%>%arrange(desc(df_date))

#filter by first row to get last date in study period
#df<-df%>% filter(row_number()==1)

## then add variables
## age, bmi, imd, eth, number of del codes?


#add rules for variables, eg remove v low and v high BMI
# remove bmi outliers - check df names
#df$bmi <- ifelse(df_$bmi <8 | df$bmi>50, NA, df$bmi)

df$imd<-as.factor(df$imd)

## ethnicity
df$ethnicity=ifelse(is.na(df$ethnicity),"6",df$ethnicity)
df<-df%>% mutate(ethnicity_6 = case_when(ethnicity == 1 ~ "White",
                                 ethnicity == 2  ~ "Mixed",
                                 ethnicity == 3  ~ "South Asian",
                                 ethnicity == 4  ~ "Black",
                                 ethnicity == 5  ~ "Other",
                                 ethnicity == 6   ~ "Unknown"))
df$ethnicity_6 <- as.factor(df$ethnicity_6)

## code for overall counts - use for paper
# overall_counts <- as.data.frame(cbind(first_mon, last_mon, num_pats, num_pracs))
# write_csv(overall_counts, here::here("output", "overall_counts_blt_2020.csv"))
# rm(overall_counts) 


## then create overall df and df before and after pandemic




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