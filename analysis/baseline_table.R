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
##dfmonths$df_date<-paste0(dfmonths$cal_month, "-", dfmonths$cal_year)


## 1. Define/clean variables before splitting dfs

# bmi - numeric
# remove bmi outliers - this replaces <8 or >50 with NA
df$bmi <- ifelse(df$bmi <8 | df$bmi>50, NA, df$bmi)
df <- df %>% mutate(bmi_cat = case_when(is.na(bmi) ~ "Unknown",
                                    bmi>=8 & bmi< 18.5 ~ "Underweight",
                                    bmi>=18.5 & bmi<=24.9 ~ "Healthy weight",
                                    bmi>24.9 & bmi<=29.9 ~ "Overweight",
                                    bmi>29.9 ~"Obese"))
df$bmi_cat <- as.factor(df$bmi_cat)

# imd as a factor - do these have/need labels?
df$imd <- as.factor(df$imd)

## ethnicity
# replace NA with 6 ("unknown") then convert to factor
df$ethnicity=ifelse(is.na(df$ethnicity),"6",df$ethnicity)
df <- df %>% mutate(ethnicity_6 = case_when(ethnicity == 1 ~ "White",
                                 ethnicity == 2  ~ "Mixed",
                                 ethnicity == 3  ~ "South Asian",
                                 ethnicity == 4  ~ "Black",
                                 ethnicity == 5  ~ "Other",
                                 ethnicity == 6   ~ "Unknown"))
df$ethnicity_6 <- as.factor(df$ethnicity_6)

#Create before/after pandemic dfs, keep overall
# use del_code_date to split?
# check dates and date format - before/after first lockdown?
df_overall <- df
df_before <- df %>% filter(del_code_date < "2020-03-01") 
df_after <- df %>% filter(del_code_date >= "2020-03-01") 

#dates from plots
#  as.Date("2021-01-01"),xmax = as.Date("2021-04-01")
#  as.Date("2020-11-01"),xmax = as.Date("2020-12-01")
#  as.Date("2020-03-01"),xmax = as.Date("2020-06-01")

## 2. group by patient ID and sort in ascending order
df_overall2 <- df %>% group_by(patient_id)
df_before2 <- df_before %>% group_by(patient_id)
df_after2 <- df_after %>% group_by(patient_id)

## 3. filter del codes >0
df_overall3 <- df_overall2 %>% filter(delivery_code_present > 0)
df_before3 <- df_before2 %>% filter(delivery_code_present > 0)
df_after3 <- df_after2 %>% filter(delivery_code_present > 0)

## 4. arrange by patient ID then del code date
# then filter by first row to get last date in study period
#arrange(data, by_group = TRUE)
#or just arrange by both? line below works
df_overall4 <- df_overall3 %>% arrange(patient_id, desc(delivery_code_date)) %>% filter(row_number()==1)
df_before4 <- df_before3 %>% arrange(patient_id, desc(delivery_code_date)) %>% filter(row_number()==1)
df_after4 <- df_after3 %>% arrange(patient_id, desc(delivery_code_date)) %>% filter(row_number()==1)

## old code - do we need to do this?
#create df_sum - shows no of del codes by date and patient ID 
#df_sum<-df%>%group_by(patient_id, delivery_code_date)%>%summarise(delivery_code_present)

## code for overall counts - use for paper to get number of practices etc
# think about which vars we need this for
# overall_counts <- as.data.frame(cbind(first_mon, last_mon, num_pats, num_pracs))
# write_csv(overall_counts, here::here("output", "overall_counts_blt_2020.csv"))
# rm(overall_counts) 

## 5. Create summary table

# select variables for the baseline table
# bmi and bmi cat? are we looking at averages?
bltab_vars <- df_overall %>% select(age, age_cat, bmi, bmi_cat, del_code_number, ethnicity_6, imd) 

# # columns for baseline table
colsfortab <- colnames(bltab_vars)

#bltab_vars %>% summary_factorlist(explanatory = colsfortab) -> t
# #str(t)
#write_csv(t, here::here("output", "blt_one_random_obs_perpat_2020.csv"))


# ## matching variable for covrx extraction
# DF=df_one_pat%>%select(patient_id, date)
# DF$patient_index_date=as.character(DF$date)
# write_csv(DF, here::here("output","measures", "id_2020.csv"))