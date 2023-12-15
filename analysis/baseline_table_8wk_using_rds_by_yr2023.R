library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')
#library('fs')

#dir_create(here::here("output", "joined_8wk"), showWarnings = FALSE, recurse = TRUE)


setwd(here::here("output", "joined_8wk"))

#combine all "input_*" monthly files 
#df<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()
df <- read_rds('basic_joined_8wk_records_2023.rds')

## count patients with delivery codes (potential record for patient each month)
tabdelcodes <- as.data.frame(table(df$delivery_code_present))
write_csv(tabdelcodes, here::here("output", "table_delcodes_8wk_update_2023.csv"))
rm(tabdelcodes)

## filter del codes >0 (must be numeric)
df$delivery_code_present <- as.numeric(df$delivery_code_present)
##test 0 and 1 or 1 and 2 after converted to numeric for filtering
tabdelcodes <- as.data.frame(table(df$delivery_code_present))
write_csv(tabdelcodes, here::here("output", "table_delcodes_8wk_update_2023_after_numeric.csv"))
rm(tabdelcodes)

df <- df %>% dplyr::filter(delivery_code_present > 1)
tabdelcodes <- as.data.frame(table(df$delivery_code_present))
write_csv(tabdelcodes, here::here("output", "table_delcodes_8wk_update_2023_after_filter.csv"))
rm(tabdelcodes)

df$delivery_code_present <- as.factor(df$delivery_code_present)

####
## filter to one observation per patient 
df$delivery_code_date<-as.Date(df$delivery_code_date)
## group by patient ID, then arrange by most recent delivery code date
## take most recent code per patient in period. 
df2 <- df %>% group_by(patient_id)%>% arrange(desc(delivery_code_date)) %>% filter(row_number()==1)
rm(df)

## count overall practices and patients:
num_pracs <- length(unique(df2$practice))
num_pats <- length(unique(df2$patient_id))
overall_counts <- as.data.frame(cbind(num_pats, num_pracs))
write_csv(overall_counts, here::here("output", "overall_counts_8wk_update_2023.csv"))

## Define/clean variables before baseline table

# bmi - numeric
# remove bmi outliers - this replaces <8 or >50 with NA
df2$bmi <- ifelse(df2$bmi <8 | df2$bmi>50, NA, df2$bmi)
df2 <- df2 %>% mutate(bmi_cat = case_when(is.na(bmi) ~ "Unknown",
                                    bmi>=8 & bmi< 18.5 ~ "Underweight",
                                    bmi>=18.5 & bmi<=24.9 ~ "Healthy weight",
                                    bmi>24.9 & bmi<=29.9 ~ "Overweight",
                                    bmi>29.9 ~"Obese"))
df2$bmi_cat <- as.factor(df2$bmi_cat)

df2$imd <- as.factor(df2$imd)

df2$region <- as.factor(df2$region)
df2 <- df2 %>% group_by(region) %>% filter(n() >= 40)
ungroup(df2)

df2$ethnicity <- as.factor(df2$ethnicity)
df2$ethnicity2 <- as.factor(df2$ethnicity2)
df2$eth <- as.factor(df2$eth)
df2$ethnicity_sus <- as.factor(df2$ethnicity_sus)

## ethnicity (based on snomed codelist)
## https://www.opencodelists.org/codelist/opensafely/ethnicity-snomed-0removed/2e641f61/
df2$ethnicity=ifelse(is.na(df2$ethnicity),"6",df2$ethnicity)
df2 <- df2 %>% 
  mutate(ethnicity_6 = case_when(ethnicity == 1 ~ "White",
                                 ethnicity == 2  ~ "Mixed",
                                 ethnicity == 3  ~ "Asian or Asian British",
                                 ethnicity == 4  ~ "Black or Blsck British",
                                 ethnicity == 5  ~ "Chinese or Other Ethnic Groups",
                                 ethnicity == 6   ~ "Unknown"))
df2$ethnicity_6 <- as.factor(df2$ethnicity_6)


df2 <- df2 %>% group_by(ethnicity) %>% filter(n() >= 5)
ungroup(df2)

## covid positive
df2 <- df2 %>% mutate(covid_positive = case_when(gp_covid == 1 ~ "1",
                                               Covid_test_result_sgss ==1 ~ "1",
                                               TRUE ~ "0"))
df2$covid_positive<-as.factor(df2$covid_positive)

## create charlson index
df2$cancer_comor<- ifelse(df2$cancer_comor == 1L, 2L, 0L)
df2$cardiovascular_comor <- ifelse(df2$cardiovascular_comor == 1L, 1L, 0L)
df2$chronic_obstructive_pulmonary_comor <- ifelse(df2$chronic_obstructive_pulmonary_comor == 1L, 1L, 0)
df2$heart_failure_comor <- ifelse(df2$heart_failure_comor == 1L, 1L, 0L)
df2$connective_tissue_comor <- ifelse(df2$connective_tissue_comor == 1L, 1L, 0L)
df2$dementia_comor <- ifelse(df2$dementia_comor == 1L, 1L, 0L)
df2$diabetes_comor <- ifelse(df2$diabetes_comor == 1L, 1L, 0L)
df2$diabetes_complications_comor <- ifelse(df2$diabetes_complications_comor == 1L, 2L, 0L)
df2$hemiplegia_comor <- ifelse(df2$hemiplegia_comor == 1L, 2L, 0L)
df2$hiv_comor <- ifelse(df2$hiv_comor == 1L, 6L, 0L)
df2$metastatic_cancer_comor <- ifelse(df2$metastatic_cancer_comor == 1L, 6L, 0L)
df2$mild_liver_comor <- ifelse(df2$mild_liver_comor == 1L, 1L, 0L)
df2$mod_severe_liver_comor <- ifelse(df2$mod_severe_liver_comor == 1L, 3L, 0L)
df2$mod_severe_renal_comor <- ifelse(df2$mod_severe_renal_comor == 1L, 2L, 0L)
df2$mi_comor <- ifelse(df2$mi_comor == 1L, 1L, 0L)
df2$peptic_ulcer_comor <- ifelse(df2$peptic_ulcer_comor == 1L, 1L, 0L)
df2$peripheral_vascular_comor <- ifelse(df2$peripheral_vascular_comor == 1L, 1L, 0L)

## total charlson for each patient 
charlson=c("cancer_comor","cardiovascular_comor","chronic_obstructive_pulmonary_comor",
           "heart_failure_comor","connective_tissue_comor", "dementia_comor",
           "diabetes_comor","diabetes_complications_comor","hemiplegia_comor",
           "hiv_comor","metastatic_cancer_comor" ,"mild_liver_comor",
           "mod_severe_liver_comor", "mod_severe_renal_comor", "mi_comor",
           "peptic_ulcer_comor" , "peripheral_vascular_comor" )

df2$charlson_score=rowSums(df2[charlson])

## Charlson - as a categorical group variable
df2 <- df2 %>%
  mutate(charlsonGrp = case_when(charlson_score >0 & charlson_score <=2 ~ 2,
                                charlson_score >2 & charlson_score <=4 ~ 3,
                                charlson_score >4 & charlson_score <=6 ~ 4,
                                charlson_score >=7 ~ 5,
                                charlson_score == 0 ~ 1))

df2$charlsonGrp <- as.factor(df2$charlsonGrp)
df2$charlsonGrp <- factor(df2$charlsonGrp, 
                                 labels = c("zero", "low", "medium", "high", "very high"))

# select variables for the baseline table
bltab_vars <- df2 %>% select(patient_id, age, age_cat, bmi, bmi_cat, delivery_code_number, region, ethnicity, ethnicity2, ethnicity_6, imd, eth, ethnicity_sus, pn8wk_code_number, postnatal_8wk_code_present, charlsonGrp, covid_positive, hbp_any,
                                     "cancer_comor","cardiovascular_comor","chronic_obstructive_pulmonary_comor",
                                     "heart_failure_comor","connective_tissue_comor", "dementia_comor",
                                     "diabetes_comor","diabetes_complications_comor","hemiplegia_comor",
                                     "hiv_comor","metastatic_cancer_comor" ,"mild_liver_comor",
                                     "mod_severe_liver_comor", "mod_severe_renal_comor", "mi_comor",
                                     "peptic_ulcer_comor" , "peripheral_vascular_comor" ) 

# columns for baseline table
colsfortab <- colnames(bltab_vars)

bltab_vars %>% summary_factorlist(explanatory = colsfortab) -> t
t<-(t[-1,])
write_csv(t, here::here("output", "blt_overall_8wk_update_2023.csv"))



