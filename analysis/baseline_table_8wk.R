library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')

#setwd(here::here("output", "pn8wk"))
setwd(here::here("output", "measures_update"))

#combine all "input_measures" files 
df<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

df$delivery_code_date<-as.Date(df$delivery_code_date)

## filter del codes >0
df <- df %>% filter(delivery_code_present > 0)

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

df$imd <- as.factor(df$imd)
df$ethnicity <- as.factor(df$ethnicity)
df$region <- as.factor(df$region)

## covid positive
df$gp_covid<-as.factor(df$gp_covid)
df$Covid_test_result_sgss<- as.factor(df$Covid_test_result_sgss)

## create charlson index
df$cancer_comor<- ifelse(df$cancer_comor == 1L, 2L, 0L)
df$cardiovascular_comor <- ifelse(df$cardiovascular_comor == 1L, 1L, 0L)
df$chronic_obstructive_pulmonary_comor <- ifelse(df$chronic_obstructive_pulmonary_comor == 1L, 1L, 0)
df$heart_failure_comor <- ifelse(df$heart_failure_comor == 1L, 1L, 0L)
df$connective_tissue_comor <- ifelse(df$connective_tissue_comor == 1L, 1L, 0L)
df$dementia_comor <- ifelse(df$dementia_comor == 1L, 1L, 0L)
df$diabetes_comor <- ifelse(df$diabetes_comor == 1L, 1L, 0L)
df$diabetes_complications_comor <- ifelse(df$diabetes_complications_comor == 1L, 2L, 0L)
df$hemiplegia_comor <- ifelse(df$hemiplegia_comor == 1L, 2L, 0L)
df$hiv_comor <- ifelse(df$hiv_comor == 1L, 6L, 0L)
df$metastatic_cancer_comor <- ifelse(df$metastatic_cancer_comor == 1L, 6L, 0L)
df$mild_liver_comor <- ifelse(df$mild_liver_comor == 1L, 1L, 0L)
df$mod_severe_liver_comor <- ifelse(df$mod_severe_liver_comor == 1L, 3L, 0L)
df$mod_severe_renal_comor <- ifelse(df$mod_severe_renal_comor == 1L, 2L, 0L)
df$mi_comor <- ifelse(df$mi_comor == 1L, 1L, 0L)
df$peptic_ulcer_comor <- ifelse(df$peptic_ulcer_comor == 1L, 1L, 0L)
df$peripheral_vascular_comor <- ifelse(df$peripheral_vascular_comor == 1L, 1L, 0L)

## total charlson for each patient 
charlson=c("cancer_comor","cardiovascular_comor","chronic_obstructive_pulmonary_comor",
           "heart_failure_comor","connective_tissue_comor", "dementia_comor",
           "diabetes_comor","diabetes_complications_comor","hemiplegia_comor",
           "hiv_comor","metastatic_cancer_comor" ,"mild_liver_comor",
           "mod_severe_liver_comor", "mod_severe_renal_comor", "mi_comor",
           "peptic_ulcer_comor" , "peripheral_vascular_comor" )

df$charlson_score=rowSums(df[charlson])

## Charlson - as a categorical group variable
df <- df %>%
  mutate(charlsonGrp = case_when(charlson_score >0 & charlson_score <=2 ~ 2,
                                charlson_score >2 & charlson_score <=4 ~ 3,
                                charlson_score >4 & charlson_score <=6 ~ 4,
                                charlson_score >=7 ~ 5,
                                charlson_score == 0 ~ 1))

df$charlsonGrp <- as.factor(df$charlsonGrp)
df$charlsonGrp <- factor(df$charlsonGrp, 
                                 labels = c("zero", "low", "medium", "high", "very high"))


df_overall <- df

## group by patient ID, then arrange by most recent delivery code date
## take most recent code per patient in period. 
df_overall2 <- df_overall %>% group_by(patient_id)%>% arrange(desc(delivery_code_date)) %>% filter(row_number()==1)

#Creates before/after pandemic dfs as well as overall
df_overall3 <- df_overall2 %>% filter(delivery_code_date <= "2023-04-30")
df_before <- df_overall2 %>% filter(delivery_code_date < "2020-03-01") 
df_after <- df_overall2 %>% filter(delivery_code_date > "2020-02-29") 

# select variables for the baseline table
# add covid, eth2, hyp, charlson
bltab_vars <- df_overall3 %>% select(patient_id, age, age_cat, bmi, bmi_cat, delivery_code_number, region, ethnicity, imd, pn8wk_code_number, postnatal_8wk_code_present, charlsonGrp, gp_covid, Covid_test_result_sgss) 
bltab_vars_before  <- df_before %>% select(patient_id, age, age_cat, bmi, bmi_cat, delivery_code_number, region, ethnicity, imd, pn8wk_code_number, postnatal_8wk_code_present, charlsonGrp, gp_covid, Covid_test_result_sgss) 
bltab_vars_after  <- df_after %>% select(patient_id, age, age_cat, bmi, bmi_cat, delivery_code_number, region, ethnicity, imd, pn8wk_code_number, postnatal_8wk_code_present, charlsonGrp, gp_covid, Covid_test_result_sgss) 

# columns for baseline table
colsfortab <- colnames(bltab_vars)
colsfortab_before <- colnames(bltab_vars_before)
colsfortab_after <- colnames(bltab_vars_after)

bltab_vars %>% summary_factorlist(explanatory = colsfortab) -> t
#str(t)
#write_csv(t, here::here("output", "blt_overall_8wk.csv"))
write_csv(t, here::here("output", "blt_overall_8wk_update.csv"))

bltab_vars_before %>% summary_factorlist(explanatory = colsfortab_before) -> t2
#str(t2)
#write_csv(t2, here::here("output", "blt_before_8wk.csv"))
write_csv(t2, here::here("output", "blt_before_8wk_update.csv"))

bltab_vars_after %>% summary_factorlist(explanatory = colsfortab_after) -> t3
#str(t3)
#write_csv(t3, here::here("output", "blt_after_8wk.csv"))
write_csv(t3, here::here("output", "blt_after_8wk_update.csv"))

## 6. Overall counts
num_pracs <- length(unique(df_overall3$practice))
num_pats <- length(unique(df_overall3$patient_id))
overall_counts <- as.data.frame(cbind(num_pats, num_pracs))
#write_csv(overall_counts, here::here("output", "overall_counts_8wk.csv"))
write_csv(overall_counts, here::here("output", "overall_counts_8wk_update.csv"))

num_pracs_before <- length(unique(df_before3$practice))
num_pats_before <- length(unique(df_before3$patient_id))
overall_counts_before <- as.data.frame(cbind(num_pats_before, num_pracs_before))
#write_csv(overall_counts_before, here::here("output", "overall_counts_before_8wk.csv"))
write_csv(overall_counts_before, here::here("output", "overall_counts_before_8wk_update.csv"))

num_pracs_after <- length(unique(df_after3$practice))
num_pats_after <- length(unique(df_after3$patient_id))
overall_counts_after <- as.data.frame(cbind(num_pats_after, num_pracs_after))
#write_csv(overall_counts_after, here::here("output", "overall_counts_after_8wk.csv"))
write_csv(overall_counts_after, here::here("output", "overall_counts_after_8wk_update.csv"))

