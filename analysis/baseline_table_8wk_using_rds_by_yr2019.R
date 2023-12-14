library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')
#library('fs')

#dir_create(here::here("output", "joined_8wk"), showWarnings = FALSE, recurse = TRUE)


setwd(here::here("output", "joined_8wk"))

#combine all "input_*" monthly files 
#df<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()
df <- read_rds('basic_joined_8wk_records_2019.rds')

tabdelcodes <- as.data.frame(table(df$delivery_code_present))
write_csv(tabdelcodes, here::here("output", "table_delcodes_8wk_update_2019.csv"))
rm(tabdelcodes)

## filter del codes >0 (must be numeric)
df$delivery_code_present <- as.numeric(df$delivery_code_present)
##test 0 and 1 or 1 and 2 after converted to numeric for filtering
tabdelcodes <- as.data.frame(table(df$delivery_code_present))
write_csv(tabdelcodes, here::here("output", "table_delcodes_8wk_update_2019_after_numeric.csv"))
rm(tabdelcodes)

df <- df %>% dplyr::filter(delivery_code_present > 1)
tabdelcodes <- as.data.frame(table(df$delivery_code_present))
write_csv(tabdelcodes, here::here("output", "table_delcodes_8wk_update_2019_after_filter.csv"))
rm(tabdelcodes)

df$delivery_code_present <- as.factor(df$delivery_code_present)

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
df$ethnicity2 <- as.factor(df$ethnicity2)
df$region <- as.factor(df$region)
df <- df %>% group_by(region) %>% filter(n() >= 40)
ungroup(df)

df <- df %>% group_by(ethnicity) %>% filter(n() >= 5)

## covid positive
df <- df %>% mutate(covid_positive = case_when(gp_covid == 1 ~ "1",
                                               Covid_test_result_sgss ==1 ~ "1",
                                               TRUE ~ "0"))
df$covid_positive<-as.factor(df$covid_positive)

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
df_overall$delivery_code_date<-as.Date(df_overall$delivery_code_date)
## group by patient ID, then arrange by most recent delivery code date
## take most recent code per patient in period. 
df_overall2 <- df_overall %>% group_by(patient_id)%>% arrange(desc(delivery_code_date)) %>% filter(row_number()==1)

## overall practices and patient count:
num_pracs <- length(unique(df_overall2$practice))
num_pats <- length(unique(df_overall2$patient_id))
overall_counts <- as.data.frame(cbind(num_pats, num_pracs))
write_csv(overall_counts, here::here("output", "overall_counts_8wk_update_2019.csv"))



#Creates before/after pandemic dfs as well as overall
# df_overall3 <- df_overall2 %>% filter(delivery_code_date <= "2023-09-30")
# df_before <- df_overall2 %>% filter(delivery_code_date < "2020-03-01") 
# df_after <- df_overall2 %>% filter(delivery_code_date > "2020-02-29") 

# select variables for the baseline table
bltab_vars <- df_overall2 %>% select(patient_id, age, age_cat, bmi, bmi_cat, delivery_code_number, region, ethnicity, ethnicity2, imd, pn8wk_code_number, postnatal_8wk_code_present, charlsonGrp, covid_positive, hbp_any,
                                     "cancer_comor","cardiovascular_comor","chronic_obstructive_pulmonary_comor",
                                     "heart_failure_comor","connective_tissue_comor", "dementia_comor",
                                     "diabetes_comor","diabetes_complications_comor","hemiplegia_comor",
                                     "hiv_comor","metastatic_cancer_comor" ,"mild_liver_comor",
                                     "mod_severe_liver_comor", "mod_severe_renal_comor", "mi_comor",
                                     "peptic_ulcer_comor" , "peripheral_vascular_comor" ) 
#bltab_vars_before  <- df_before %>% select(patient_id, practice, age, age_cat, bmi, bmi_cat, delivery_code_number, region, ethnicity, ethnicity2, imd, pn8wk_code_number, postnatal_8wk_code_present, charlsonGrp, covid_positive, hbp_any) 
#bltab_vars_after  <- df_after %>% select(patient_id, practice, age, age_cat, bmi, bmi_cat, delivery_code_number, region, ethnicity, ethnicity2, imd, pn8wk_code_number, postnatal_8wk_code_present, charlsonGrp, covid_positive, hbp_any) 

# columns for baseline table
colsfortab <- colnames(bltab_vars)
#colsfortab_before <- colnames(bltab_vars_before)
#colsfortab_after <- colnames(bltab_vars_after)

bltab_vars %>% summary_factorlist(explanatory = colsfortab) -> t
t<-(t[-1,])
#write_csv(t, here::here("output", "blt_overall_8wk.csv"))
write_csv(t, here::here("output", "blt_overall_8wk_update_2019.csv"))

#bltab_vars_before %>% summary_factorlist(explanatory = colsfortab_before) -> t2
#t2<-(t2[-1,])
#write_csv(t2, here::here("output", "blt_before_8wk.csv"))
#write_csv(t2, here::here("output", "blt_before_8wk_update_2019.csv"))

# bltab_vars_after %>% summary_factorlist(explanatory = colsfortab_after) -> t3
# t3<-(t3[-1,])
# #write_csv(t3, here::here("output", "blt_after_8wk.csv"))
# write_csv(t3, here::here("output", "blt_after_8wk_update.csv"))


