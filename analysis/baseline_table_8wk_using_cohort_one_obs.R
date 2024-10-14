library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')

setwd(here::here("output", "joined_8wk"))


df <- read_rds('cohort_selection_one_random_obvs_8wk.rds')

## count overall practices and patients:
num_pracs <- round(length(unique(df$practice))/5)*5
num_pats <- round(length(unique(df$patient_id))/5)*5
overall_counts <- as.data.frame(cbind(num_pats, num_pracs))
overall_counts$cohort<- "8 weeks"
overall_counts$withPN <- round(sum(df$postnatal_8wk_code_present)/5)*5
write_csv(overall_counts, here::here("output", "overall_counts_8wk_cohort_one_obs.csv"))
rm(num_pracs, num_pats, overall_counts)

df$bmi_cat <- as.factor(df$bmi_cat)
df$imd <- as.factor(df$imd)
df$region <- as.factor(df$region)
df$Ethnicity <- as.factor(df$Ethnicity)
df$covid_positive<-as.factor(df$covid_positive)
df$charlsonGrp <- as.factor(df$charlsonGrp)
df$charlsonGrp2 <- factor(df$charlsonGrp2)

df$bmi <- as.numeric(df$bmi)


# select CONTINUOUS variables for the baseline table
bltab_vars_contin <- df %>% select(patient_id, age, bmi, sum_delivery_codes, 
                            sum_pn_codes) 
names_contin <- colnames(bltab_vars_contin)

# select FACTOR variables for the baseline table
bltab_vars_cat <- df %>% select(patient_id, age_cat, bmi_cat, region, Ethnicity, imd,
                                charlsonGrp, charlsonGrp2, covid_positive, hbp_any,hbp_pregnancy,
                            "cancer_comor","cardiovascular_comor","chronic_obstructive_pulmonary_comor",
                            "heart_failure_comor","connective_tissue_comor", "dementia_comor",
                            "diabetes_comor","diabetes_complications_comor","hemiplegia_comor",
                            "hiv_comor","metastatic_cancer_comor" ,"mild_liver_comor",
                            "mod_severe_liver_comor", "mod_severe_renal_comor", "mi_comor",
                            "peptic_ulcer_comor" , "peripheral_vascular_comor" ) 
names_cat<- colnames(bltab_vars_cat)

df$postnatal_8wk_code_present<-as.factor(df$postnatal_8wk_code_present)
dependent="postnatal_8wk_code_present"

## continuos table:
df %>% summary_factorlist(dependent, explanatory = names_contin) -> t
t<-(t[-1,])
write_csv(t, here::here("output", "blt_overall_8wk_cohort_one_obs_contin.csv"))

## categorical table:
df %>% summary_factorlist(dependent, explanatory = names_cat, total_col = T) -> t2
t2<-(t2[-1,])

## round to 5
## split cols out
# Function to round counts to the nearest 5 and recalculate percentages
round_counts <- function(t2) {
  # Extract counts and percentages
  counts_no_check <- as.numeric(sub("\\s*\\(.*", "", t2$`0`))
  counts_check <- as.numeric(sub("\\s*\\(.*", "", t2$`1`))
  counts_total <- as.numeric(sub("\\s*\\(.*", "", t2$Total))
  
  # Round counts to the nearest 5
  counts_no_check <- round(counts_no_check / 5) * 5
  counts_check <- round(counts_check / 5) * 5
  counts_total <- round(counts_total / 5) * 5

  # Update the columns with rounded counts 
  t2$`0`<- paste(counts_no_check)
  t2$`1`<- paste(counts_check)
  t2$Total <- paste(counts_total)
  
  return(t2)
}

# Apply the function to the data
data <- round_counts(t2)
#redact any counts <7 
data2 <- data %>%
  mutate(across(3:5, ~ if_else(as.numeric(.) <= 7, "redacted", as.character(.))))


write_csv(t2, here::here("output", "blt_overall_8wk_cohort_one_obs_cat.csv"))
write_csv(data2, here::here("output", "blt_overall_8wk_cohort_one_obs_cat_rounded.csv"))