library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')
#library('tableone')

#setwd("C:/Users/mdehsdh7/GitHub/uom_pregnancy_tx_pathways/output/measures")

setwd(here::here("output", "measures"))

#combine all "input_measures" files 
df<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

df$delivery_code_date<-as.Date(df$delivery_code_date)

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

#Creates before/after pandemic dfs as well as overall
df_overall <- df
df_before <- df %>% filter(delivery_code_date < "2020-03-01") 
df_after <- df %>% filter(delivery_code_date > "2020-02-29") 

## 2. filter del codes >0
df_overall2 <- df_overall %>% filter(delivery_code_present > 0)
df_before2 <- df_before %>% filter(delivery_code_present > 0)
df_after2 <- df_after %>% filter(delivery_code_present > 0)

## 3. group by patient ID
df_overall3 <- df_overall2 %>% group_by(patient_id)
df_before3 <- df_before2 %>% group_by(patient_id)
df_after3 <- df_after2 %>% group_by(patient_id)

## 4. arrange by patient ID then del code date
# then filter by first row to get last date in study period

df_overall4 <- df_overall3 %>% arrange(patient_id, desc(delivery_code_date)) %>% filter(row_number()==1)
df_before4 <- df_before3 %>% arrange(patient_id, desc(delivery_code_date)) %>% filter(row_number()==1)
df_after4 <- df_after3 %>% arrange(patient_id, desc(delivery_code_date)) %>% filter(row_number()==1)

# df4<-df3%>% group_by(patient_id) %>% 
#   arrange(desc(delivery_code_date), group_by=TRUE) %>%
#   filter(row_number()==1)

## 5. Create summary table

# select variables for the baseline table
bltab_vars <- df_overall4 %>% select(age, age_cat, bmi, bmi_cat, delivery_code_number, ethnicity_6, imd) 
bltab_vars_before <- df_before4 %>% select(age, age_cat, bmi, bmi_cat, delivery_code_number, ethnicity_6, imd) 
bltab_vars_after <- df_after4 %>% select(age, age_cat, bmi, bmi_cat, delivery_code_number, ethnicity_6, imd) 

# columns for baseline table
colsfortab <- colnames(bltab_vars)
colsfortab_before <- colnames(bltab_vars_before)
colsfortab_after <- colnames(bltab_vars_after)

bltab_vars %>% summary_factorlist(explanatory = colsfortab) -> t
str(t)
write_csv(t, here::here("output", "blt_overall.csv"))

bltab_vars_before %>% summary_factorlist(explanatory = colsfortab_before) -> t2
str(t2)
write_csv(t2, here::here("output", "blt_before.csv"))

bltab_vars_after %>% summary_factorlist(explanatory = colsfortab_after) -> t3
str(t3)
write_csv(t3, here::here("output", "blt_after.csv"))

## 6. Overall counts

num_pracs <- length(unique(df_overall4$practice))
num_pats <- length(unique(df_overall4$patient_id))
overall_counts <- as.data.frame(cbind(num_pats, num_pracs))
write_csv(overall_counts, here::here("output", "overall_counts.csv"))


#could also use createtableone? 
