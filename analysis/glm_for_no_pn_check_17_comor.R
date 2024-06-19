library('tidyverse')
library('lubridate')
library('dplyr')
library('broom')

setwd(here::here("output", "joined_8wk"))

df <-read_rds('cohort_selection_one_random_obvs_8wk.rds')


## make covid variable
## define dates
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))
df_input=df%>%mutate(covid=cut(delivery_code_date,breaks,labels = c("0","1"),include.lowest = TRUE))
df_input$covid <- as.factor(df_input$covid)
rm(df)

## dependent variable is if they had a postnatal check or not. 
df_input$postnatal_8wk_code_present <- as.factor(df_input$postnatal_8wk_code_present)
## glm is risk of no pn check, ie risk of == 0
## relevel so thoes with a pn check are the reference
df_input$postnatal_8wk_code_present <- relevel(df_input$postnatal_8wk_code_present, "1")

# df_input$age_cat<-as.factor(df_input$age_cat)
# df_input$age_cat <- relevel(df_input$age_cat, "25-29")

df_input$Ethnicity <- as.factor(df_input$Ethnicity)
df_input$Ethnicity <- relevel(df_input$Ethnicity, "White") #white as reference

df_input$region <- factor(df_input$region, levels = c("East","East Midlands",
                                                      "London", "North East", "North West",
                                                      "South East","South West","West Midlands",
                                                      "Yorkshire and The Humber"))
df_input$region <- relevel(df_input$region, "London")


df_input$imd<- factor(df_input$imd, levels= c("0","1", "2", "3", "4","5"))
df_input$imd <- relevel(df_input$imd, "5")# least deprived as reference

# bmi - numeric
df_input$bmi <- as.numeric(df_input$bmi)
# df_input$bmi_cat <- as.factor(df_input$bmi_cat)

df_input<-ungroup(df_input)

df_input <- df_input %>% filter(Ethnicity != "Unknown")
df_input$Ethnicity<-as.factor(df_input$Ethnicity)
df_input$Ethnicity<-droplevels(df_input$Ethnicity)

df_input <- df_input %>% filter(imd != "0")
df_input$imd<- factor(df_input$imd, levels= c("5", "0","1", "2", "3", "4"))
df_input$imd <- relevel(df_input$imd, "5")# least deprived as reference
df_input$imd<-droplevels(df_input$imd)

df_input$hbp_pregnancy <- as.factor(df_input$hbp_pregnancy)

###remove _comor from column names for plotting
for ( col in 1:ncol(df_input)){
  colnames(df_input)[col] <-  sub("_comor.*", "", colnames(df_input)[col])
}

df_input <- df_input %>% mutate_at(c("cancer","cardiovascular","chronic_obstructive_pulmonary",
                                     "heart_failure","connective_tissue", "dementia",
                                     "diabetes","diabetes_complications","hemiplegia",
                                     "hiv","metastatic_cancer" ,"mild_liver",
                                     "mod_severe_liver", "mod_severe_renal", "mi",
                                     "peptic_ulcer" , "peripheral_vascular",
                                     "hbp_pregnancy", "hbp_all", "hbp_any"), as.factor)



############### 
## model with comorbidities x17 
############
## traditional glm()
model_full_17comor <- glm(postnatal_8wk_code_present ~ age+bmi+region+Ethnicity+imd+
                            cancer+cardiovascular+chronic_obstructive_pulmonary+
                            heart_failure+connective_tissue+ dementia+
                            diabetes+diabetes_complications+hemiplegia+
                            hiv+metastatic_cancer +mild_liver+
                            mod_severe_liver+ mod_severe_renal+ mi+
                            peptic_ulcer+ peripheral_vascular, 
                            data = df_input, family = binomial(link = "logit"))

# Extract coefficient estimates and exponentiate them
fit_results_17comor <- tidy(model_full_17comor, exponentiate = TRUE)
# Extract confidence intervals and exponentiate them
conf_intervals_17comor <- confint(model_full_17comor)
exp_conf_intervals_17comor <- exp(conf_intervals_17comor)
# Append exponentiated confidence intervals to the data frame
fit_results_17comor$exp_conf_low <- exp_conf_intervals_17comor[, 1]
fit_results_17comor$exp_conf_high <- exp_conf_intervals_17comor[, 2]

write_csv(fit_results_17comor, here::here("output","mod_full_17_comor.csv"))


############### 
## model with comorbidities x17 + HBP
############
## traditional glm()
model_full_17comor_HBP <- glm(postnatal_8wk_code_present ~ age+bmi+region+Ethnicity+imd+
                                cancer+cardiovascular+chronic_obstructive_pulmonary+
                                heart_failure+connective_tissue+ dementia+
                                diabetes+diabetes_complications+hemiplegia+
                                hiv+metastatic_cancer +mild_liver+
                                mod_severe_liver+ mod_severe_renal+ mi+
                                peptic_ulcer+ peripheral_vascular+hbp_pregnancy,
                                data = df_input, family = binomial(link = "logit"))

# Extract coefficient estimates and exponentiate them
fit_results_17comor_HBP <- tidy(model_full_17comor_HBP, exponentiate = TRUE)
# Extract confidence intervals and exponentiate them
conf_intervals_17comor_HBP <- confint(model_full_17comor_HBP)
exp_conf_intervals_17comor_HBP <- exp(conf_intervals_17comor_HBP)
# Append exponentiated confidence intervals to the data frame
fit_results_17comor_HBP$exp_conf_low <- exp_conf_intervals_17comor_HBP[, 1]
fit_results_17comor_HBP$exp_conf_high <- exp_conf_intervals_17comor_HBP[, 2]

write_csv(fit_results_17comor_HBP, here::here("output","mod_full_17_comor_AND_HBP.csv"))

