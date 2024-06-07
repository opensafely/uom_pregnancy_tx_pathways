library('tidyverse')
library('lubridate')
library('dplyr')
library('broom')
library('detectseparation')
setwd(here::here("output", "joined_8wk"))

df_input <-read_rds('cohort_selection_one_random_obvs_8wk.rds')


## dependent variable is if they had a postnatal check or not. 
df_input$postnatal_8wk_code_present <- as.factor(df_input$postnatal_8wk_code_present)
## glm is risk of no pn check, ie risk of == 0
## relevel so thoes with a pn check are the reference
df_input$postnatal_8wk_code_present <- relevel(df_input$postnatal_8wk_code_present, "1")

df_input$age_cat<-as.factor(df_input$age_cat)
df_input$age_cat <- relevel(df_input$age_cat, "25-29")

df_input$Ethnicity <- as.factor(df_input$Ethnicity)
df_input$Ethnicity <- relevel(df_input$Ethnicity, "White") #white as reference

df_input$region<-as.factor(df_input$region)
df_input$region <- relevel(df_input$region, "London")
df_input$imd<-as.factor(df_input$imd)
df_input$imd <- relevel(df_input$imd, "5")# least deprived as reference
 
# bmi - numeric
df_input$bmi <- as.numeric(df_input$bmi)
df_input$bmi_cat <- as.factor(df_input$bmi_cat)

df_input<-ungroup(df_input)


# select variables for modelling
colnames(df_input)[3]<-"Age"
colnames(df_input)[7]<-"Region"
colnames(df_input)[8]<-"IMD"
colnames(df_input)[9]<-"BMI"
#colnames(df_input)[40]<-"HBP"


df_input <- df_input %>% filter(Ethnicity != "Unknown")
df_input$Ethnicity<-as.factor(df_input$Ethnicity)
df_input$Ethnicity<-droplevels(df_input$Ethnicity)
df_input <- df_input %>% filter(IMD != "0")
df_input$IMD<-as.factor(df_input$IMD)
df_input$IMD<-droplevels(df_input$IMD)

df_input$hbp_pregnancy <- as.factor(df_input$hbp_pregnancy)

############### 
## model with Charlson Y/N, no hbp_pregnancy history
### take data from 2019 only
### take data from 2022 onwards
df19<- df_input %>% filter(delivery_code_date < as.Date("2020-01-01"))
df22<- df_input %>% filter(delivery_code_date > as.Date("2021-12-31"))

rm(df_input)

#2019
model_full_hbp_2019 <- glm(postnatal_8wk_code_present ~ Age+BMI+Region+Ethnicity+IMD+charlsonGrp2+hbp_pregnancy, data = df19, family = binomial(link = "logit"))
# Extract coefficient estimates and exponentiate them
fit_results_2019 <- tidy(model_full_hbp_2019, exponentiate = TRUE)
# Extract confidence intervals and exponentiate them
conf_intervals_2019 <- confint(model_full_hbp_2019)
exp_conf_intervals_2019 <- exp(conf_intervals_2019)
# Append exponentiated confidence intervals to the data frame
fit_results_2019$exp_conf_low <- exp_conf_intervals_2019[, 1]
fit_results_2019$exp_conf_high <- exp_conf_intervals_2019[, 2]
write_csv(fit_results_2019, here::here("output","mod_full_Charlson_AND_hbp_2019.csv"))


detectsep_model19 <- glm(postnatal_8wk_code_present ~ Age+BMI+Region+Ethnicity+IMD+charlsonGrp2+hbp_pregnancy, data = df19, family = binomial(link = "logit"), method = "detect_separation")
separation_result19 <- capture.output(detectsep_model19)
writeLines(separation_result19, here::here("output","separation_result19.txt"))



#2022 onwards
model_full_hbp_2022 <- glm(postnatal_8wk_code_present ~ Age+BMI+Region+Ethnicity+IMD+charlsonGrp2+hbp_pregnancy, data = df22, family = binomial(link = "logit"))
# Extract coefficient estimates and exponentiate them
fit_results_2022 <- tidy(model_full_hbp_2022, exponentiate = TRUE)
# Extract confidence intervals and exponentiate them
conf_intervals_2022 <- confint(model_full_hbp_2022)
exp_conf_intervals_2022 <- exp(conf_intervals_2022)
# Append exponentiated confidence intervals to the data frame
fit_results_2022$exp_conf_low <- exp_conf_intervals_2022[, 1]
fit_results_2022$exp_conf_high <- exp_conf_intervals_2022[, 2]
write_csv(fit_results_2022, here::here("output","mod_full_Charlson_AND_hbp_2022.csv"))

detectsep_model22 <- glm(postnatal_8wk_code_present ~ Age+BMI+Region+Ethnicity+IMD+charlsonGrp2+hbp_pregnancy, data = df22, family = binomial(link = "logit"), method = "detect_separation")
separation_result22 <- capture.output(detectsep_model22)
writeLines(separation_result22, here::here("output","separation_result22.txt"))
