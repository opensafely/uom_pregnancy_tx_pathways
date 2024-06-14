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

df_input$charlsonGrp2 <- as.factor(df_input$charlsonGrp2)
df_input$hbp_pregnancy <- as.factor(df_input$hbp_pregnancy)


############### 
## model with Charlson Y/N, no hbp_pregnancy history
###############
#  short model  
## traditional glm()
model_bmi <- glm(postnatal_8wk_code_present ~ BMI, data = df_input, family = binomial(link = "logit"))
model_bmicat <- glm(postnatal_8wk_code_present ~ bmi_cat, data = df_input, family = binomial(link = "logit"))

# Extract coefficient estimates and exponentiate them
fit_results <- tidy(model_bmi, exponentiate = TRUE)
fit_results_cat <- tidy(model_bmicat, exponentiate = TRUE)

# Extract confidence intervals and exponentiate them
conf_intervals <- confint(model_bmi)
exp_conf_intervals <- exp(conf_intervals).

conf_intervals_cat <- confint(model_bmicat)
exp_conf_intervals_cat <- exp(conf_intervals_cat)


# Append exponentiated confidence intervals to the data frame
fit_results$exp_conf_low <- exp_conf_intervals[, 1]
fit_results$exp_conf_high <- exp_conf_intervals[, 2]
fit_results_cat$exp_conf_low <- exp_conf_intervals_cat[, 1]
fit_results_cat$exp_conf_high <- exp_conf_intervals_cat[, 2]

write_csv(fit_results, here::here("output","mod_bmi.csv"))
write_csv(fit_results_cat, here::here("output","mod_bmi_cat.csv"))

