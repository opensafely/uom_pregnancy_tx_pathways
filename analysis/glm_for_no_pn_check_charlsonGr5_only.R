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


df_input<-ungroup(df_input)


############### 
## model with Charlson Y/N, no hbp_pregnancy history
###############
#  short model  
## traditional glm()
model_charlsonGrp5 <- glm(postnatal_8wk_code_present ~ charlsonGrp, data = df_input, family = binomial(link = "logit"))

# Extract coefficient estimates and exponentiate them
fit_results <- tidy(model_charlsonGrp5, exponentiate = TRUE)

# Extract confidence intervals and exponentiate them
conf_intervals <- confint(model_charlsonGrp5)
exp_conf_intervals <- exp(conf_intervals)

# Append exponentiated confidence intervals to the data frame
fit_results$exp_conf_low <- exp_conf_intervals[, 1]
fit_results$exp_conf_high <- exp_conf_intervals[, 2]

write_csv(fit_results, here::here("output","mod_charlsonGr5_only.csv"))

