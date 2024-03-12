library('tidyverse')
library('lubridate')
library('dplyr')


setwd(here::here("output", "joined_8wk"))

#2019
df19 <- read_rds('basic_joined_8wk_records_2019.rds')
## filter del codes >0 (must be numeric)
df19$delivery_code_present <- as.numeric(df19$delivery_code_present)
df19 <- df19 %>% dplyr::filter(delivery_code_present > 0)

#2020
df20 <- read_rds('basic_joined_8wk_records_2020.rds')
## filter del codes >0 (must be numeric)
df20$delivery_code_present <- as.numeric(df20$delivery_code_present)
df20 <- df20 %>% dplyr::filter(delivery_code_present > 0)

#2021
df21 <- read_rds('basic_joined_8wk_records_2021.rds')
## filter del codes >0 (must be numeric)
df21$delivery_code_present <- as.numeric(df21$delivery_code_present)
df21 <- df21 %>% dplyr::filter(delivery_code_present > 0)

#2022
df22 <- read_rds('basic_joined_8wk_records_2022.rds')
## filter del codes >0 (must be numeric)
df22$delivery_code_present <- as.numeric(df22$delivery_code_present)
df22 <- df22 %>% dplyr::filter(delivery_code_present > 0)

#2023
df23 <- read_rds('basic_joined_8wk_records_2023.rds')
## filter del codes >0 (must be numeric)
df23$delivery_code_present <- as.numeric(df23$delivery_code_present)
df23 <- df23 %>% dplyr::filter(delivery_code_present > 0)

df <- rbind(df19,df20,df21,df22,df23)
rm(df19,df20,df21,df22,df23)


# remove last month data - to match other plots
last.date="2023-08-31"
df=df%>% filter(date <=last.date)


## make covid variable
## define dates
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))
df_input=df%>%mutate(covid=cut(delivery_code_date,breaks,labels = c("0","1"),include.lowest = TRUE))
df_input$covid <- as.factor(df_input$covid)

# 
# # population with delivery codes
# df_input<- df_input %>% filter(delivery_code_number >0)


## dependent var is if they had a pn check or not. 
df_input$postnatal_8wk_code_present <- as.factor(df_input$postnatal_8wk_code_present)
#str(df_input$postnatal_8wk_code_present)
## glm is risk of no pn check, ie risk of == 0
## relevel to pn check as reference
df_input$postnatal_8wk_code_present <- relevel(df_input$postnatal_8wk_code_present, "1")

df_input$age_cat<-as.factor(df_input$age_cat)
df_input$age_cat <- relevel(df_input$age_cat, "25-29")


## simple model 
mod1 <- glm(postnatal_8wk_code_present~age_cat, family=binomial(link=logit), data=df_input)
#df_capture <-as.data.frame(capture.output(summary(mod1), 'output.txt'))
mod1df <- cbind(Estimate = coef(mod1), confint(mod1))
mod1df.exp=round(exp(mod1df), digits = 2)
pval<- coef(summary(mod1))[,4]
mod1df.exp<-cbind(mod1df.exp, pval)
mod1df.exp<- as.data.frame(mod1df.exp)
write_csv(mod1df.exp, here::here("output","mod1_ageadj_coef.csv"))
#write_csv(df_capture, here::here("output","mod1_ageadj_capture.csv"))


## Adjusted model 
#df_input$ethnicity<-as.factor(df_input$ethnicity)
df_input$ethnicity2<-as.factor(df_input$ethnicity2)
df_input$ethnicity2 <- relevel(df_input$ethnicity2, "1") #white as reference

### neeed to relabel ethnicity 2 and then releve to ref cat white. 
#df_input$ethnicity2 <- relevel(df_input$ethnicity, "White")

df_input$region<-as.factor(df_input$region)
df_input$region <- relevel(df_input$region, "London")
df_input$imd<-as.factor(df_input$imd) 
df_input$imd <- relevel(df_input$imd, "5")# least deprived as reference

# bmi - numeric
# remove bmi outliers - this replaces <8 or >50 with NA
df_input$bmi <- ifelse(df_input$bmi <8 | df_input$bmi>50, NA, df_input$bmi)
df_input <- df_input %>% dplyr::mutate(bmi_cat = case_when(is.na(bmi) ~ "Unknown",
                                                 bmi>=8 & bmi< 18.5 ~ "Underweight",
                                                 bmi>=18.5 & bmi<=24.9 ~ "Healthy weight",
                                                 bmi>24.9 & bmi<=29.9 ~ "Overweight",
                                                 bmi>29.9 ~"Obese"))
df_input$bmi_cat <- as.factor(df_input$bmi_cat)

#### highrisk pregnancy
## comorbidities. (Hypertension disorder before / during)
## create charlson index
df_input$cancer_comor<- ifelse(df_input$cancer_comor == 1L, 2L, 0L)
df_input$cardiovascular_comor <- ifelse(df_input$cardiovascular_comor == 1L, 1L, 0L)
df_input$chronic_obstructive_pulmonary_comor <- ifelse(df_input$chronic_obstructive_pulmonary_comor == 1L, 1L, 0)
df_input$heart_failure_comor <- ifelse(df_input$heart_failure_comor == 1L, 1L, 0L)
df_input$connective_tissue_comor <- ifelse(df_input$connective_tissue_comor == 1L, 1L, 0L)
df_input$dementia_comor <- ifelse(df_input$dementia_comor == 1L, 1L, 0L)
df_input$diabetes_comor <- ifelse(df_input$diabetes_comor == 1L, 1L, 0L)
df_input$diabetes_complications_comor <- ifelse(df_input$diabetes_complications_comor == 1L, 2L, 0L)
df_input$hemiplegia_comor <- ifelse(df_input$hemiplegia_comor == 1L, 2L, 0L)
df_input$hiv_comor <- ifelse(df_input$hiv_comor == 1L, 6L, 0L)
df_input$metastatic_cancer_comor <- ifelse(df_input$metastatic_cancer_comor == 1L, 6L, 0L)
df_input$mild_liver_comor <- ifelse(df_input$mild_liver_comor == 1L, 1L, 0L)
df_input$mod_severe_liver_comor <- ifelse(df_input$mod_severe_liver_comor == 1L, 3L, 0L)
df_input$mod_severe_renal_comor <- ifelse(df_input$mod_severe_renal_comor == 1L, 2L, 0L)
df_input$mi_comor <- ifelse(df_input$mi_comor == 1L, 1L, 0L)
df_input$peptic_ulcer_comor <- ifelse(df_input$peptic_ulcer_comor == 1L, 1L, 0L)
df_input$peripheral_vascular_comor <- ifelse(df_input$peripheral_vascular_comor == 1L, 1L, 0L)

## total charlson for each patient 
charlson=c("cancer_comor","cardiovascular_comor","chronic_obstructive_pulmonary_comor",
           "heart_failure_comor","connective_tissue_comor", "dementia_comor",
           "diabetes_comor","diabetes_complications_comor","hemiplegia_comor",
           "hiv_comor","metastatic_cancer_comor" ,"mild_liver_comor",
           "mod_severe_liver_comor", "mod_severe_renal_comor", "mi_comor",
           "peptic_ulcer_comor" , "peripheral_vascular_comor" )

df_input$charlson_score=rowSums(df_input[charlson])

## Charlson - as a categorical group variable
df_input <- df_input %>%
  dplyr::mutate(charlsonGrp = case_when(charlson_score >0 & charlson_score <=2 ~ 2,
                                        charlson_score >2 & charlson_score <=4 ~ 3,
                                        charlson_score >4 & charlson_score <=6 ~ 4,
                                        charlson_score >=7 ~ 5,
                                        charlson_score == 0 ~ 1))

df_input$charlsonGrp <- as.factor(df_input$charlsonGrp)
df_input$charlsonGrp <- factor(df_input$charlsonGrp, 
                               labels = c("zero", "low", "medium", "high", "very high"))





mod2 <- glm(postnatal_8wk_code_present~age_cat+
              ethnicity2+region+imd+bmi_cat+charlsonGrp, family=binomial(link=logit), data=df_input)
#summary(mod2)
mod2df <- cbind(Estimate = coef(mod2), confint(mod2))
mod2df.exp=round(exp(mod2df), digits = 2)
pval2<- coef(summary(mod2))[,4]
mod2df.exp<-cbind(mod2df.exp, pval2)
mod2df.exp<- as.data.frame(mod2df.exp)
write_csv(mod2df.exp, here::here("output","mod2_fulladj_coef.csv"))
#df2_capture <-as.data.frame(capture.output(summary(mod2), 'output.txt'))

#covid
mod2_covid <- glm(postnatal_8wk_code_present~age_cat+
                ethnicity2+region+imd+bmi_cat+charlsonGrp+covid, family=binomial(link=logit), data=df_input)
mod2covid_df <- cbind(Estimate = coef(mod2_covid), confint(mod2_covid))
mod2covid_df.exp=round(exp(mod2covid_df), digits = 2)
pval2<- coef(summary(mod2_covid))[,4]
mod2covid_df.exp<-cbind(mod2covid_df.exp, pval2)
mod2covid_df.exp<- as.data.frame(mod2covid_df.exp)
write_csv(mod2covid_df.exp, here::here("output","mod2_covid_fulladj_coef.csv"))


### model on obs per patient ID
## take one row per patient
dfone_lats <- df_input %>% group_by(patient_id) %>%
  arrange(desc(delivery_code_date))%>%
  filter(row_number()==1)

# dfone_first <- df_input %>% group_by(patient_id) %>%
#   arrange((delivery_code_date))%>%
  # filter(row_number()==1)

## sensitivity modelling on obs per person
# mod3first <- glm(postnatal_8wk_code_present~age_cat+
#               ethnicity+region+imd+bmi, family=binomial(link=logit), data=dfone_first)
mod3last <- glm(postnatal_8wk_code_present~age_cat+
                   ethnicity2+region+imd+bmi_cat+charlsonGrp, family=binomial(link=logit), data=dfone_lats)

mod3last_df <- cbind(Estimate = coef(mod3last), confint(mod3last))
mod3last_df.exp=round(exp(mod3last_df), digits = 2)
pval3last<- coef(summary(mod3last))[,4]
mod3last_df.exp<-cbind(mod3last_df.exp, pval3last)
mod3last_df.exp<- as.data.frame(mod3last_df.exp)
write_csv(mod3last_df.exp, here::here("output","mod3_fulladj_last.csv"))


## plot
library(forestplot)

# remove intercept
mod2df.exp<- mod2df.exp[c(-1),]
colnames(mod2df.exp)[2]<-"LOW"
colnames(mod2df.exp)[3]<-"HIGH"
# plottext2<- row.names(tidy2rd)
# print(plottext2)
blank_row <- data.frame(Estimate = "", LOW = "", HIGH = "",pval2 = "")

dfplot <- rbind(blank_row, mod2df.exp[1:6,],blank_row, blank_row, mod2df.exp[7:11,],
                blank_row, blank_row, mod2df.exp[12:19,], blank_row,blank_row, mod2df.exp[20:24,],
                blank_row, blank_row, mod2df.exp[25:27,],blank_row, blank_row, mod2df.exp[29:32,])

plottext2 <- c("Age:", "14-19","20-24","30-34","35-39","40-44","45-49",
               "","Ethnicity:",
              # "Asian or Asian British", "Black or Black British", "Mixed", "Other",
               "0", "2", "3", "4", "5",
                "","Region:",
               "East", "East Midlands", "North East", "North West", "South East",
               "South West", "West Midlands","Yorkshire and The Humber",
               "","IMD:",
              "imd:0", "imd:1", "imd:2", "imd:3", "imd:4",
               "", "BMI",
              "Obese", "Overweight", "Underweight", "", "Charlson Gp",
              "Low", "Medium", "High", "Very High")

dfplot <- dfplot %>% mutate_at(c(2:4), as.numeric)

plot_mod2<-forestplot(labeltext=plottext2,
                      mean = c(dfplot$Estimate), lower = c(dfplot$LOW), upper = c(dfplot$HIGH),
                      ci.vertices = TRUE, ci.vertices.height = 0.2,
                      col=fpColors(box= "royalblue", line="darkblue", zero="gray", hrz_lines="black"),
                      title="Odds Ratio [95% CI]",
                      txt_gp=fpTxtGp(label=gpar(cex=1.2), ticks=gpar(cex=1.1)),
                      grid=TRUE,
                      boxsize=0.3,
                      zero=1,
                      xticks=c(0.5, 0.75, 1.0,1.5,2.0), lwd.ci=1)


save_plot(here::here("output", "glm_fullAdjs.jpeg"),
          plot_mod2,
          base_width = 9, base_height = 10
          )

# save_forest(plot= plot_mod2,
#   filename="glm_fullAdjs.jpeg", path=here::here("output"),
# )
#

##compare with glmer() repeated delcodes per mum..
##or select one row per mum as sensitivity.

