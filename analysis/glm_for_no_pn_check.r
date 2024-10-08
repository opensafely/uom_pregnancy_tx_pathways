library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')

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


## dependent variable is if they had a postnatal check or not. 
df_input$postnatal_8wk_code_present <- as.factor(df_input$postnatal_8wk_code_present)
## glm is risk of no pn check, ie risk of == 0
## relevel so thoes with a pn check are the reference
df_input$postnatal_8wk_code_present <- relevel(df_input$postnatal_8wk_code_present, "1")

df_input$age_cat<-as.factor(df_input$age_cat)
df_input$age_cat <- relevel(df_input$age_cat, "25-29")

## simple model 

# select variables 
explanatory_m1= "age_cat"
dependent="postnatal_8wk_code_present"
df_input %>%
  finalfit(dependent, explanatory_m1, 
           dependent_label_prefix= "", metrics = TRUE) -> t_m1
t_m1.df <- as.data.frame(t_m1)
write_csv(t_m1[[1]], here::here("output","mod1_ageadj.csv"))
write_csv(t_m1.df, here::here("output","mod1_ageadj_matrix.csv"))

## Adjusted model
df_input$ethnicity2<-as.factor(df_input$ethnicity2)
df_input <- df_input %>% 
                dplyr::mutate(Ethnicity = case_when(is.na(ethnicity2) ~ "Unknown",
                                                    ethnicity2 == "1" ~ "White",
                                                    ethnicity2 == "2"  ~ "Mixed",
                                                    ethnicity2 == "3"  ~ "Asian or Asian British",
                                                    ethnicity2 == "4"  ~ "Black or Black British",
                                                    ethnicity2 == "5"  ~ "Chinese or Other Ethnic Groups",
                                                    ethnicity2 == "0"  ~ "Unknown"))
df_input$Ethnicity <- as.factor(df_input$Ethnicity)
df_input$Ethnicity <- relevel(df_input$Ethnicity, "White") #white as reference

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

#### high risk pregnancy
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
  dplyr::mutate(Charlson_Gp = case_when(charlson_score >0 & charlson_score <=2 ~ 2,
                                        charlson_score >2 & charlson_score <=4 ~ 3,
                                        charlson_score >4 & charlson_score <=6 ~ 4,
                                        charlson_score >=7 ~ 5,
                                        charlson_score == 0 ~ 1))

df_input$Charlson_Gp <- as.factor(df_input$Charlson_Gp)
df_input$Charlson_Gp <- factor(df_input$Charlson_Gp,
                               labels = c("zero", "low", "medium", "high", "very high"))

## covid positive
df_input <- df_input %>% mutate(covid_positive = case_when(gp_covid == 1 ~ "1",
                                                 Covid_test_result_sgss ==1 ~ "1",
                                                 TRUE ~ "0"))
df_input$covid_positive<-as.factor(df_input$covid_positive)


# select variables for modelling
colnames(df_input)[3]<-"Age"
colnames(df_input)[7]<-"Region"
colnames(df_input)[8]<-"IMD"
colnames(df_input)[9]<-"BMI"
colnames(df_input)[40]<-"HBP"

df_input <- df_input %>% filter(Ethnicity != "Unknown")
df_input$Ethnicity<-droplevels(df_input$Ethnicity)
variables_names <- df_input %>% dplyr::select(Age, BMI)#, Region, Ethnicity, IMD, 
                                              #Charlson_Gp, covid_positive, HBP)

explanatory_m2=colnames(variables_names)
dependent="postnatal_8wk_code_present"

df_input %>%
  finalfit(dependent, explanatory_m2, 
           dependent_label_prefix= "", metrics = TRUE) -> t_m2
t_m2.df <- as.data.frame(t_m2)
write_csv(t_m2[[1]], here::here("output","mod2_fulladj.csv"))
write_csv(t_m2.df, here::here("output","mod2_fulladj_matrix.csv"))
t_m2.df_adj <- t_m2.df[,-c(3:5)]
write_csv(t_m2.df_adj, here::here("output","mod2_fulladj_matrix_reduced.csv"))


mod2_plot<- df_input %>% 
                or_plot(dependent, explanatory_m2)#, 
                    #breaks = c(0.5, 1, 5, 10, 20, 30))

df_input %>% 
  ff_glimpse(dependent, explanatory_m2)


ggsave(
  plot= mod2_plot, 
  filename="mod2_plot.jpeg", path=here::here("output"), dpi=300
)


## 
# Load the necessary library
library(stringr)

# Function to extract values from the string
extract_values <- function(row) {
  values <- str_extract(row, "\\d+\\.?\\d*")  # Extract numeric values
  values <- as.numeric(values)  # Convert to numeric
  return(values)
}

# Apply the function to create a matrix of values
values_matrix <- t(sapply(t_m2.df_adj$OR..multivariable., extract_values))

# Convert the matrix to a data frame
extracted_values_df <- as.data.frame(values_matrix)

# Rename the columns
colnames(extracted_values_df) <- c("OR", "CI_lower", "CI_upper")

# Append the extracted values to the original data frame
t_m2.df_adj2 <- cbind(t_m2.df_adj, extracted_values_df)

write_csv(t_m2.df_adj2, here::here("output","mod2_fulladj_matrix_reduced_2.csv"))




# #covid
# # mod2_covid <- glm(postnatal_8wk_code_present~age_cat+
# #                 ethnicity2+region+imd+bmi_cat+charlsonGrp+covid, family=binomial(link=logit), data=df_input)
# # mod2covid_df <- cbind(Estimate = coef(mod2_covid), confint(mod2_covid))
# # mod2covid_df.exp=round(exp(mod2covid_df), digits = 2)
# # pval2<- coef(summary(mod2_covid))[,4]
# # mod2covid_df.exp<-cbind(mod2covid_df.exp, pval2)
# # mod2covid_df.exp<- as.data.frame(mod2covid_df.exp)
# # write_csv(mod2covid_df.exp, here::here("output","mod2_covid_fulladj_coef.csv"))
# 
# 
# ### model on obs per patient ID
# ## take one row per patient
# dfone_lats <- df_input %>% group_by(patient_id) %>%
#   arrange(desc(delivery_code_date))%>%
#   filter(row_number()==1)
# 
# # dfone_first <- df_input %>% group_by(patient_id) %>%
# #   arrange((delivery_code_date))%>%
#   # filter(row_number()==1)
# 
# ## sensitivity modelling on obs per person
# # mod3first <- glm(postnatal_8wk_code_present~age_cat+
# #               ethnicity+region+imd+bmi, family=binomial(link=logit), data=dfone_first)
# mod3last <- glm(postnatal_8wk_code_present~age_cat+
#                    ethnicity2+region+imd+bmi_cat+charlsonGrp, family=binomial(link=logit), data=dfone_lats)
# 
# mod3last_df <- cbind(Estimate = coef(mod3last), confint(mod3last))
# mod3last_df.exp=round(exp(mod3last_df), digits = 2)
# pval3last<- coef(summary(mod3last))[,4]
# mod3last_df.exp<-cbind(mod3last_df.exp, pval3last)
# mod3last_df.exp<- as.data.frame(mod3last_df.exp)
# write_csv(mod3last_df.exp, here::here("output","mod3_fulladj_last.csv"))
# 
# 
# ## plot
# #library(forestplot)
# library(ggplot2)
# # remove intercept
# mod2df.exp<- mod2df.exp[c(-1),]
# colnames(mod2df.exp)[2]<-"LOW"
# colnames(mod2df.exp)[3]<-"HIGH"
# mod2df.exp$predictors<-as.factor(row.names(mod2df.exp))
# # Define the predictor groups
# # Create a new column indicating overarching predictor names
# mod2df.exp$predictor_gp <- ifelse(grepl("^age", mod2df.exp$predictors), "Age",
#                                   ifelse(grepl("^ethnicity", mod2df.exp$predictors), "Ethnicity",
#                                          ifelse(grepl("^region", mod2df.exp$predictors), "Region",
#                                                 ifelse(grepl("^imd", mod2df.exp$predictors), "IMD",
#                                                        ifelse(grepl("^bmi", mod2df.exp$predictors), "BMI",
#                                                               ifelse(grepl("^charlsonGrp", mod2df.exp$predictors), "Charlson", NA))))))
# 
# # Convert predictor_gp to factor to maintain the order
# mod2df.exp$predictor_gp <- factor(mod2df.exp$predictor_gp, levels = c("Age", "Ethnicity", "Region", "IMD", "BMI", "Charlson"))
# #table(mod2df.exp$predictor_gp)
# 
# # Create the forest plot
# plot2 <- ggplot(mod2df.exp, aes(x = Estimate, y = predictors)) +
#   geom_point(size = 2, position = position_dodge(width = 0.5)) +
#   geom_errorbarh(aes(xmin = LOW, xmax = HIGH), height = 0.2, position = position_dodge(width = 0.5)) +
#   labs(title = "Adjusted Odds Ratios (ORs)",
#        x = "OR and 95% CI", y = "") +
#   geom_vline(xintercept = 1, lty = 2) +
#   facet_wrap(~ predictor_gp, scales = "free_y", ncol = 1) +
#   theme(strip.background = element_blank(),
#         strip.text = element_blank())
# 
# ggsave(
#   plot= plot2,
#   filename="glm_fullAdjs.jpeg", path=here::here("output"), dpi = 300
# )
# 
# 
# # #           plot_mod2,
# # #           base_width = 9, base_height = 10
# # #           )
# # 
# # # save_forest(plot= plot_mod2,
# # #   filename="glm_fullAdjs.jpeg", path=here::here("output"),
# # # )
# # #
# # 
# # ##compare with glmer() repeated delcodes per mum..
# # ##or select one row per mum as sensitivity.

