library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')

setwd(here::here("output", "joined_12wk"))

df <-read_rds('cohort_selection_one_random_obvs_12wk.rds')


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

df_input <- df_input %>% filter(Ethnicity != "Unknown")
df_input$Ethnicity<-droplevels(df_input$Ethnicity)
df_input <- df_input %>% filter(IMD != "0")
df_input$IMD<-droplevels(df_input$IMD)



############### 
## model with Charlson Y/N and hbp_pregnancy
###############
#  mod 2+4  
variables_names_charlgp_hbppreg <- df_input %>% dplyr::select(Age, BMI, Region, Ethnicity, IMD,
                                                      charlsonGrp2, hbp_pregnancy)
explanatory_m2.4=colnames(variables_names_charlgp_hbppreg)
dependent="postnatal_8wk_code_present"


rm(variables_names_charlgp_hbppreg)


#12 week cohort model 
df_input %>%
  finalfit.glm(dependent, explanatory_m2.4, add_dependent_label = F,
               dependent_label_prefix= "", metrics = TRUE) -> t_m8
t_m8.df <- as.data.frame(t_m8)
t_m8.df_adj <- t_m8.df[,-c(3:5)]
write_csv(t_m8.df_adj, here::here("output","mod8_fulladj_matrix_reduced_12weekcohort.csv"))

