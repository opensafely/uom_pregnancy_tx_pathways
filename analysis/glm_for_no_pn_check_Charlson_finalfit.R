library('tidyverse')
library('lubridate')
library('dplyr')
library('finalfit')

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
#  mod full 
## finalfit glm()
variables_names_charlgp <- df_input %>% dplyr::select(Age, BMI, Region, Ethnicity, IMD,
                                                      charlsonGrp2)
explanatory_vars=colnames(variables_names_charlgp)
dependent="postnatal_8wk_code_present"


df_input %>%
  finalfit.glm(dependent, explanatory_vars, add_dependent_label = F,
               dependent_label_prefix= "", metrics = TRUE) -> modCharl
modCharl.df <- as.data.frame(modCharl)
modCharl.dfr  <- modCharl.df [,-c(3:5)]
write_csv(modCharl.dfr, here::here("output","mod_full_Charlson_finalfit.csv"))
