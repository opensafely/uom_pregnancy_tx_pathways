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

df_input <- df_input %>% mutate_at(c("cancer_comor","cardiovascular_comor","chronic_obstructive_pulmonary_comor",
                                     "heart_failure_comor","connective_tissue_comor", "dementia_comor",
                                     "diabetes_comor","diabetes_complications_comor","hemiplegia_comor",
                                     "hiv_comor","metastatic_cancer_comor" ,"mild_liver_comor",
                                     "mod_severe_liver_comor", "mod_severe_renal_comor", "mi_comor",
                                     "peptic_ulcer_comor" , "peripheral_vascular_comor",
                                     "hbp_pregnancy", "hbp_all", "hbp_any"), as.factor)

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



###remove _comor from column names for plotting

for ( col in 1:ncol(df_input)){
  colnames(df_input)[col] <-  sub("_comor.*", "", colnames(df_input)[col])
}
#colnames(df_input)

# ### age & bmi adj
# variables_names <- df_input %>% dplyr::select(Age, BMI)
# explanatory_m1=colnames(variables_names)
# dependent="postnatal_8wk_code_present"
# 
# df_input %>%
#   finalfit.glm(dependent, explanatory_m1, add_dependent_label = F,
#                dependent_label_prefix= "", metrics = TRUE) -> t_m1
# t_m1.df <- as.data.frame(t_m1)
# write_csv(t_m1[[1]], here::here("output","mod1_age_bmi.csv"))
# write_csv(t_m1.df, here::here("output","mod1_age_bmi_matrix.csv"))


## select variable for fully adjusted model
###########################################
#  mod 2  ##### Charlson Gp ##################
variables_names_charlgp <- df_input %>% dplyr::select(Age, BMI, Region, Ethnicity, IMD,
                                                      charlsonGrp2)
explanatory_m2=colnames(variables_names_charlgp)
rm(variables_names_charlgp)

#  mod 3  ######## comorbidities x17 ############
variables_names_comor <- df_input %>% 
                  dplyr::select(Age, BMI, Region, Ethnicity, IMD, 
                                "cancer","cardiovascular","chronic_obstructive_pulmonary",
                                "heart_failure","connective_tissue", "dementia",
                                "diabetes","diabetes_complications","hemiplegia",
                                "hiv","metastatic_cancer" ,"mild_liver",
                                "mod_severe_liver", "mod_severe_renal", "mi",
                                "peptic_ulcer" , "peripheral_vascular")

explanatory_m3=colnames(variables_names_comor)
rm(variables_names_comor)

#  mod 4  ############ history of HBP in pregnancy ##########
variables_names_hbp_pregnancy <- df_input %>% dplyr::select(Age, BMI, Region, 
                                                            Ethnicity, IMD, hbp_pregnancy)

explanatory_m4=colnames(variables_names_hbp_pregnancy)
rm(variables_names_hbp_pregnancy)

dependent="postnatal_8wk_code_present"


## model 2 - fully adjusted with Charlson to capture comorbidities
df_input %>%
  finalfit.glm(dependent, explanatory_m2, add_dependent_label = F,
           dependent_label_prefix= "", metrics = TRUE) -> t_m2
t_m2.df <- as.data.frame(t_m2)
write_csv(t_m2[[1]], here::here("output","mod2_fulladj.csv"))
write_csv(t_m2.df, here::here("output","mod2_fulladj_matrix.csv"))
t_m2.df_adj <- t_m2.df[,-c(3:5)]
write_csv(t_m2.df_adj, here::here("output","mod2_fulladj_matrix_reduced_Charlson.csv"))

#plot
mod2_plot<- df_input %>% 
                or_plot(dependent, explanatory_m2, 
                        column_space = c(-0.5, -0.1, 0.5),
                        add_dependent_label = F,
                        width = 10, height = 7,
                        dependent_label = "No PN Check",
                        table_text_size = 3)

ggsave(
  plot= mod2_plot, width = 11.5, height = 8,
  filename="mod2_plot_charlson.jpeg", path=here::here("output"), dpi=300
)
rm(t_m2, t_m2.df, t_m2.df_adj, mod2_plot)


## model 3 - fully adjusted with x17 comorbidities
df_input %>%
  finalfit.glm(dependent, explanatory_m3, add_dependent_label = F,
               dependent_label_prefix= "", metrics = TRUE) -> t_m3
t_m3.df <- as.data.frame(t_m3)
write_csv(t_m3[[1]], here::here("output","mod3_fulladj.csv"))
write_csv(t_m3.df, here::here("output","mod3_fulladj_matrix.csv"))
t_m3.df_adj <- t_m3.df[,-c(3:5)]
write_csv(t_m3.df_adj, here::here("output","mod3_fulladj_matrix_reduced_comorbids.csv"))


mod3_plot<- df_input %>% 
  or_plot(dependent, explanatory_m3, 
          column_space = c(-0.5, -0.1, 0.5),
          add_dependent_label = F,
          width = 10, height = 7,
          dependent_label = "No PN Check",
          table_text_size = 3,
          remove_ref = TRUE)

ggsave(
  plot= mod3_plot, width = 11.5, height = 8,
  filename="mod3_plot_comor.jpeg", path=here::here("output"), dpi=300
)
rm(t_m3, t_m3.df, t_m3.df_adj, mod3_plot)


## model 4 - fully adjusted with History of HBP in pregnancy (current and 5yrs+)
df_input %>%
  finalfit.glm(dependent, explanatory_m4, add_dependent_label = F,
               dependent_label_prefix= "", metrics = TRUE) -> t_m4
t_m4.df <- as.data.frame(t_m4)
write_csv(t_m4[[1]], here::here("output","mod4_fulladj.csv"))
write_csv(t_m4.df, here::here("output","mod4_fulladj_matrix.csv"))
t_m4.df_adj <- t_m4.df[,-c(3:5)]
write_csv(t_m4.df_adj, here::here("output","mod4_fulladj_matrix_reduced_hbp_pregnancy.csv"))


mod4_plot<- df_input %>% 
  or_plot(dependent, explanatory_m4, 
          column_space = c(-0.5, -0.1, 0.5),
          add_dependent_label = F,
          width = 10, height = 7,
          dependent_label = "No PN Check",
          table_text_size = 3)

ggsave(
  plot= mod4_plot, width = 11.5, height = 8,
  filename="mod4_plot_hpb.jpeg", path=here::here("output"), dpi=300
)
rm(t_m4, t_m4.df, t_m4.df_adj, mod4_plot)

############### 
## model with Charlson Y/N and hbp_pregnancy
###############
#  mod 2+4  
variables_names_charlgp_hbppreg <- df_input %>% dplyr::select(Age, BMI, Region, Ethnicity, IMD,
                                                      charlsonGrp2, hbp_pregnancy)
explanatory_m2.4=colnames(variables_names_charlgp_hbppreg)
rm(variables_names_charlgp_hbppreg)

df_input %>%
  finalfit.glm(dependent, explanatory_m2.4, add_dependent_label = F,
               dependent_label_prefix= "", metrics = TRUE) -> t_m2.4
t_m2.4.df <- as.data.frame(t_m2.4)
write_csv(t_m2.4[[1]], here::here("output","mod2.4_fulladj.csv"))
write_csv(t_m2.4.df, here::here("output","mod2.4_fulladj_matrix.csv"))
t_m2.4.df_adj <- t_m2.4.df[,-c(3:5)]
write_csv(t_m2.4.df_adj, here::here("output","mod2.4_fulladj_matrix_reduced_Charlson_hbp_pregnancy.csv"))


mod4.2_plot<- df_input %>% 
  or_plot(dependent, explanatory_m2.4, 
          column_space = c(-0.5, -0.1, 0.5),
          add_dependent_label = F,
          width = 10, height = 7,
          dependent_label = "No PN Check",
          table_text_size = 3)

ggsave(
  plot= mod4.2_plot, width = 11.5, height = 8,
  filename="mod2.4_plot_hpb.jpeg", path=here::here("output"), dpi=300
)
rm(t_m4, t_m4.df, t_m4.df_adj, mod4_plot)



# ##### sensitivityanalysis - see other action covid
 
# ### covid time
# 
# ### dummy data dates not working (only 2019 generated)
# ## recreate covid variable to include nas as a level - na wont be introduced on real data 
# #str(df_input$covid)
# df_input <- df_input %>% dplyr::mutate(covid2 = case_when(is.na(covid) ~ 9,
#                                                            covid == 0~ 0,
#                                                          covid ==1 ~1))
# df_input$covid2 <- as.factor(df_input$covid2)
# 
# 
# ## traditional glm()
# model_covid <- glm(postnatal_8wk_code_present ~ (Age+BMI+Region+Ethnicity+IMD+Charlson_Gp) * covid2, data = df_input, family = binomial(link = "logit"))
# library('broom')
# 
# # Extract coefficient estimates and exponentiate them
# fit_covid_results <- tidy(model_covid, exponentiate = TRUE)
# # Extract confidence intervals and exponentiate them
# conf_intervals <- confint(model_covid)
# exp_conf_intervals <- exp(conf_intervals)
# # Append exponentiated confidence intervals to the data frame
# fit_covid_results$exp_conf_low <- exp_conf_intervals[, 1]
# fit_covid_results$exp_conf_high <- exp_conf_intervals[, 2]
# 
# write_csv(fit_covid_results, here::here("output","mod7_fulladj_traditional_reduced_covid.csv"))
# 
# ## finalfit() glm
# explanatory_m2_covid=c("Age*covid2","BMI*covid2","Region*covid2" , "Ethnicity*covid2" ,"IMD*covid2", "Charlson_Gp*covid2"  )

# df_input %>%
#   finalfit.glm(dependent, explanatory_m2_covid, add_dependent_label = F,
#                dependent_label_prefix= "", metrics = TRUE) -> t_m7
# t_m7.df <- as.data.frame(t_m7)
# #write_csv(t_m7[[1]], here::here("output","mod7_fulladj.csv"))
# #write_csv(t_m7.df, here::here("output","mod7_fulladj_matrix.csv"))
# t_m7.df_adj <- t_m7.df[,-c(3:5)]
# write_csv(t_m7.df_adj, here::here("output","mod7_finalfit_covid.csv"))



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
# #mod2df.exp<- mod2df.exp[c(-1),]
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

