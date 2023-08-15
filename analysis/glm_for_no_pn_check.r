library('tidyverse')
library("cowplot")

setwd(here::here("output", "pn8wk"))
df_input<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

#str(df_input$delivery_code_date)
df_input$delivery_code_date<-as.Date(df_input$delivery_code_date)

## make covid variable
## define dates
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max("2023-05-01"))
df_input=df_input%>%mutate(covid=cut(delivery_code_date,breaks,labels = c("0","1"),include.lowest = TRUE))
df_input$covid <- as.factor(df_input$covid)


# population with delivery codes
df_input<- df_input %>% filter(delivery_code_number >0)


## dependent var is if they had a pn check or not. 
df_input$postnatal_8wk_code_present <- as.factor(df_input$postnatal_8wk_code_present)
df_input$age_cat<-as.factor(df_input$age_cat)


## simple model 
mod1 <- glm(postnatal_8wk_code_present~age_cat, family=binomial(link=logit), data=df_input)
#summary(mod1)
mod1df<- as.data.frame(mod1$coefficients)
#df_capture <-as.data.frame(capture.output(summary(mod1), 'output.txt'))

write_csv(mod1df, here::here("output","mod1_ageadj_coef.csv"))
#write_csv(df_capture, here::here("output","mod1_ageadj_capture.csv"))


## Adjusted model 
df_input$ethnicity<-as.factor(df_input$ethnicity)
df_input$ethnicity <- relevel(df_input$ethnicity, "White")
df_input$region<-as.factor(df_input$region)
df_input$region <- relevel(df_input$region, "London")
df_input$imd<-as.factor(df_input$imd) 
df_input$imd <- relevel(df_input$imd, "1") 


mod2 <- glm(postnatal_8wk_code_present~age_cat+
              ethnicity+region+imd+bmi, family=binomial(link=logit), data=df_input)
#summary(mod2)
mod2df<- as.data.frame(mod2$coefficients)
#df2_capture <-as.data.frame(capture.output(summary(mod2), 'output.txt'))

write_csv(mod2df, here::here("output","mod2_fulladj_coef.csv"))
#write_csv(df2_capture, here::here("output","mod2_fulladj_capture.csv"))


mod2_covid <- glm(postnatal_8wk_code_present~age_cat+
                ethnicity+region+imd+bmi+covid, family=binomial(link=logit), data=df_input)
mod2_coviddf<- as.data.frame(mod2_covid$coefficients)
write_csv(mod2_coviddf, here::here("output","mod2_covid_fulladj_coef.csv"))


# get coefs and CIs and plot
library(broom)
tidy1 <- as.data.frame(mod1$coefficients)
tidy2 <- as.data.frame(mod2$coefficients)
tidy2.covid <- as.data.frame(mod2_covid$coefficients)
#head(tidy2, 30)
#View(tidy2)

# Age adjusted model
var.diag1 <- diag(vcov(mod1))
tidy1$OR <- exp(tidy1$`mod1$coefficients`)
tidy1$Oddstd <- sqrt((tidy1$OR^2)*var.diag1)
tidy1$LOW <- tidy1$OR-(1.96*tidy1$Oddstd)
tidy1$HIGH <- tidy1$OR+(1.96*tidy1$Oddstd)
write_csv(tidy1, here::here("output","mod1_tidy_OR_CI.csv"))

# fully adjusted model 
var.diag2 <- diag(vcov(mod2))
tidy2$OR <- exp(tidy2$`mod2$coefficients`)
tidy2$Oddstd <- sqrt((tidy2$OR^2)*var.diag2)
tidy2$LOW <- tidy2$OR-(1.96*tidy2$Oddstd)
tidy2$HIGH <- tidy2$OR+(1.96*tidy2$Oddstd)
write_csv(tidy2, here::here("output", "mod2_tidy_OR_CI.csv"))

# fully adjusted model plus covid time
var.diag3 <- diag(vcov(mod2_covid))
tidy3$OR <- exp(tidy3$`mod2_covid$coefficients`)
tidy3$Oddstd <- sqrt((tidy3$OR^2)*var.diag3)
tidy3$LOW <- tidy3$OR-(1.96*tidy3$Oddstd)
tidy3$HIGH <- tidy3$OR+(1.96*tidy3$Oddstd)
write_csv(tidy3, here::here("output","mod2_covid_tidy_OR_CI.csv"))

# ## plot
# library(forestplot)
# 
# # remove intercept
# tidy2rd<- tidy2[c(-1,-12,-21),]
# # plottext2<- row.names(tidy2rd)
# # print(plottext2)
# colnames(tidy2rd)[1]<-"mod2coefficients" 
# blank_row <- data.frame(mod2coefficients = "", OR = "", Oddstd = "", LOW = "", HIGH= "")
# 
# dfplot <- rbind(blank_row, tidy2rd[1:6,],blank_row, blank_row, tidy2rd[7:10,],
#                 blank_row, blank_row, tidy2rd[11:18,], blank_row,blank_row, tidy2rd[19:22,],
#                 blank_row, tidy2rd[23,])
# 
# plottext2 <- c("Age:", "20-24","25-29","30-34","35-39","40-44","45-49",
#                "","Ethnicity:",
#                "Asian or Asian British", "Black or Black British", "Mixed", "Other",
#                "","Region:",
#                "East", "East Midlands", "North East", "North West", "South East",
#                "South West", "West Midlands","Yorkshire and The Humber",
#                "","IMD:",
#                "imd:2", "imd:3", "imd:4", "imd:5",
#                "", "BMI")
# 
# dfplot <- dfplot %>% mutate_at(c(1:5), as.numeric) 
# 
# plot_mod2<-forestplot(labeltext=plottext2, 
#                       mean = c(dfplot$OR), lower = c(dfplot$LOW), upper = c(dfplot$HIGH),
#                       ci.vertices = TRUE, ci.vertices.height = 0.2, 
#                       col=fpColors(box= "royalblue", line="darkblue", zero="gray", hrz_lines="black"),
#                       title="Odds Ratio [95% CI]", 
#                       txt_gp=fpTxtGp(label=gpar(cex=1.2), ticks=gpar(cex=1.1)),
#                       grid=TRUE, 
#                       boxsize=0.3, 
#                       zero=1, 
#                       xticks=c(0.5, 0.75, 1.0,1.5,2.0), lwd.ci=1)
# 
# 
# save_plot(here::here("output", "glm_fullAdjs.jpeg"),
#           plot_mod2,
#           base_width = 9, base_height = 10
#           )
# 
# # save_forest(plot= plot_mod2,
# #   filename="glm_fullAdjs.jpeg", path=here::here("output"),
# # )
# # 
# 
# ##compare with glmer() repeated delcodes per mum.. 
# ##or select one row per mum as sensitivity.

