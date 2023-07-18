library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
library("ggpubr")
#library(modelsummary)
#library("gtsummary")

## Import data
df = read.csv(here::here("output", "pn8wk", "measure_postnatal_check_rate_by_age_cat.csv"))

# delivery_code_present  = col_double(),
# postnatal_8wk_code_present = col_double(),
# population  = col_number(),
# value = col_number(),
# measure = col_character(),
# )

df<-df%>%filter(delivery_code_present>0)

df$date <- as.Date(df$date)
df$month= format(df$date,"%m")

df$times <- as.numeric(as.factor(df$date))

## redaction and rounding
# df$postnatal_8wk_code_present_redacted <- ifelse(df$postnatal_8wk_code_present <= 7, "NA", df$postnatal_8wk_code_present)
# df$postnatal_8wk_code_present_redacted <- as.numeric(df$postnatal_8wk_code_present_redacted)

df$postnatal_8wk_code_present_redacted <- df$postnatal_8wk_code_present
df$postnatal_8wk_code_present_redacted[which(df$postnatal_8wk_code_present_redacted <=7)] <- NA
df$postnatal_8wk_code_present_redacted <- as.numeric(df$postnatal_8wk_code_present_redacted)

# df$population_redacted <- ifelse(df$population <= 7, "NA", df$population)
# df$population_redacted <- as.numeric(df$population_redacted)

df$population_redacted <- df$population
df$population_redacted[which(df$population <=7)] <- NA
df$population_redacted <- as.numeric(df$population)

#rounding to nearest 5
df$postnatal_8wk_code_present_rounded<-round(df$postnatal_8wk_code_present_redacted/5)*5
df$population_rounded<-round(df$population_redacted/5)*5

df$rate=df$postnatal_8wk_code_present_rounded/df$population_rounded
df_plot=df %>% filter(!is.na(rate))

## define dates
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df_plot=df_plot%>%mutate(covid=cut(date,breaks,labels = 1:2))
#df_plot<-ungroup(df_plot)

df_plot=df_plot%>% filter(covid==1 | covid==2)
df_plot$covid= recode(df_plot$covid, '1'="0", '2'="1")
df_plot$covid <- factor(df_plot$covid, levels=c("0","1"))

df_plot=df_plot%>% group_by(covid)%>%mutate(time.since=1:n())
df_plot$time.since <- ifelse(df_plot$covid==0,0,df_plot$time.since)

# df for each age cat
df1=filter(df_plot, age_cat=="14-19")
df2=filter(df_plot, age_cat=="20-24")
df3=filter(df_plot, age_cat=="25-29")
df4=filter(df_plot, age_cat=="30-34")
df5=filter(df_plot, age_cat=="35-39")
df6=filter(df_plot, age_cat=="40-44")
df7=filter(df_plot, age_cat=="45-49")

# 14-19
m1.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df1)
# 20-24
m2.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df2)
# 25-29
m3.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df3)
# 30-34
m4.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df4)
# 35-39
m5.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df5)
# 40-44
m6.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df6)
# 45-49
m7.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df7)

# # 14-19
# m1.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df1)
# # 20-24
# m2.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df2)
# # 25-29
# m3.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df3)
# # 30-34
# m4.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df4)
# # 35-39
# m5.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df5)
# # 40-44
# m6.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df6)
# # 45-49
# m7.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df7)

# estimates and confidence intervals 
## exp(estimate) - to get IRR

# 14-19
(est1.1 <- cbind(Estimate = coef(m1.1), confint(m1.1)))
exp1.1=exp(est1.1)
# 20-24
(est2.1 <- cbind(Estimate = coef(m2.1), confint(m2.1)))
exp2.1=exp(est2.1)
# 25-29
(est3.1 <- cbind(Estimate = coef(m3.1), confint(m3.1)))
exp3.1=exp(est3.1)
# 30-34
(est4.1 <- cbind(Estimate = coef(m4.1), confint(m4.1)))
exp4.1=exp(est4.1)
# 35-39
(est5.1 <- cbind(Estimate = coef(m5.1), confint(m5.1)))
exp5.1=exp(est5.1)
# 40-44
(est6.1 <- cbind(Estimate = coef(m6.1), confint(m6.1)))
exp6.1=exp(est6.1)
# 45-49
(est7.1 <- cbind(Estimate = coef(m7.1), confint(m7.1)))
exp7.1=exp(est7.1)

## save estimates as .csv
# write_csv(as.data.frame(exp1.1), here::here("output", "ITS_estimates_1.1.csv"))
# write_csv(as.data.frame(exp2.1), here::here("output", "ITS_estimates_2.1.csv"))
# write_csv(as.data.frame(exp3.1), here::here("output", "ITS_estimates_3.1.csv"))
# write_csv(as.data.frame(exp4.1), here::here("output", "ITS_estimates_4.1.csv"))
# write_csv(as.data.frame(exp5.1), here::here("output", "ITS_estimates_5.1.csv"))
# write_csv(as.data.frame(exp6.1), here::here("output", "ITS_estimates_6.1.csv"))
# write_csv(as.data.frame(exp7.1), here::here("output", "ITS_estimates_7.1.csv"))

# creates combined df with estimates and CIs for each age_cat
df_plot_overall=bind_rows(exp1.1[2,],exp2.1[2,],exp3.1[2,],exp4.1[2,],exp5.1[2,],exp6.1[2,],exp7.1[2,])

#adds age_cat column
df_plot_overall$age_cat=c("14-19","20-24","25-29","30-34","35-39","40-44","45-49")

df_plot_overall$age_cat=factor(df_plot_overall$age_cat,levels = c("14-19","20-24","25-29","30-34","35-39","40-44","45-49"))

# IRR - incident rate ratio
names(df_plot_overall)[1]="coefficient & IRR"
names(df_plot_overall)[2]="ci_l"
names(df_plot_overall)[3]="ci_u"

## add to project.yaml
# gives df_plot_overall with IRR, LCI, UCI, age_cat and 7 rows
write_csv(as.data.frame(df_plot_overall), here::here("output", "ITS_plot_age_cat_overall.csv"))

## plots for each category ##
## 14-19
df1 <- cbind(df1, "resp" = predict(m1.1, type = "response", se.fit = TRUE)[1:2])

## times variable isnt being selected here 
df1_counter <- df1[, c(10:12, 5, 7:8)] 
df1_counter$covid <- 0
df1_counter$time.since <- 0
#View(df_plot_counter)
df1_counter$covid<- as.factor(df1_counter$covid)
df1_counter <- cbind(df1_counter, "resp" = predict(m1.1, type = "response", se.fit = TRUE, newdata = df1_counter)[1:2])
df1_counter2<-df1_counter[,8:9]

df1_f<- cbind(df1,df1_counter2)

plot_ITS_14_19<-ggplot(df1_f, aes(x=date, y=fit*1000/population, group=covid))+
  theme_bw()+ 
    #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ 
    geom_line(aes(y=fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    geom_line(aes(y=rate*1000),color="blue")+
    geom_line(aes(y=resp.fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((resp.fit-1.96*se.fit)*1000)/population, ymax=((resp.fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    scale_x_date(date_labels = "%m-%Y", 
                 breaks = seq(as.Date("2019-01-01"), as.Date(max(df1_f$date)), 
                              by = "3 months"))+
      theme(axis.text.x = element_text(angle = 60,hjust=1),
          legend.position = "bottom",legend.title =element_blank())+
    labs(
      title = "",
      x = "", 
      y = "Number of PN checks per 1000 Delivery codes")
 
# 20-24
df2 <- cbind(df2, "resp" = predict(m2.1, type = "response", se.fit = TRUE)[1:2])

df2_counter <- df2[, c(10:12, 5, 7:8)] 
df2_counter$covid <- 0
df2_counter$time.since <- 0
#View(df_plot_counter)
df2_counter$covid<- as.factor(df2_counter$covid)
df2_counter <- cbind(df2_counter, "resp" = predict(m2.1, type = "response", se.fit = TRUE, newdata = df2_counter)[1:2])
df2_counter2<-df2_counter[,8:9]

df2_f<- cbind(df2,df2_counter2)

plot_ITS_20_24<-ggplot(df2_f, aes(x=date, y=fit*1000/population, group=covid))+
  theme_bw()+ 
    #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ 
    geom_line(aes(y=fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    geom_line(aes(y=rate*1000),color="blue")+
    geom_line(aes(y=resp.fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((resp.fit-1.96*se.fit)*1000)/population, ymax=((resp.fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    scale_x_date(date_labels = "%m-%Y", 
                 breaks = seq(as.Date("2019-01-01"), as.Date(max(df2_f$date)), 
                              by = "3 months"))+
      theme(axis.text.x = element_text(angle = 60,hjust=1),
          legend.position = "bottom",legend.title =element_blank())+
    labs(
      title = "",
      x = "", 
      y = "Number of PN checks per 1000 Delivery codes")

# 25-29
df3 <- cbind(df3, "resp" = predict(m3.1, type = "response", se.fit = TRUE)[1:2])

df3_counter <- df3[, c(10:12, 5, 7:8)] 
df3_counter$covid <- 0
df3_counter$time.since <- 0
#View(df_plot_counter)
df3_counter$covid<- as.factor(df3_counter$covid)
df3_counter <- cbind(df3_counter, "resp" = predict(m3.1, type = "response", se.fit = TRUE, newdata = df3_counter)[1:2])
df3_counter2<-df3_counter[,8:9]

df3_f<- cbind(df3,df3_counter2)

plot_ITS_25_29<-ggplot(df3_f, aes(x=date, y=fit*1000/population, group=covid))+
  theme_bw()+ 
    #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ 
    geom_line(aes(y=fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    geom_line(aes(y=rate*1000),color="blue")+
    geom_line(aes(y=resp.fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((resp.fit-1.96*se.fit)*1000)/population, ymax=((resp.fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    scale_x_date(date_labels = "%m-%Y", 
                 breaks = seq(as.Date("2019-01-01"), as.Date(max(df3_f$date)), 
                              by = "3 months"))+
      theme(axis.text.x = element_text(angle = 60,hjust=1),
          legend.position = "bottom",legend.title =element_blank())+
    labs(
      title = "",
      x = "", 
      y = "Number of PN checks per 1000 Delivery codes")

# 30-34
df4 <- cbind(df4, "resp" = predict(m4.1, type = "response", se.fit = TRUE)[1:2])

df4_counter <- df4[, c(10:12, 5, 7:8)] 
df4_counter$covid <- 0
df4_counter$time.since <- 0
#View(df_plot_counter)
df4_counter$covid<- as.factor(df4_counter$covid)
df4_counter <- cbind(df4_counter, "resp" = predict(m4.1, type = "response", se.fit = TRUE, newdata = df4_counter)[1:2])
df4_counter2<-df4_counter[,8:9]

df4_f<- cbind(df4,df4_counter2)

plot_ITS_30_34<-ggplot(df4_f, aes(x=date, y=fit*1000/population, group=covid))+
  theme_bw()+ 
    #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ 
    geom_line(aes(y=fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    geom_line(aes(y=rate*1000),color="blue")+
    geom_line(aes(y=resp.fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((resp.fit-1.96*se.fit)*1000)/population, ymax=((resp.fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    scale_x_date(date_labels = "%m-%Y", 
                 breaks = seq(as.Date("2019-01-01"), as.Date(max(df4_f$date)), 
                              by = "3 months"))+
      theme(axis.text.x = element_text(angle = 60,hjust=1),
          legend.position = "bottom",legend.title =element_blank())+
    labs(
      title = "",
      x = "", 
      y = "Number of PN checks per 1000 Delivery codes")

# 35-39
df5 <- cbind(df5, "resp" = predict(m5.1, type = "response", se.fit = TRUE)[1:2])

df5_counter <- df5[, c(10:12, 5, 7:8)] 
df5_counter$covid <- 0
df5_counter$time.since <- 0
#View(df_plot_counter)
df5_counter$covid<- as.factor(df5_counter$covid)
df5_counter <- cbind(df5_counter, "resp" = predict(m5.1, type = "response", se.fit = TRUE, newdata = df5_counter)[1:2])
df5_counter2<-df5_counter[,8:9]

df5_f<- cbind(df5,df5_counter2)

plot_ITS_35_39<-ggplot(df5_f, aes(x=date, y=fit*1000/population, group=covid))+
  theme_bw()+ 
    #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ 
    geom_line(aes(y=fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    geom_line(aes(y=rate*1000),color="blue")+
    geom_line(aes(y=resp.fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((resp.fit-1.96*se.fit)*1000)/population, ymax=((resp.fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    scale_x_date(date_labels = "%m-%Y", 
                 breaks = seq(as.Date("2019-01-01"), as.Date(max(df5_f$date)), 
                              by = "3 months"))+
      theme(axis.text.x = element_text(angle = 60,hjust=1),
          legend.position = "bottom",legend.title =element_blank())+
    labs(
      title = "",
      x = "", 
      y = "Number of PN checks per 1000 Delivery codes")

# 40-44
df6 <- cbind(df6, "resp" = predict(m6.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit

df6_counter <- df6[, c(10:12, 5, 7:8)] 
df6_counter$covid <- 0
df6_counter$time.since <- 0
#View(df_plot_counter)
df6_counter$covid<- as.factor(df6_counter$covid)
df6_counter <- cbind(df6_counter, "resp" = predict(m6.1, type = "response", se.fit = TRUE, newdata = df6_counter)[1:2])
df6_counter2<-df6_counter[,8:9]

df6_f<- cbind(df6,df6_counter2)

plot_ITS_40_44<-ggplot(df6_f, aes(x=date, y=fit*1000/population, group=covid))+
  theme_bw()+ 
    #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ 
    geom_line(aes(y=fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    geom_line(aes(y=rate*1000),color="blue")+
    geom_line(aes(y=resp.fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((resp.fit-1.96*se.fit)*1000)/population, ymax=((resp.fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    scale_x_date(date_labels = "%m-%Y", 
                 breaks = seq(as.Date("2019-01-01"), as.Date(max(df6_f$date)), 
                              by = "3 months"))+
      theme(axis.text.x = element_text(angle = 60,hjust=1),
          legend.position = "bottom",legend.title =element_blank())+
    labs(
      title = "",
      x = "", 
      y = "Number of PN checks per 1000 Delivery codes")


# 45-49
df7 <- cbind(df7, "resp" = predict(m7.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit

df7_counter <- df7[, c(10:12, 5, 7:8)] 
df7_counter$covid <- 0
df7_counter$time.since <- 0
#View(df_plot_counter)
df7_counter$covid<- as.factor(df7_counter$covid)
df7_counter <- cbind(df7_counter, "resp" = predict(m7.1, type = "response", se.fit = TRUE, newdata = df7_counter)[1:2])
df7_counter2<-df7_counter[,8:9]

df7_f<- cbind(df7,df7_counter2)

plot_ITS_45_49<-ggplot(df7_f, aes(x=date, y=fit*1000/population, group=covid))+
  theme_bw()+ 
    #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ 
    geom_line(aes(y=fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    geom_line(aes(y=rate*1000),color="blue")+
    geom_line(aes(y=resp.fit*1000/population),color="grey")+
    geom_ribbon(aes(ymin=((resp.fit-1.96*se.fit)*1000)/population, ymax=((resp.fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="black") +
    scale_x_date(date_labels = "%m-%Y", 
                 breaks = seq(as.Date("2019-01-01"), as.Date(max(df7_f$date)), 
                              by = "3 months"))+
      theme(axis.text.x = element_text(angle = 60,hjust=1),
          legend.position = "bottom",legend.title =element_blank())+
    labs(
      title = "",
      x = "", 
      y = "Number of PN checks per 1000 Delivery codes")

df_age_cat=bind_rows(df1,df2,df3,df4,df5,df6,df7)
#df_age_cat$group=factor(df_age_cat$group,levels=c("14-19","20-24","25-29","30-34","35-39","40-44","45-49"))

# names(df_age_cat)[1]="IRR"
# names(df_age_cat)[2]="ci_l"
# names(df_age_cat)[3]="ci_u"

## ITS plot with panels for each age cat

plot_ITS_age_cat_1<-ggplot(data=df_age_cat,aes(x=date,y=rate,group=covid)) + 
 theme_bw()+
  #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2021-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  
  geom_point(shape = 4)+
  geom_smooth(se = FALSE,fullrange=FALSE, color="black")+
  update_geom_defaults("smooth", list(size = .5))+
  
  facet_grid(rows = vars(age_cat),scales="free_y",labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01))+
  
  scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        legend.position = "bottom",legend.title =element_blank(),
         axis.title.x=element_blank(),
        )+
  labs(
    title = "Rate of postnatal checks over time",

    x = "Month", 
    y = "Rate")

ggsave(plot= plot_ITS_age_cat_1,filename="plot_ITS_age_cat_1.jpeg", path=here::here("output"),)

#### creates plot with IRRs and error bars/CIs

## need to hash out text line to run locally
plot_ITS_age_cat_2<-ggplot(data=df_plot_overall, aes(y=age_cat, x=IRR))+
geom_point()+

geom_errorbarh(aes(xmin=ci_l, xmax=ci_u))+

geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5)+
theme_bw()+
theme(text=element_text(family="times",size=18, color="black"))+
theme(panel.spacing = unit(1, "lines"))+
labs(
      title = "",
    x="IRR (95% CI)",
    y=""
  )+
facet_grid(age_cat~., scales = "free", space = "free")+
 theme(strip.text.y = element_text(angle = 0),
   axis.title.y =element_blank(),
        axis.text.y=element_blank(),
       axis.ticks.y=element_blank(),
       legend.title=element_blank(),
       legend.position="bottom")

ggsave(plot= plot_ITS_age_cat_2,filename="plot_ITS_age_cat_2.jpeg", path=here::here("output"),)


####error in 'eval' (predvars, data, env): object 'times' not found
##cbind .. predict.lm ->model.frame


##add labels etc
# plot_ITS_age_cat<-ggplot(df_plot, aes(x=date, y=value, group=covid))+ theme_bw()+ 
#     annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
#     annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
#     geom_point(shape=4)+ geom_smooth(color="black",se = FALSE)+ scale_y_continuous(labels = scales::percent)+ scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+ 
#     theme(axis.text.x = element_text(angle = 60,hjust=1), legend.position = "bottom",legend.title =element_blank())+ labs(title = "", x = "", y = "")

# ggsave(
#    plot= plot_ITS_14_19,
#    filename="pn_check_ITS_age_cat_14_19.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_20_24,
#    filename="pn_check_ITS_age_cat_20_24.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_25_29,
#    filename="pn_check_ITS_age_cat_25_29.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_30_34,
#    filename="pn_check_ITS_age_cat_30_34.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_35_39,
#    filename="pn_check_ITS_age_cat_35_39.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_40_44,
#    filename="pn_check_ITS_age_cat_40_44.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_45_49,
#    filename="pn_check_ITS_age_cat_45_49.jpeg", path=here::here("output"),
# )

