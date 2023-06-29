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
df$postnatal_8wk_code_present_redacted <- df$postnatal_8wk_code_present
df$postnatal_8wk_code_present_redacted[which(df$postnatal_8wk_code_present_redacted <=7)] <- NA
df$postnatal_8wk_code_present_redacted <- as.numeric(df$postnatal_8wk_code_present_redacted)

df$population_redacted <- df$population
df$population_redacted[which(df$population <=7)] <- NA
df$population_redacted <- as.numeric(df$population)

#rounding to nearest 5
df$postnatal_8wk_code_present_rounded<-round(df$postnatal_8wk_code_present_redacted/5)*5
df$population_rounded<-round(df$population_redacted/5)*5

df$rate=df$postnatal_8wk_code_present_rounded/df$population_rounded
df_plot=df %>% filter(!is.na(rate))

breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df_plot=df_plot%>%mutate(covid=cut(date,breaks,labels = 1:2))
df_plot<-ungroup(df_plot)

df_plot=df_plot%>% filter(covid==1 | covid==2)
df_plot$covid= recode(df_plot$covid, '1'="0", '2'="1")
df_plot$covid <- factor(df_plot$covid, levels=c("0","1"))

df_plot=df_plot%>% group_by(covid, age_cat)%>%mutate(time.since=1:n())
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
m1.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df1)
# 20-24
m2.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df2)
# 25-29
m3.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df3)
# 30-34
m4.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df4)
# 35-39
m5.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df5)
# 40-44
m6.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df6)
# 45-49
m7.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df7)

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
names(df_plot_overall)[1]="IRR"
names(df_plot_overall)[2]="ci_l"
names(df_plot_overall)[3]="ci_u"

## save this table? 
# gives df_plot_overall with IRR, LCI, UCI, age_cat and 7 rows
write_csv(as.data.frame(df_plot_overall), here::here("output", "ITS_plot_age_cat_overall.csv"))

#DF1.exp=df_plot_overall
#DF1.exp

## plots for each category

## 14-19
# estimates using adjusted model?
df1 <- cbind(df1, "resp" = predict(m1.1, type = "response", se.fit = TRUE)[1:2])
 # adds fit and se.fit columns despite message below
# Warning: "Outer names are only allowed for unnamed scalar atomic inputs" 
plot_ITS_14_19<-ggplot(df1, aes(x=date, y=fit, group=covid)) + 
 theme_bw()+
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-03-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  #annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  geom_point(shape=4)+
  geom_line(aes(y=fit/population),color="grey")+
  geom_ribbon(aes(ymin=(fit-1.96*se.fit)/population, ymax=(fit+1.96*se.fit)/population),alpha=0.2,fill="black") +
  geom_smooth(color="black",se = FALSE)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        legend.position = "bottom",legend.title =element_blank())+
  labs(
    title = "Rate of postnatal checks over time",

    x = "Month", 
    y = "Rate") 
 
# 20-24
df2 <- cbind(df2, "resp" = predict(m2.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_20_24<-ggplot(df2, aes(x=date, y=fit, group=covid)) + 
 theme_bw()+
  annotate(geom = "rect", xmin = as.Date("2019-03-01"),xmax = as.Date("2020-03-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  #annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  geom_point(shape=4)+
  geom_line(aes(y=fit/population),color="grey")+
  geom_ribbon(aes(ymin=(fit-1.96*se.fit)/population, ymax=(fit+1.96*se.fit)/population),alpha=0.2,fill="black") +
  geom_smooth(color="black",se = FALSE)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        legend.position = "bottom",legend.title =element_blank())+
  labs(
    title = "", 

    x = "", 
    y = "")

# 25-29
df3 <- cbind(df3, "resp" = predict(m3.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_25_29<-ggplot(df3, aes(x=date, y=fit, group=covid)) + 
 theme_bw()+
  annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  geom_point(shape=4)+
  geom_line(aes(y=fit/population),color="grey")+
  geom_ribbon(aes(ymin=(fit-1.96*se.fit)/population, ymax=(fit+1.96*se.fit)/population),alpha=0.2,fill="black") +
  geom_smooth(color="black",se = FALSE)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        legend.position = "bottom",legend.title =element_blank())+
  labs(
    title = "",

    x = "", 
    y = "")

# 30-34
df4 <- cbind(df4, "resp" = predict(m4.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_30_34<-ggplot(df4, aes(x=date, y=fit, group=covid)) + 
 theme_bw()+
  annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  geom_point(shape=4)+
  geom_line(aes(y=fit/population),color="grey")+
  geom_ribbon(aes(ymin=(fit-1.96*se.fit)/population, ymax=(fit+1.96*se.fit)/population),alpha=0.2,fill="black") +
  geom_smooth(color="black",se = FALSE)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        legend.position = "bottom",legend.title =element_blank())+
  labs(
    title = "",

    x = "", 
    y = "")

# 35-39
df5 <- cbind(df5, "resp" = predict(m5.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_35_39<-ggplot(df5, aes(x=date, y=fit, group=covid)) + 
 theme_bw()+
  annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  geom_point(shape=4)+
  geom_line(aes(y=fit/population),color="grey")+
  geom_ribbon(aes(ymin=(fit-1.96*se.fit)/population, ymax=(fit+1.96*se.fit)/population),alpha=0.2,fill="black") +
  geom_smooth(color="black",se = FALSE)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        legend.position = "bottom",legend.title =element_blank())+
  labs(
    title = "",

    x = "", 
    y = "")

# 40-44
df6 <- cbind(df6, "resp" = predict(m6.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit
plot_ITS_40_44<-ggplot(df6, aes(x=date, y=fit, group=covid)) + 
 theme_bw()+
  annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  geom_point(shape=4)+
  geom_line(aes(y=fit/population),color="grey")+
  geom_ribbon(aes(ymin=(fit-1.96*se.fit)/population, ymax=(fit+1.96*se.fit)/population),alpha=0.2,fill="black") +
  geom_smooth(color="black",se = FALSE)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        legend.position = "bottom",legend.title =element_blank())+
  labs(
    title = "",

    x = "", 
    y = "")

# 45-49
df7 <- cbind(df7, "resp" = predict(m7.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit
plot_ITS_45_49<-ggplot(df7, aes(x=date, y=fit, group=covid)) + 
 theme_bw()+
  annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  geom_point(shape=4)+
  geom_line(aes(y=fit/population),color="grey")+
  geom_ribbon(aes(ymin=(fit-1.96*se.fit)/population, ymax=(fit+1.96*se.fit)/population),alpha=0.2,fill="black") +
  geom_smooth(color="black",se = FALSE)+
  scale_y_continuous(labels = scales::percent)+
  scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        legend.position = "bottom",legend.title =element_blank())+
  labs(
    title = "",

    x = "", 
    y = "")

## should I just be using df_plot_overall here?
## this has IRR, ci_l, ci_u and age_cat

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
theme(text=element_text(family="Times",size=18, color="black"))+
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

