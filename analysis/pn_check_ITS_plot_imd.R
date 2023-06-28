library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
library("ggpubr")
#library(modelsummary)
#library("gtsummary")

## Import data
df = read.csv(here::here("output", "pn8wk", "measure_postnatal_check_rate_by_imd.csv"))

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

#define dates 
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df_plot=df_plot%>%mutate(covid=cut(date,breaks,labels = 1:2))
df_plot<-ungroup(df_plot)

df_plot=df_plot%>% filter(covid==1 | covid==2)
df_plot$covid= recode(df_plot$covid, '1'="0", '2'="1")
df_plot$covid <- factor(df_plot$covid, levels=c("0","1"))

df_plot=df_plot%>% group_by(covid, imd)%>%mutate(time.since=1:n())
df-plot$time.since <- ifelse(df_plot$covid==0,0,df_plot$time.since)

## categories are 0-5

# df for each imd category
df1=filter(df_plot, imd=="0")
df2=filter(df_plot, imd=="1")
df3=filter(df_plot, imd=="2")
df4=filter(df_plot, imd=="3")
df5=filter(df_plot, imd=="4")
df6=filter(df_plot, imd=="5")

# 1
m1.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df1)
# 1
m2.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df2)
# 2
m3.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df3)
# 3
m4.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df4)
# 4
m5.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df5)
# 5
m6.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df6)

# 0
(est1.1 <- cbind(Estimate = coef(m1.1), confint(m1.1)))
exp1.1=exp(est1.1)
# 1
(est2.1 <- cbind(Estimate = coef(m2.1), confint(m2.1)))
exp2.1=exp(est2.1)
# 2
(est3.1 <- cbind(Estimate = coef(m3.1), confint(m3.1)))
exp3.1=exp(est3.1)
# 3
(est4.1 <- cbind(Estimate = coef(m4.1), confint(m4.1)))
exp4.1=exp(est4.1)
# 4
(est5.1 <- cbind(Estimate = coef(m5.1), confint(m5.1)))
exp5.1=exp(est5.1)
# 5
(est6.1 <- cbind(Estimate = coef(m6.1), confint(m6.1)))
exp6.1=exp(est6.1)

## save estimates as .csv
## would need to change names for each category.R file
# write_csv(as.data.frame(exp1.1), here::here("output", "ITS_estimates_1.1.csv"))
# write_csv(as.data.frame(exp2.1), here::here("output", "ITS_estimates_2.1.csv"))
# write_csv(as.data.frame(exp3.1), here::here("output", "ITS_estimates_3.1.csv"))
# write_csv(as.data.frame(exp4.1), here::here("output", "ITS_estimates_4.1.csv"))
# write_csv(as.data.frame(exp5.1), here::here("output", "ITS_estimates_5.1.csv"))
# write_csv(as.data.frame(exp6.1), here::here("output", "ITS_estimates_6.1.csv"))

# creates combined df with estimates and CIs for each imd category
df_plot_overall=bind_rows(exp1.1[2,],exp2.1[2,],exp3.1[2,],exp4.1[2,],exp5.1[2,],exp6.1[2,],exp7.1[2,])

#adds imd column
df_plot_overall$imd=c("0","1","2","3","4","5")

df_plot_overall$imd=factor(df_plot_overall$imd,levels = c("0","1","2","3","4","5"))

# IRR - incident rate ratio
names(df_plot_overall)[1]="IRR"
names(df_plot_overall)[2]="ci_l"
names(df_plot_overall)[3]="ci_u"

## save this table? 
# gives df_plot_overall with IRR, LCI, UCI, imd and 7 rows
write_csv(as.data.frame(df_plot_overall), here::here("output", "ITS_plot_imd_overall.csv"))


## plots for each category

# 0
df1 <- cbind(df1, "resp" = predict(m1.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_imd_0<-ggplot(df1, aes(x=date, y=fit, group=covid)) + 
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
 
# 1
df2 <- cbind(df2, "resp" = predict(m2.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_imd_1<-ggplot(df2, aes(x=date, y=fit, group=covid)) + 
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

# 2
df3 <- cbind(df3, "resp" = predict(m3.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_imd_2<-ggplot(df3, aes(x=date, y=fit, group=covid)) + 
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

# 3
df4 <- cbind(df4, "resp" = predict(m4.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_imd_3<-ggplot(df4, aes(x=date, y=fit, group=covid)) + 
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

# 4
df5 <- cbind(df5, "resp" = predict(m5.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_imd_4<-ggplot(df5, aes(x=date, y=fit, group=covid)) + 
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

# 5
df6 <- cbind(df6, "resp" = predict(m6.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit
plot_ITS_imd_5<-ggplot(df6, aes(x=date, y=fit, group=covid)) + 
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

df_imd=bind_rows(df1,df2,df3,df4,df5,df6)
#df_imd$group=factor(df_imd$group,levels=c("0","1","2","3","4","5"))

## ITS plot with panels for each imd cat

plot_ITS_imd_1<-ggplot(data=df_imd,aes(x=date,y=rate,group=covid)) + 
 theme_bw()+
  #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2021-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  
  geom_point(shape = 4)+
  geom_smooth(se = FALSE,fullrange=FALSE, color="black")+
  update_geom_defaults("smooth", list(size = .5))+
  
  facet_grid(rows = vars(imd),scales="free_y",labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  
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

ggsave(plot= plot_ITS_imd_1,filename="plot_ITS_imd_1.jpeg", path=here::here("output"),)

#### creates plot with IRRs and error bars/CIs

## need to hash out text line to run locally
plot_ITS_imd_2<-ggplot(data=df_plot_overall, aes(y=imd, x=IRR, color=imd))+
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
facet_grid(imd~., scales = "free", space = "free")+
 theme(strip.text.y = element_text(angle = 0),
   axis.title.y =element_blank(),
        axis.text.y=element_blank(),
       axis.ticks.y=element_blank(),
       legend.title=element_blank(),
       legend.position="bottom")

ggsave(plot= plot_ITS_imd_2,filename="plot_ITS_imd_2.jpeg", path=here::here("output"),)




##add labels etc
# plot_ITS_age_cat<-ggplot(df_plot, aes(x=date, y=value, group=covid))+ theme_bw()+ 
#     annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
#     annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
#     geom_point(shape=4)+ geom_smooth(color="black",se = FALSE)+ scale_y_continuous(labels = scales::percent)+ scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+ 
#     theme(axis.text.x = element_text(angle = 60,hjust=1), legend.position = "bottom",legend.title =element_blank())+ labs(title = "", x = "", y = "")

# ggsave(
#    plot= plot_ITS_imd_0,
#    filename="pn_check_ITS_imd_0.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_imd_1,
#    filename="pn_check_ITS_age_imd_1.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_imd_2,
#    filename="pn_check_ITS_imd_2.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_imd_3,
#    filename="pn_check_ITS_imd_3.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_imd_4,
#    filename="pn_check_ITS_imd_4.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_imd_5,
#    filename="pn_check_ITS_imd_5.jpeg", path=here::here("output"),
# )


# # ##add labels etc
# # plot_ITS_imd<-ggplot(df, aes(x=date, y=value, group=covid))+ theme_bw()+ 
# #     annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
# #     annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
# #     geom_point(shape=4)+ geom_smooth(color="black",se = FALSE)+ scale_y_continuous(labels = scales::percent)+ scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+ 
# #     theme(axis.text.x = element_text(angle = 60,hjust=1), legend.position = "bottom",legend.title =element_blank())+ labs(title = "", x = "", y = "")

# # ggsave(
# #    plot= plot_ITS_imd,
# #    filename="pn_check_ITS_imd.jpeg", path=here::here("output"),
# # )