library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
library("ggpubr")
#library(modelsummary)
#library("gtsummary")

## Import data
df = read.csv(here::here("output", "pn8wk", "measure_postnatal_check_rate_by_region.csv"))

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

df_plot=df_plot%>% group_by(covid, region)%>%mutate(time.since=1:n())
df_plot$time.since <- ifelse(df_plot$covid==0,0,df_plot$time.since)

# df for each age cat
df1=filter(df_plot, region=="North East")
df2=filter(df_plot, region=="North West")
df3=filter(df_plot, region=="Yorkshire and The Humber")
df4=filter(df_plot, region=="East Midlands")
df5=filter(df_plot, region=="West Midlands")
df6=filter(df_plot, region=="East")
df7=filter(df_plot, region=="London")
df8=filter(df_plot, region=="South West")
df9=filter(df_plot, region=="South East")

# "North East"
m1.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df1)
# "North West"
m2.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df2)
# "Yorkshire and The Humber"
m3.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df3)
# "East Midlands"
m4.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df4)
# "West Midlands"
m5.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df5)
# "East"
m6.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df6)
# "London"
m7.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df7)
# "South West"
m8.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df8)
# "South East"
m9.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df9)


# "North East"
(est1.1 <- cbind(Estimate = coef(m1.1), confint(m1.1)))
exp1.1=exp(est1.1)
# "North West"
(est2.1 <- cbind(Estimate = coef(m2.1), confint(m2.1)))
exp2.1=exp(est2.1)
# "Yorkshire and The Humber"
(est3.1 <- cbind(Estimate = coef(m3.1), confint(m3.1)))
exp3.1=exp(est3.1)
# "East Midlands"
(est4.1 <- cbind(Estimate = coef(m4.1), confint(m4.1)))
exp4.1=exp(est4.1)
# "West Midlands"
(est5.1 <- cbind(Estimate = coef(m5.1), confint(m5.1)))
exp5.1=exp(est5.1)
# "East"
(est6.1 <- cbind(Estimate = coef(m6.1), confint(m6.1)))
exp6.1=exp(est6.1)
# "London"
(est7.1 <- cbind(Estimate = coef(m7.1), confint(m7.1)))
exp7.1=exp(est7.1)
# "South West"
(est8.1 <- cbind(Estimate = coef(m8.1), confint(m8.1)))
exp8.1=exp(est8.1)
# "South East"
(est9.1 <- cbind(Estimate = coef(m9.1), confint(m9.1)))
exp9.1=exp(est9.1)

## save estimates as .csv
# write_csv(as.data.frame(exp1.1), here::here("output", "ITS_estimates_1.1.csv"))
# write_csv(as.data.frame(exp2.1), here::here("output", "ITS_estimates_2.1.csv"))
# etc

# creates combined df with estimates and CIs for each region
df_plot_overall=bind_rows(exp1.1[2,],exp2.1[2,],exp3.1[2,],exp4.1[2,],exp5.1[2,],exp6.1[2,],exp7.1[2,],exp8.1[2,],exp9.1[2,])

#adds region column
df_plot_overall$region=c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East", "London", "South West", "South East")

df_plot_overall$region=factor(df_plot_overall$region,levels = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East", "London", "South West", "South East"))

# IRR - incident rate ratio
names(df_plot_overall)[1]="IRR"
names(df_plot_overall)[2]="ci_l"
names(df_plot_overall)[3]="ci_u"

## save this table? 
# gives df_plot_overall with IRR, LCI, UCI, region and 7 rows
write_csv(as.data.frame(df_plot_overall), here::here("output", "ITS_plot_region_overall.csv"))

#DF1.exp=df_plot_overall
#DF1.exp

## plots for each category

## NE
# estimates using adjusted model?
df1 <- cbind(df1, "resp" = predict(m1.1, type = "response", se.fit = TRUE)[1:2])
 # adds fit and se.fit columns despite message below
# Warning: "Outer names are only allowed for unnamed scalar atomic inputs" 
plot_ITS_North_East<-ggplot(df1, aes(x=date, y=value, group=covid)) + 
 theme_bw()+
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
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
 
# NW
df2 <- cbind(df2, "resp" = predict(m2.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_North_West<-ggplot(df2, aes(x=date, y=value, group=covid)) + 
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

# Yorks
df3 <- cbind(df3, "resp" = predict(m3.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_Yorkshire<-ggplot(df3, aes(x=date, y=value, group=covid)) + 
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

# E Mids
df4 <- cbind(df4, "resp" = predict(m4.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_East_Midlands<-ggplot(df4, aes(x=date, y=value, group=covid)) + 
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

# W Mids
df5 <- cbind(df5, "resp" = predict(m5.1, type = "response", se.fit = TRUE)[1:2])
plot_ITS_West_Midlands<-ggplot(df5, aes(x=date, y=value, group=covid)) + 
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

# East
df6 <- cbind(df6, "resp" = predict(m6.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit
plot_ITS_East<-ggplot(df6, aes(x=date, y=value, group=covid)) + 
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

# London
df7 <- cbind(df7, "resp" = predict(m7.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit
plot_ITS_London<-ggplot(df7, aes(x=date, y=value, group=covid)) + 
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

# SW
df8 <- cbind(df8, "resp" = predict(m8.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit
plot_ITS_South_West<-ggplot(df8, aes(x=date, y=value, group=covid)) + 
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

# SE
df9 <- cbind(df9, "resp" = predict(m9.1, type = "response", se.fit = TRUE)[1:2])#select fit & se.fit
plot_ITS_South_East<-ggplot(df9, aes(x=date, y=value, group=covid)) + 
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
## this has IRR, ci_l, ci_u and region

df_region=bind_rows(df1,df2,df3,df4,df5,df6,df7,df8,df9)
df_region$region=factor(df_region$region,levels=c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East", "London", "South West", "South East"))

# names(df_region)[1]="IRR"
# names(df_region)[2]="ci_l"
# names(df_region)[3]="ci_u"

## ITS plot with panels for each region

plot_ITS_region_1<-ggplot(data=df_region,aes(x=date,y=rate,group=covid)) + 
 theme_bw()+
  #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2021-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  
  geom_point(shape = 4)+
  geom_smooth(se = FALSE,fullrange=FALSE, color="black")+
  update_geom_defaults("smooth", list(size = .5))+
  
  facet_grid(rows = vars(region),scales="free_y",labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  
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

ggsave(plot= plot_ITS_region_1,filename="plot_ITS_region_1.jpeg", path=here::here("output"),)

  
#### creates plot with IRRs and error bars/CIs

## variable is group or age_cat for df_age_cat and age_cat for df_plot_overall (helpfully)
## need to hash out text line to run
plot_ITS_region_2<-ggplot(data=df_plot_overall, aes(y=region, x=IRR, color=region))+
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
facet_grid(region~., scales = "free", space = "free")+
 theme(strip.text.y = element_text(angle = 0),
   axis.title.y =element_blank(),
        axis.text.y=element_blank(),
       axis.ticks.y=element_blank(),
       legend.title=element_blank(),
       legend.position="bottom")

ggsave(plot= plot_ITS_region_2,filename="plot_ITS_region_2.jpeg", path=here::here("output"),)

### region names
# "North East"
# "North West"
# "Yorkshire and The Humber"
# "East Midlands"
# "West Midlands"
# "East"
# "London"
# "South West"
# "South East"