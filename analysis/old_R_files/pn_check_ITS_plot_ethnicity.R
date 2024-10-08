library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
library("ggpubr")
#library(modelsummary)
#library("gtsummary")

## Import data
df = read.csv(here::here("output", "pn8wk", "measure_postnatal_check_rate_by_ethnicity.csv"))

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

# ## redaction and rounding
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

df_plot=df_plot%>% filter(covid==1 | covid==2)
df_plot$covid= recode(df_plot$covid, '1'="0", '2'="1")
df_plot$covid <- factor(df_plot$covid, levels=c("0","1"))

df_plot=df_plot%>% group_by(covid)%>%mutate(time.since=1:n())
df_plot$time.since <- ifelse(df_plot$covid==0,0,df_plot$time.since)

# df for each eth cat
df1=filter(df_plot, ethnicity=="White")
df2=filter(df_plot, ethnicity=="Mixed")
df3=filter(df_plot, ethnicity=="Asian or Asian British")
df4=filter(df_plot, ethnicity=="Black or Black British")
df5=filter(df_plot, ethnicity=="Other")
#df6=filter(df_plot, ethnicity=="Unknown")

# White
m1.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df1)
# Mixed
m2.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df2)
# Asian or Asian British
m3.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df3)
# Black or Black British
m4.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df4)
# Other
m5.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df5)
# Unknown
#m6.1 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df6)

# estimates and confidence intervals 
## exp(estimate) - to get IRR

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
#(est6.1 <- cbind(Estimate = coef(m6.1), confint(m6.1)))
#exp6.1=exp(est6.1)

## save estimates as .csv
# write_csv(as.data.frame(exp1.1), here::here("output", "ITS_estimates_1.1.csv"))
# write_csv(as.data.frame(exp2.1), here::here("output", "ITS_estimates_2.1.csv"))
# write_csv(as.data.frame(exp3.1), here::here("output", "ITS_estimates_3.1.csv"))
# write_csv(as.data.frame(exp4.1), here::here("output", "ITS_estimates_4.1.csv"))
# write_csv(as.data.frame(exp5.1), here::here("output", "ITS_estimates_5.1.csv"))
# write_csv(as.data.frame(exp6.1), here::here("output", "ITS_estimates_6.1.csv"))

# creates combined df with estimates and CIs for each eth category
df_plot_overall=bind_rows(exp1.1[2,],exp2.1[2,],exp3.1[2,],exp4.1[2,],exp5.1[2,])

#adds ethnicity column
#df_plot_overall$ethnicity=c("0","1","2","3","4","5")
df_plot_overall$ethnicity=c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other")

#df_plot_overall$ethnicity=factor(df_plot_overall$ethnicity,levels = c("0","1","2","3","4","5"))
df_plot_overall$ethnicity=factor(df_plot_overall$ethnicity,levels = c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other"))

# IRR - incident rate ratio
names(df_plot_overall)[1]="IRR"
names(df_plot_overall)[2]="ci_l"
names(df_plot_overall)[3]="ci_u"

# gives df_plot_overall with IRR, LCI, UCI, ethnicity and 7 rows
write_csv(as.data.frame(df_plot_overall), here::here("output", "ITS_plot_ethnicity_overall.csv"))

## plots for each category ##
## model prediction

df1 <- cbind(df1, "resp" = predict(m1.1, type = "response", se.fit = TRUE)[1:2])
df2 <- cbind(df2, "resp" = predict(m2.1, type = "response", se.fit = TRUE)[1:2])
df3 <- cbind(df3, "resp" = predict(m3.1, type = "response", se.fit = TRUE)[1:2])
df4 <- cbind(df4, "resp" = predict(m4.1, type = "response", se.fit = TRUE)[1:2])
df5 <- cbind(df5, "resp" = predict(m5.1, type = "response", se.fit = TRUE)[1:2])
#df6 <- cbind(df6, "resp" = predict(m6.1, type = "response", se.fit = TRUE)[1:2])

DF=rbind(df1,df2,df3,df4,df5)
DF$ethnicity=factor(DF$ethnicity,levels=c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other"))

# prediction -non covid - counterfactual trace
df1_counter <- subset(df1, select=-c(fit,se.fit))
df1_counter$covid=as.factor(0)
df1_counter$time.since=0
df1_counter  <- cbind(df1_counter, "resp" = predict(m1.1, type = "response", se.fit = TRUE, newdata = df1_counter)[1:2])

df2_counter <- subset(df2, select=-c(fit,se.fit))
df2_counter$covid=as.factor(0)
df2_counter$time.since=0
df2_counter  <- cbind(df2_counter, "resp" = predict(m2.1, type = "response", se.fit = TRUE, newdata = df2_counter)[1:2])

df3_counter <- subset(df3, select=-c(fit,se.fit))
df3_counter$covid=as.factor(0)
df3_counter$time.since=0
df3_counter  <- cbind(df3_counter, "resp" = predict(m3.1, type = "response", se.fit = TRUE, newdata = df3_counter)[1:2])

df4_counter <- subset(df4, select=-c(fit,se.fit))
df4_counter$covid=as.factor(0)
df4_counter$time.since=0
df4_counter  <- cbind(df4_counter, "resp" = predict(m4.1, type = "response", se.fit = TRUE, newdata = df4_counter)[1:2])

df5_counter <- subset(df5, select=-c(fit,se.fit))
df5_counter$covid=as.factor(0)
df5_counter$time.since=0
df5_counter  <- cbind(df5_counter, "resp" = predict(m5.1, type = "response", se.fit = TRUE, newdata = df5_counter)[1:2])

#df6_counter <- subset(df6, select=-c(fit,se.fit))
#df6_counter$covid=as.factor(0)
#df6_counter$time.since=0
#df6_counter  <- cbind(df6_counter, "resp" = predict(m6.1, type = "response", se.fit = TRUE, newdata = df6_counter)[1:2])

DF_counter= rbind(df1_counter,df2_counter,df3_counter,df4_counter,df5_counter)
DF_counter$ethnicity=factor(DF_counter$ethnicity,levels=c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other"))

DF_counter=DF_counter%>%filter(date>=as.Date("2020-04-01"))

## ITS plot with panels

plot_ITS<-ggplot(DF, aes(x=date, y=fit*1000/population, group=covid))+ 
  
  #actual rate point
  geom_point(shape=4, aes(x=date, y=postnatal_8wk_code_present_rounded/population*1000))+ 
  
  # prediction model  
  geom_line(color="blue")+ 
  geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="blue") +
  
  # prediction model: non covid    
  geom_line(aes(y=fit*1000/population,x=date),color="red",data = DF_counter)+
  geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population, ymax=((fit+1.96*se.fit)*1000)/population),alpha=0.2,fill="red",data = DF_counter) +
  
  # group by indication  
  facet_grid(rows = vars(ethnicity),scales="free_y",labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  
  # them
  theme_bw()+ 
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60",alpha=0.5)+   
  
  # legend  
  scale_x_date(date_labels = "%m-%Y", 
               breaks = seq(as.Date("2019-01-01"), as.Date(max(DF$date)), 
                            by = "3 months"))+
  theme(axis.text.x = element_text(angle = 60,hjust=1),
        axis.text.y = element_text(size = 6),
        legend.position = "bottom",legend.title =element_blank(),
        strip.text = element_text(size = 6))+
  labs(
    title = "",
    x = "", 
    y = "Number of PN checks per 1000 patients")

#plot_ITS

ggsave(
  plot= plot_ITS,
  filename="plot_ITS_eth_1.jpeg", path=here::here("output"), dpi = 300
  )

write.csv(DF,here::here("output","plot_ITS_check_ethnicity.csv"))

#### creates plot with IRRs and error bars/CIs

## need to hash out text line to run locally
plot_ITS_ethnicity_2<-ggplot(data=df_plot_overall, aes(y=ethnicity, x=IRR))+
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
  facet_grid(ethnicity~., scales = "free", space = "free")+
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank(),
        legend.position="bottom")

ggsave(
  plot= plot_ITS_ethnicity_2, 
  filename="plot_ITS_eth_2.jpeg", path=here::here("output"), dpi=300
  )


# plot_ITS_eth_1<-ggplot(data=df_eth,aes(x=date,y=rate,group=covid)) + 
#  theme_bw()+
#   #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+
#   annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2021-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  
#   geom_point(shape = 4)+
#   geom_smooth(se = FALSE,fullrange=FALSE, color="black")+
#   update_geom_defaults("smooth", list(size = .5))+
  
#   facet_grid(rows = vars(ethnicity),scales="free_y",labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  
#   scale_y_continuous(labels = scales::label_number(accuracy = 0.01))+
  
#   scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+
  
#   theme(axis.text.x = element_text(angle = 60,hjust=1),
#         legend.position = "bottom",legend.title =element_blank(),
#          axis.title.x=element_blank(),
#         )+
#   labs(
#     title = "Rate of postnatal checks over time",

#     x = "Month", 
#     y = "Rate")

# ggsave(plot= plot_ITS_eth_1,filename="plot_ITS_eth_1.jpeg", path=here::here("output"),)

# #### creates plot with IRRs and error bars/CIs

# plot_ITS_eth_2<-ggplot(data=df_plot_overall, aes(y=ethnicity, x=IRR))+
# geom_point()+

# geom_errorbarh(aes(xmin=ci_l, xmax=ci_u))+

# geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5)+
# theme_bw()+
# theme(text=element_text(family="Times",size=18, color="black"))+
# theme(panel.spacing = unit(1, "lines"))+
# labs(
#       title = "",
#     x="IRR (95% CI)",
#     y=""
#   )+
# facet_grid(ethnicity~., scales = "free", space = "free")+
#  theme(strip.text.y = element_text(angle = 0),
#    axis.title.y =element_blank(),
#         axis.text.y=element_blank(),
#        axis.ticks.y=element_blank(),
#        legend.title=element_blank(),
#        legend.position="bottom")

# ggsave(plot= plot_ITS_eth_2,filename="plot_ITS_eth_2.jpeg", path=here::here("output"),)
# ggsave(plot= plot_ITS_eth_2,filename="plot_ITS_eth_2.jpeg", path=here::here("output"),)


# ggsave(
#    plot= plot_ITS_eth_0,
#    filename="pn_check_ITS_eth_0.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_eth_1,
#    filename="pn_check_ITS_eth_1.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_eth_2,
#    filename="pn_check_ITS_eth_2.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_eth_3,
#    filename="pn_check_ITS_eth_3.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_eth_4,
#    filename="pn_check_ITS_eth_4.jpeg", path=here::here("output"),
# )
# ggsave(
#    plot= plot_ITS_eth_5,
#    filename="pn_check_ITS_eth_5.jpeg", path=here::here("output"),
# )

# ggsave(
#    plot= plot_ITS_ethnicity,
#    filename="pn_check_ITS_ethnicity.jpeg", path=here::here("output"),
# )