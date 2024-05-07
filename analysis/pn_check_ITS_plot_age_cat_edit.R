library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
library("ggpubr")

#########################################
## ITS plot with 45-49 age cat removed ##
#########################################


## Import data
df <- read_csv(
 here::here("output", "joined_8wk", "measure_postnatal_check_rate_by_age_cat.csv"),

    col_types = cols_only(
     delivery_code_present  = col_double(),
     postnatal_8wk_code_present = col_double(),
     population  = col_number(),
     value = col_number(),
     date = col_date(format="%Y-%m-%d"),
     age_cat = col_factor()
     )
 )

df<-df%>%filter(delivery_code_present>0)

df$date <- as.Date(df$date)
df$month= format(df$date,"%m")

# remove last few months
last.date="2023-08-31"
df=df%>% filter(date <=last.date)

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

## define dates
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df_plot=df_plot%>%mutate(covid=cut(date,breaks,labels = 1:2))

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

# creates combined df with estimates and CIs for each age_cat
df_plot_overall=bind_rows(exp1.1[2,],exp2.1[2,],exp3.1[2,],exp4.1[2,],exp5.1[2,],exp6.1[2,])

#adds age_cat column
df_plot_overall$age_cat=c("14-19","20-24","25-29","30-34","35-39","40-44")
df_plot_overall$age_cat=factor(df_plot_overall$age_cat,levels = c("14-19","20-24","25-29","30-34","35-39","40-44"))

# IRR - incident rate ratio
names(df_plot_overall)[1]="IRR"
names(df_plot_overall)[2]="ci_l"
names(df_plot_overall)[3]="ci_u"

# gives df_plot_overall with IRR, LCI, UCI, age_cat and 7 rows
write_csv(as.data.frame(df_plot_overall), here::here("output", "ITS_estimates_IRR_age_cat.csv"))

## plots for each category ##
## model prediction
df1 <- cbind(df1, "resp" = predict(m1.1, type = "response", se.fit = TRUE)[1:2])
df2 <- cbind(df2, "resp" = predict(m2.1, type = "response", se.fit = TRUE)[1:2])
df3 <- cbind(df3, "resp" = predict(m3.1, type = "response", se.fit = TRUE)[1:2])
df4 <- cbind(df4, "resp" = predict(m4.1, type = "response", se.fit = TRUE)[1:2])
df5 <- cbind(df5, "resp" = predict(m5.1, type = "response", se.fit = TRUE)[1:2])
df6 <- cbind(df6, "resp" = predict(m6.1, type = "response", se.fit = TRUE)[1:2])

DF=rbind(df1,df2,df3,df4,df5,df6)
DF$age_cat<-factor(DF$age_cat,levels=c("14-19","20-24","25-29","30-34","35-39","40-44"))

# prediction -non covid - counterfactual trace
df1_counter <- subset(df1, select=-c(fit,se.fit))
df1_counter$covid=as.factor(0)
df1_counter$time.since=0
df1_counter  <- cbind(df1_counter, "resp" = predict(m1.1, type = "response", se.fit = TRUE, newdata = df1_counter)[1:2])
df1_counter_final=df1_counter%>%filter(date>=as.Date("2020-03-01"))

df2_counter <- subset(df2, select=-c(fit,se.fit))
df2_counter$covid=as.factor(0)
df2_counter$time.since=0
df2_counter  <- cbind(df2_counter, "resp" = predict(m2.1, type = "response", se.fit = TRUE, newdata = df2_counter)[1:2])
df2_counter_final=df2_counter%>%filter(date>=as.Date("2020-03-01"))

df3_counter <- subset(df3, select=-c(fit,se.fit))
df3_counter$covid=as.factor(0)
df3_counter$time.since=0
df3_counter  <- cbind(df3_counter, "resp" = predict(m3.1, type = "response", se.fit = TRUE, newdata = df3_counter)[1:2])
df3_counter_final=df3_counter%>%filter(date>=as.Date("2020-03-01"))

df4_counter <- subset(df4, select=-c(fit,se.fit))
df4_counter$covid=as.factor(0)
df4_counter$time.since=0
df4_counter  <- cbind(df4_counter, "resp" = predict(m4.1, type = "response", se.fit = TRUE, newdata = df4_counter)[1:2])
df4_counter_final=df4_counter%>%filter(date>=as.Date("2020-03-01"))

df5_counter <- subset(df5, select=-c(fit,se.fit))
df5_counter$covid=as.factor(0)
df5_counter$time.since=0
df5_counter  <- cbind(df5_counter, "resp" = predict(m5.1, type = "response", se.fit = TRUE, newdata = df5_counter)[1:2])
df5_counter_final=df5_counter%>%filter(date>=as.Date("2020-03-01"))

df6_counter <- subset(df6, select=-c(fit,se.fit))
df6_counter$covid=as.factor(0)
df6_counter$time.since=0
df6_counter  <- cbind(df6_counter, "resp" = predict(m6.1, type = "response", se.fit = TRUE, newdata = df6_counter)[1:2])
df6_counter_final=df6_counter%>%filter(date>=as.Date("2020-03-01"))

DF_plot_f= rbind(df1,df2,df3,df4,df5,df6)
DF_plot_f$age_cat=factor(DF_plot_f$age_cat,levels=c("14-19","20-24","25-29","30-34","35-39","40-44"))

DF_counter= rbind(df1_counter_final,df2_counter_final,df3_counter_final,df4_counter_final,df5_counter_final,df6_counter_final)
DF_counter$age_cat=factor(DF_counter$age_cat,levels=c("14-19","20-24","25-29","30-34","35-39","40-44"))


### plot 
plot_ITS_age_cat_edit<-ggplot(DF_plot_f, aes(x=date, y=fit*1000/population_rounded, group=covid))+ 
  theme_bw()+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+  
  
  ##actual rate point
  geom_point(shape=4, aes(x=date, y=postnatal_8wk_code_present_rounded /population_rounded*1000))+ 
  geom_line(aes(y=postnatal_8wk_code_present_rounded /population_rounded*1000),color="grey")+
  
  #### prediction model  
  geom_line(color="blue")+ 
  geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population_rounded, ymax=((fit+1.96*se.fit)*1000)/population_rounded),alpha=0.2,fill="blue") +
      
  # prediction model: no covid -- counterfactual
  geom_line(aes(y=fit*1000/population_rounded,x=date),color="lightgreen",data = DF_counter)+
  geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population_rounded, ymax=((fit+1.96*se.fit)*1000)/population_rounded),alpha=0.2,fill="lightgreen",data = DF_counter) +
      
  # group by indication  
  facet_grid(rows = vars(age_cat),scales="free_y",labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  
  # theme
  theme_bw()+ 
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60",alpha=0.5)+   
  
  # legend  
  scale_x_date(date_labels = "%m-%Y", 
               breaks = seq(as.Date("2019-01-01"), as.Date(max(DF_plot_f$date)), 
                            by = "2 months"))+
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
  plot= plot_ITS_age_cat_edit,
  filename="plot_ITS_by_age_cat.jpeg", path=here::here("output"), dpi = 300
  )


DF_plot_f <- DF_plot_f[,-c(1,3:5,9:10)]
DF_counter <- DF_counter[,-c(1,3:5,9:10)]

write.csv(DF_plot_f,here::here("output","plot_data_ITS_age_cat.csv"))
write.csv(DF_counter,here::here("output","plot_data_ITS_age_cat_counterfactual.csv"))

#### creates plot with IRRs and error bars/CIs
plot_ITS_age_cat<-ggplot(data=df_plot_overall, aes(y=age_cat, x=IRR))+
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

ggsave(
  plot= plot_ITS_age_cat, 
  filename="plot_ITS_age_cat_IRRs.jpeg", path=here::here("output"), dpi=300
  )

