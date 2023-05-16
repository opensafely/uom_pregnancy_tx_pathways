library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
#library(modelsummary)
#library("gtsummary")

## Import data
df = read.csv(here::here("output", "measures", "measure_postnatal_check_rate_by_age_cat.csv")),

delivery_code_present  = col_double(),
postnatal_8wk_code_present = col_double(),
population  = col_number(),
value = col_number(),
measure = col_character(),

df<-df%>%filter(delivery_code_present>0)

df$date <- as.Date(df$date)
df$month= format(df$date,"%m")

df$times <- as.numeric(as.factor(df$date))

# ## redaction and rounding
# df$postnatal_8wk_code_present_redacted <- ifelse(df$postnatal_8wk_code_present <= 7, "NA", df$postnatal_8wk_code_present)
# df$postnatal_8wk_code_present_redacted <- as.numeric(df$postnatal_8wk_code_present_redacted)

# df$population_redacted <- ifelse(df$population <= 7, "NA", df$population)
# df$population_redacted <- as.numeric(df$population_redacted)

# #rounding to nearest 5
# df$postnatal_8wk_code_present_rounded<-round(df$postnatal_8wk_code_present_redacted/5)*5
# df$population_rounded<-round(df$population_redacted/5)*5

# df$rate=df$postnatal_8wk_code_present_rounded/df$population_rounded
# df_plot=df %>% filter(!is.na(rate))

# ## then change to df_plot

breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df=df%>%mutate(covid=cut(date,breaks,labels = 1:2))

df=df%>% filter(covid==1 | covid==2)
df$covid= recode(df$covid, '1'="0", '2'="1")
df$covid <- factor(df$covid, levels=c("0","1"))

df=df%>% group_by(covid, age_cat)%>%mutate(time.since=1:n())
df$time.since <- ifelse(df$covid==0,0,df$time.since)

# line for each age cat
df1=filter(df, age_cat=="14-19")
df2=filter(df, age_cat=="20-24")
df3=filter(df, age_cat=="25-29")
df4=filter(df, age_cat=="30-34")
df5=filter(df, age_cat=="35-39")
df6=filter(df, age_cat=="40-44")
df7=filter(df, age_cat=="45-49")

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

## is this part useful and can we just skip to
## plotting each df? add code below

# df_plot=bind_rows(exp1.1[2,],exp2.1[2,],exp3.1[2,],exp4.1[2,],exp5.1[2,],exp6.1[2,])
# df$age_cat=c("14-19","20-24","25-29","30-34","35-39","40-44","45-49")
# #reorder
# #DF$Infection=factor(DF$Infection,levels = c("UTI","URTI","LRTI","Sinusitis","Otitis_externa","Otitis_media"))

# #names(DF)[1]="IRR"
# #names(DF)[2]="ci_l"
# #names(DF)[3]="ci_u"

# #DF1.exp=DF
# #DF1.exp




##add labels etc
plot_ITS_age_cat<-ggplot(df_plot, aes(x=date, y=value, group=covid))+ theme_bw()+ 
    annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ geom_smooth(color="black",se = FALSE)+ scale_y_continuous(labels = scales::percent)+ scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+ 
    theme(axis.text.x = element_text(angle = 60,hjust=1), legend.position = "bottom",legend.title =element_blank())+ labs(title = "", x = "", y = "")

ggsave(
   plot= plot_ITS_age_cat,
   filename="pn_check_ITS_age_cat.jpeg", path=here::here("output"),
)