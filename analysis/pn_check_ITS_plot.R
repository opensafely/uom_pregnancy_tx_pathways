library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
#library(modelsummary)
#library("gtsummary")

## Import data
df = read.csv(here::here("output", "measures", "measure_postnatal_check_rate.csv")),

## add measure column and rename so we can bind by row
# df1 = read.csv(here::here("output", "measures", "measure_postnatal_check_rate_by_age_cat.csv")),
# df1$measure="age_cat"
# df1<-df1%>%rename(var1=age_cat)

# df2 = read.csv(here::here("output", "measures", "measure_postnatal_check_rate_by_ethnicity.csv")),
# df2$measure="ethnicity"
# df2<-df2%>%rename(var1=ethnicity)

# df3 = read.csv(here::here("output", "measures", "measure_postnatal_check_rate_by_imd.csv")),
# df3$measure="imd"
# df3<-df3%>%rename(var1=imd)

# df4 = read.csv(here::here("output", "measures", "measure_postnatal_check_rate_by_practice.csv")),
# df4$measure="practice"
# df4<-df4%>%rename(var1=practice)

# df=rbind(df1,df2,df3,df4)

### do we need to build rounding into this?

#filter by del code present?
df<-df%>%filter(delivery_code_present>0)

delivery_code_present  = col_double(),
postnatal_8wk_code_present = col_double(),
population  = col_number(),
value = col_number(),
measure = col_character(),

df$date <- as.Date(df$date)
df$month= format(df$date,"%m")

df$times <- as.numeric(as.factor(df$date))

## define dates
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df=df%>%mutate(covid=cut(date,breaks,labels = 1:2))

## gives a covid column with 1-2
# 1 = before march 2020
# 2 = march 2020 onwards

df=df%>% filter(covid==1 | covid==2)
df$covid= recode(df$covid, '1'="0", '2'="1")
df$covid <- factor(df$covid, levels=c("0","1"))

## this doesnt work with multiple rows for each month?
df=df%>% group_by(covid)%>%mutate(time.since=1:n())
df$time.since <- ifelse(df$covid==0,0,df$time.since)

## from other repo - do we need?
# df$value=1-df$percentage
# df$count= df$total-df$count

## new vars so far
# times (months since start of study) = T
# covid (binary) = D
# time.since (months since covid) = P
# our outcome var is rate (value) = Y

df_overall=df

## split data when measures files added
#df0=df_overall
# df1=df_overall%>%filter(measure=="age_cat")
# df2=df_overall%>%filter(measure=="ethnicity")
# df3=df_overall%>%filter(measure=="imd")
# df4=df_overall%>%filter(measure=="practice")

##warnings for non-integers?

# 1. overall
m2.0 <- glm.nb(value~ offset(log(population)) + covid + times + time.since  , data = df_overall)

# # 2.  age_cat
# m2.1 <- glm.nb(count~ offset(log(total)) + covid + times + time.since  , data = df1)

# # 3.  ethnicity
# m3.1 <- glm.nb(count~ offset(log(total))+ covid + times + time.since  , data = df2)

# # 3.  imd
# m4.1 <- glm.nb(count~ offset(log(total)) + covid + times + time.since  , data = df3)

# # 5.  practice
# m5.1 <- glm.nb(count~ offset(log(total))+  covid + times + time.since  , data = df4)


(est2.0 <- cbind(Estimate = coef(m2.0), confint(m2.0)))

exp2.0=exp(est2.0)

##add labels etc
ggplot(df_overall, aes(x=date, y=value, group=covid))+ theme_bw()+ annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ geom_point(shape=4)+ geom_smooth(color="black",se = FALSE)+ scale_y_continuous(labels = scales::percent)+ scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+ theme(axis.text.x = element_text(angle = 60,hjust=1), legend.position = "bottom",legend.title =element_blank())+ labs(title = "", x = "", y = "")