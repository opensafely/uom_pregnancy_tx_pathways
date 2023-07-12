library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
#library(modelsummary)
#library("gtsummary")

## Import data
df <- read.csv(
 here::here("output", "pn8wk", "measure_postnatal_check_rate.csv"))
 
#  col_types = cols_only(
#    delivery_code_present  = col_double(),
#    postnatal_8wk_code_present = col_double(),
#    population  = col_number(),
#    value = col_number(),
#    measure = col_character(),
#    ),
#  )

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
# check this works on real data
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df_plot=df_plot%>%mutate(covid=cut(date,breaks,labels = 1:2))
#df_plot<-ungroup(df_plot)

df_plot=df_plot%>% filter(covid==1 | covid==2)
df_plot$covid= recode(df_plot$covid, '1'="0", '2'="1")
df_plot$covid <- factor(df_plot$covid, levels=c("0","1"))

df_plot=df_plot%>% group_by(covid)%>%mutate(time.since=1:n())
df_plot$time.since <- ifelse(df_plot$covid==0,0,df_plot$time.since)

## new vars so far
# times (months since start of study) = T
# covid (binary) = D
# time.since (months since covid) = P
# our outcome var is rate (value) = Y

# rate is with rounding/redaction, value without
m1.0 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since  , data = df_plot)

# estimates and confidence intervals 
est1.0 <- cbind(Estimate = coef(m1.0), confint(m1.0))
## exp(estimate) - to get IRR
exp1.0=exp(est1.0)

DF=bind_rows(est1.0[2,],exp1.0[2,])

## binds coef and exp(coef) together 
names(DF)[1]="coefficient & IRR"
names(DF)[2]="ci_l"
names(DF)[3]="ci_u"

write_csv(as.data.frame(exp1.0), here::here("output", "ITS_estimates_overall.csv"))

## predict
df_plot <- cbind(df_plot, "resp" = predict(m1.0, type = "response", se.fit = TRUE)[1:2])

df_plot_counter <- df_plot[, c(10:12, 5, 7)] 
df_plot_counter$covid <- 0
df_plot_counter$time.since <- 0
#View(df_plot_counter)
df_plot_counter$covid<- as.factor(df_plot_counter$covid)
df_plot_counter <- cbind(df_plot_counter, "resp" = predict(m1.0, type = "response", se.fit = TRUE, newdata = df_plot_counter)[1:2])
df_plot_counter2<-df_plot_counter[,8:9]

df_plot_f<- cbind(df_plot,df_plot_counter2)

##add labels etc
plot_ITS_overall<-ggplot(df_plot_f, aes(x=date, y=fit*1000/population, group=covid))+ ###fit or rate??
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
                 breaks = seq(as.Date("2019-01-01"), as.Date(max(df_plot_f$date)), 
                              by = "3 months"))+
      theme(axis.text.x = element_text(angle = 60,hjust=1),
          legend.position = "bottom",legend.title =element_blank())+
    labs(
      title = "",
      x = "", 
      y = "Number of PN checks per 1000 Delivery codes")

ggsave(
   plot= plot_ITS_overall,
   filename="pn_check_ITS_overall_8wk_new_modelled.jpeg", path=here::here("output"),
)
