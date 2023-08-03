library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
library("ggpubr")
#library(modelsummary)
#library("gtsummary")

## is this useful/
## would we have to map quartiles to individual dfs/estimates

## Import data
df = read.csv(here::here("output", "pn8wk", "measure_postnatal_check_rate_by_practice.csv"),

delivery_code_present  = col_double(),
postnatal_8wk_code_present = col_double(),
population  = col_number(),
value = col_number(),
measure = col_character(),
)

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

# define dates
#breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max("2023-05-01"))

df_plot=df_plot%>%mutate(covid=cut(date,breaks,labels = 1:2))
df_plot<-ungroup(df_plot)

df_plot=df_plot%>% filter(covid==1 | covid==2)
df_plot$covid= recode(df_plot$covid, '1'="0", '2'="1")
df_plot$covid <- factor(df_plot$covid, levels=c("0","1"))

# write csv for rates
write_csv(as.data.frame(df_plot), here::here("output", "ITS_plot_data_practice.csv"))

df_plot=df%>% group_by(covid, practice)%>%mutate(time.since=1:n())
df_plot$time.since <- ifelse(df_plot$covid==0,0,df_plot$time.since)

m1.0 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df_plot)

(est1.0 <- cbind(Estimate = coef(m1.0), confint(m1.0)))

exp1.0=exp(est1.0)

##add labels etc
plot_ITS_practice<-ggplot(df, aes(x=date, y=fit, group=covid))+ theme_bw()+ 
    annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ geom_smooth(color="black",se = FALSE)+ scale_y_continuous(labels = scales::percent)+ scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+ 
    theme(axis.text.x = element_text(angle = 60,hjust=1), legend.position = "bottom",legend.title =element_blank())+ labs(title = "", x = "", y = "")

ggsave(
   plot= plot_ITS_practice,
   filename="pn_check_ITS_practice.jpeg", path=here::here("output"),
)