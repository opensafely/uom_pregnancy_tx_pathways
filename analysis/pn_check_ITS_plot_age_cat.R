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

# df$population_redacted <- ifelse(df$population <= 7, "NA", df2$population)
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

m1.0 <- glm.nb(value~ offset(log(population)) + covid + times + time.since , data = df)

(est1.0 <- cbind(Estimate = coef(m1.0), confint(m1.0)))

exp1.0=exp(est1.0)

##add labels etc
plot_ITS_age_cat<-ggplot(df, aes(x=date, y=value, group=covid))+ theme_bw()+ 
    annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-04-01"),xmax = as.Date("2021-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    geom_point(shape=4)+ geom_smooth(color="black",se = FALSE)+ scale_y_continuous(labels = scales::percent)+ scale_x_date(date_breaks = "1 month",date_labels =  "%Y-%m")+ 
    theme(axis.text.x = element_text(angle = 60,hjust=1), legend.position = "bottom",legend.title =element_blank())+ labs(title = "", x = "", y = "")

ggsave(
   plot= plot_ITS_age_cat,
   filename="pn_check_ITS_age_cat.jpeg", path=here::here("output"),
)