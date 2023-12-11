library('tidyverse')
library("data.table")
library("dplyr")
library('here')
library("lubridate")


rm(list=ls())
#setwd(here::here("output", "measures"))

df <- read_csv(
  here::here("output", "joined_8wk", "measure_postnatal_check_rate_by_age_cat.csv"),
  col_types = cols_only(

    #Identifier
    age_cat = col_factor(),
    
    # Outcomes
    delivery_code_present  = col_double(),
    postnatal_8wk_code_present = col_double(),
    population  = col_number(),
    value = col_number(),
    
    # Date
    date = col_date(format="%Y-%m-%d")
    
  ),
  na = character()
  )


## remove rows where delivery_code_present == 0 (group_by var in measures)
df=df%>% filter(delivery_code_present > 0)

# remove last month data
#df$date <- as.Date(df$date)
last.date="2023-08-31"
df=df%>% filter(date <=last.date)

# define first and last months for automated plot
first_mon <- (format(min(df$date), "%m-%Y"))
last_mon <- (format(max(df$date), "%m-%Y"))

df$cal_mon <- month(df$date)
df$cal_year <- year(df$date)

#redaction
df2<-df
df2$postnatal_8wk_code_present_redacted <- df2$postnatal_8wk_code_present
df2$postnatal_8wk_code_present_redacted[which(df2$postnatal_8wk_code_present_redacted <=7)] <- NA
df2$postnatal_8wk_code_present_redacted <- as.numeric(df2$postnatal_8wk_code_present_redacted)

df2$population_redacted <- df2$population
df2$population_redacted[which(df2$population <=7)] <- NA
df2$population_redacted <- as.numeric(df2$population)

#rounding to nearest 5
df2$postnatal_8wk_code_present_rounded<-round(df2$postnatal_8wk_code_present_redacted/5)*5
df2$population_rounded<-round(df2$population_redacted/5)*5

df2$value_r=df2$postnatal_8wk_code_present_rounded/df2$population_rounded
df_plot=df2 %>% filter(!is.na(value_r))

### get monthly rate per 1000 patients
df_monrate <- df_plot%>% group_by(cal_mon, cal_year) %>%
  mutate(pn_rate_1000 = value_r*1000) 

# create dataframe without NA 
df_gaps=df_monrate%>%filter(!is.na(postnatal_8wk_code_present_rounded))

# remove 45-49 age cat due to low counts
df_gaps=filter(df_gaps, age_cat !="45-49")

plot_pn_rate <- ggplot(df_gaps, aes(x=date, group=age_cat, color=age_cat))+
  geom_line(aes(y=pn_rate_1000))+
  #geom_line(data=df_gaps, linetype="dashed", aes(color+age_cat))+ geom_point(aes(y=pn_rate_1000))+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months")+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  labs(
    title = "Rate of PN checks by month",
    subtitle = paste(first_mon,"-",last_mon),
    #caption = paste("Data from approximately", num_uniq_prac,"TPP Practices"),
    color = "Age Group",
    x = "",
    y = "Rate of PN checks per 1000 registered patients")+
  annotate(geom = "rect", xmin = as.Date("2021-01-01"),xmax = as.Date("2021-04-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-11-01"),xmax = as.Date("2020-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-06-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)

ggsave(
   plot= plot_pn_rate,
   filename="monthly_pn_rate_measures8wkcode_by_age_cat_8wk_updated.jpeg", path=here::here("output"),
)