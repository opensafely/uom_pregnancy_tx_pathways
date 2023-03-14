library('tidyverse')
library("data.table")
library("dplyr")
library('here')
library("lubridate")


rm(list=ls())
#setwd(here::here("output", "measures"))

df <- read_csv(
  here::here("output", "measures", "measure_postnatal_check_rate_by_age_cat.csv"),
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
df$date <- as.Date(df$date)
last.date=max(df$date)
df=df%>% filter(date!=last.date)

# define first and last months for automated plot
first_mon <- (format(min(df$date), "%m-%Y"))
last_mon <- (format(max(df$date), "%m-%Y"))

df$cal_mon <- month(df$date)
df$cal_year <- year(df$date)


### get monthly rate per 1000 patients
df_monrate <- df %>% group_by(cal_mon, cal_year) %>%
  mutate(pn_rate_1000 = value*1000) 


# df_mean <- df_monrate %>% group_by(cal_mon, cal_year) %>%
#   mutate(meanrate = mean(pn_rate_1000,na.rm=TRUE),
#          lowquart= quantile(pn_rate_1000, na.rm=TRUE)[2],
#          highquart= quantile(pn_rate_1000, na.rm=TRUE)[4],
#          ninefive= quantile(pn_rate_1000, na.rm=TRUE, c(0.95)),
#          five=quantile(pn_rate_1000, na.rm=TRUE, c(0.05)))


plot_pn_rate <- ggplot(df_monrate, aes(x=date, group=age_cat, color=age_cat))+
  geom_line(aes(y=pn_rate_1000),color="steelblue")+
  geom_point(aes(y=pn_rate_1000),color="steelblue")+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month")+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  labs(
    title = "Rate of PN checks by month",
    subtitle = paste(first_mon,"-",last_mon),
    #caption = paste("Data from approximately", num_uniq_prac,"TPP Practices"),
    x = "",
    y = "Rate of PN checks per 1000 registered patients")+
  annotate(geom = "rect", xmin = as.Date("2021-01-01"),xmax = as.Date("2021-04-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-11-01"),xmax = as.Date("2020-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-06-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)

ggsave(
   plot= plot_pn_rate,
   filename="monthly_pn_rate_measures8wkcode_by_age_cat.jpeg", path=here::here("output"),
)
