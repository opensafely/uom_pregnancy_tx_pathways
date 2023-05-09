library('tidyverse')
library("ggplot2")
library('dplyr')
library('lubridate')
library("data.table")
library('here')

rm(list=ls())

# import data
df <- read_csv(
  here::here("output", "measures", "measure_postnatal_check_rate_by_practice.csv"),
  col_types = cols_only(
    
    #Identifier
    practice = col_factor(),
    
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
first_mon <- (format(min(df$date), "%m-%Y"))
last_mon <- (format(max(df$date), "%m-%Y"))

# prescribing_number <- as.data.frame(sum(df$antibacterial_brit,na.rm = TRUE))
# colnames(prescribing_number) <- "Number of prescriptions"

df$cal_mon <- month(df$date)
df$cal_year <- year(df$date)

#changes counts under 6 to "[REDACTED]"
df2 <- df 
df2$postnatal_8wk_code_present_redacted <- ifelse(df2$postnatal_8wk_code_present <= 7, "[REDACTED]", df2$postnatal_8wk_code_present)
df2$postnatal_8wk_code_present_redacted <- as.numeric(df2$postnatal_8wk_code_present_redacted)

df2$population_redacted <- ifelse(df2$population <= 7, "[REDACTED]", df2$population)
df2$population_redacted <- as.numeric(df2$population_redacted)

#rounding to nearest 5
df2$postnatal_8wk_code_present_rounded<-round(df2$postnatal_8wk_code_present_redacted/5)*5
df2$population_rounded<-round(df2$population_redacted/5)*5

#create value_r based on rounded/redacted values
df2$value_r<-df2$postnatal_8wk_code_present_rounded/df2$population_rounded

### get monthly rate per 1000 patients
df_monrate <- df2%>% group_by(cal_mon, cal_year) %>%
  mutate(pn_rate_1000 = value_r*1000)

# mean list size per practice 
#dfls <- df %>% group_by(practice) %>%
#  mutate(listsize_ave = round(mean(population),digits = 0))

#group by practice here?
# df_gprate <- df %>% group_by(practice, cal_mon, cal_year) %>%
#   mutate(ab_rate_1000 = value*1000) 

num_uniq_prac <- as.numeric(dim(table((df_monrate$practice))))

df_mean <- df_monrate %>% group_by(cal_mon, cal_year) %>%
  mutate(meanPNrate = mean(pn_rate_1000,na.rm=TRUE),
         lowquart= quantile(pn_rate_1000, na.rm=TRUE)[2],
         highquart= quantile(pn_rate_1000, na.rm=TRUE)[4],
         ninefive= quantile(pn_rate_1000, na.rm=TRUE, c(0.95)),
         five=quantile(pn_rate_1000, na.rm=TRUE, c(0.05)))

#y_max <- max(df_mean$meanABrate) 
#y_min <- min(df_mean$meanABrate) 

#can change colours
plot_percentile <- ggplot(df_mean, aes(x=date))+
  geom_line(aes(y=meanPNrate),color="steelblue")+
  geom_point(aes(y=meanPNrate),color="steelblue")+
  geom_line(aes(y=lowquart), color="darkred")+
  geom_point(aes(y=lowquart), color="darkred")+
  geom_line(aes(y=highquart), color="darkred")+
  geom_point(aes(y=highquart), color="darkred")+
  geom_line(aes(y=ninefive), color="black", linetype="dotted")+
  geom_point(aes(y=ninefive), color="black")+
  geom_line(aes(y=five), color="black", linetype="dotted")+
  geom_point(aes(y=five), color="black")+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "1 month")+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  labs(
    title = "Rate of PN checks by month",
    subtitle = paste(first_mon,"-",last_mon),
    #caption = paste("Data from approximately", num_uniq_prac,"TPP Practices"),
    x = "",
    y = "Pn check rate per 1000 registered patients")+
  geom_vline(xintercept = as.numeric(as.Date("2019-12-31")), color="grey")+
  geom_vline(xintercept = as.numeric(as.Date("2020-12-31")), color="grey")

plot_percentile 

ggsave(
  plot= plot_percentile,
  filename="monthly_pn_rate_measures8wkcode_by_practice.jpeg", path=here::here("output"),
)

