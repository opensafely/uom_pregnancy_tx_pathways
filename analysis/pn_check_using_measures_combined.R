library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")

df1 <- read_csv(
 here::here("output", "pn_check_combined_plot_8wk.csv"),

    col_types = cols_only(
    #  delivery_code_present  = col_double(),
    #  postnatal_8wk_code_present = col_double(),
    #  population  = col_number(),
    #  value = col_number(),
    #  date = col_date(format="%Y-%m-%d")
     )
 )

df2 <- read_csv(
 here::here("output", "pn_check_combined_plot_6wk.csv"))

df3 <- read_csv(
 here::here("output", "pn_check_combined_plot_12wk.csv"))

df1$cohort<-"8 weeks"
df2$cohort<-"6 weeks"
df3$cohort<-"12 weeks"

df_plot<-rbind(df1,df2,df3)
df_plot$cohort=factor(df_plot$cohort,levels=c("8 weeks","6 weeks","12 weeks"))

###### plot
 plot_pn_rate <- ggplot(df_plot, aes(x=date, group=cohort, color=cohort))+
  geom_line(aes(y=pn_rate_1000))+
  #geom_line(data=df_gaps, linetype="dashed", aes(color+age_cat))+ geom_point(aes(y=pn_rate_1000))+
  scale_x_date(date_labels = "%m-%Y", date_breaks = "3 months")+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  labs(
    title = "Rate of PN checks by month",
    subtitle = paste(first_mon,"-",last_mon),
    #caption = paste("Data from approximately", num_uniq_prac,"TPP Practices"),
    x = "Month",
    y = "Rate of PN checks per 1000 registered patients")+
  annotate(geom = "rect", xmin = as.Date("2021-01-01"),xmax = as.Date("2021-04-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-11-01"),xmax = as.Date("2020-12-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-06-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)

ggsave(
   plot= plot_pn_rate,
   filename="monthly_pn_rate_measures_combined.jpeg", path=here::here("output"),
)