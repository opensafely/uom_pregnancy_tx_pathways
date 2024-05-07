library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")

df1 <- read_csv(
 here::here("output", "ITS_plot_data_overall_rates_and_predicted.csv"),

    col_types = cols_only(
    #  delivery_code_present  = col_double(),
    #  postnatal_8wk_code_present = col_double(),
    #  population  = col_number(),
    #  value = col_number(),
    #  date = col_date(format="%Y-%m-%d")
     )
 )

df2 <- read_csv(
 here::here("output", "ITS_plot_data_overall_rates_and_predicted_6wk.csv"))

df3 <- read_csv(
  here::here("output", "ITS_plot_data_overall_rates_and_predicted_12wk.csv"))

df1$cohort<-"8 weeks"
df2$cohort<-"6 weeks"
df3$cohort<-"12 weeks"

df_plot<-rbind(df1,df2,df3)
df_plot$cohort=factor(df_plot$cohort,levels=c("8 weeks","6 weeks","12 weeks"))

df4 <- read_csv(
  here::here("output", "ITS_plot_data_overall_rates_and_predicted_counterfact.csv"))

df5 <- read_csv(
  here::here("output", "ITS_plot_data_overall_rates_and_predicted_counterfact_6wk.csv"))

df6 <- read_csv(
  here::here("output", "ITS_plot_data_overall_rates_and_predicted_counterfact_12wk.csv"))

df4$cohort<-"8 weeks"
df5$cohort<-"6 weeks"
df6$cohort<-"12 weeks"

df_plot_counter_final<-rbind(df4,df5,df6)
df_plot_counter_final$cohort=factor(df_plot_counter_final$cohort,levels=c("8 weeks","6 weeks","12 weeks"))

###### plot

plot_ITS_overall_by_cohort<-ggplot(df_plot, aes(x=date, y=fit*1000/population_rounded, group=covid))+ 
  theme_bw()+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-05-11"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    #geom_point(shape=4)+   
  
  ##actual rate point
  geom_point(shape=4, aes(x=date, y=postnatal_8wk_code_present_rounded /population_rounded*1000))+ 
  geom_line(aes(y=postnatal_8wk_code_present_rounded /population_rounded*1000),color="grey")+
  
  #### prediction model  
  geom_line(color="blue")+ 
  geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population_rounded, ymax=((fit+1.96*se.fit)*1000)/population_rounded),alpha=0.2,fill="blue") +
      
  # prediction model: no covid -- counterfactual
  geom_line(aes(y=fit*1000/population_rounded,x=date),color="lightgreen",data = df_plot_counter_final)+
  geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population_rounded, ymax=((fit+1.96*se.fit)*1000)/population_rounded),alpha=0.2,fill="lightgreen",data = df_plot_counter_final) +

  # group by indication  
  facet_grid(rows = vars(cohort),scales="free_y",labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  
  # theme
  theme_bw()+ 
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60",alpha=0.5)+   
  
  # legend  
  scale_x_date(date_labels = "%m-%Y", 
               breaks = seq(as.Date("2019-01-01"), as.Date(max(df_plot$date)), 
                            by = "3 months"))+
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
  plot= plot_ITS_overall_by_cohort,
  filename="plot_ITS_overall_by_cohort.jpeg", path=here::here("output"), dpi = 300
  )

