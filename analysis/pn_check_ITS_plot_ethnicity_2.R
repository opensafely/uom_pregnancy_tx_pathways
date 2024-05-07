library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
library("ggpubr")

## Import data
df <- read_csv(
 here::here("output", "joined_8wk", "measure_postnatal_check_rate_by_ethnicity_2.csv"),

    col_types = cols_only(
     delivery_code_present  = col_double(),
     postnatal_8wk_code_present = col_double(),
     population  = col_number(),
     value = col_number(),
     date = col_date(format="%Y-%m-%d"),
     ethnicity2 = col_number()
     )
 )


df<-df%>%filter(delivery_code_present>0)

df$date <- as.Date(df$date)
df$month= format(df$date,"%m")

# remove last few months
last.date="2023-08-31"
df=df%>% filter(date <=last.date)

df$times <- as.numeric(as.factor(df$date))

## redaction and rounding
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

## define dates
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df_plot=df_plot%>%mutate(covid=cut(date,breaks,labels = 1:2))

df_plot=df_plot%>% filter(covid==1 | covid==2)
df_plot$covid= recode(df_plot$covid, '1'="0", '2'="1")
df_plot$covid <- factor(df_plot$covid, levels=c("0","1"))

df_plot=df_plot%>% group_by(covid, ethnicity2)%>%mutate(time.since=1:n())
df_plot$time.since <- ifelse(df_plot$covid==0,0,df_plot$time.since)

#df_plot$ethnicity2<- as.factor(df_plot$ethnicity2)
df_plot<- df_plot%>% mutate(ethnicity2_labs = case_when(is.na(ethnicity2) ~ "Unknown",
                                                  ethnicity2== 1 ~ "White",
                                                  ethnicity2== 2 ~ "Mixed",
                                                  ethnicity2== 3 ~ "Asian or Asian British",
                                                  ethnicity2== 4 ~ "Black or Black British",
                                                  ethnicity2== 5 ~ "Other",
                                                  ethnicity2== 0 ~ "Unknown"))
df_plot$ethnicity2_labs<- as.factor(df_plot$ethnicity2_labs)

#df for each category
df1=filter(df_plot, ethnicity2_labs=="White")
df2=filter(df_plot, ethnicity2_labs=="Mixed")
df3=filter(df_plot, ethnicity2_labs=="Asian or Asian British")
df4=filter(df_plot, ethnicity2_labs=="Black or Black British")
df5=filter(df_plot, ethnicity2_labs=="Other")

m1.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df1)
m2.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df2)
m3.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df3)
m4.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df4)
m5.1 <- glm.nb(postnatal_8wk_code_present_rounded~ offset(log(population_rounded)) + covid + times + time.since , data = df5)

# estimates and confidence intervals 
## exp(estimate) - to get IRR

(est1.1 <- cbind(Estimate = coef(m1.1), confint(m1.1)))
exp1.1=exp(est1.1)

(est2.1 <- cbind(Estimate = coef(m2.1), confint(m2.1)))
exp2.1=exp(est2.1)

(est3.1 <- cbind(Estimate = coef(m3.1), confint(m3.1)))
exp3.1=exp(est3.1)

(est4.1 <- cbind(Estimate = coef(m4.1), confint(m4.1)))
exp4.1=exp(est4.1)

(est5.1 <- cbind(Estimate = coef(m5.1), confint(m5.1)))
exp5.1=exp(est5.1)

# creates combined df with estimates and CIs for each eth cat
df_plot_overall=bind_rows(exp1.1[2,],exp2.1[2,],exp3.1[2,],exp4.1[2,],exp5.1[2,])

#df_plot_overall$ethnicity2_labs=c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other")
df_plot_overall$Ethnicity= c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other")

names(df_plot_overall)[1]="IRR"
names(df_plot_overall)[2]="ci_l"
names(df_plot_overall)[3]="ci_u"
names(df_plot_overall)[4]="Ethnicity"

write_csv(as.data.frame(df_plot_overall), here::here("output", "ITS_estimates_IRR_ethnicity2.csv"))

## plots for each category ##
## model prediction
df1 <- cbind(df1, "resp" = predict(m1.1, type = "response", se.fit = TRUE)[1:2])
df2 <- cbind(df2, "resp" = predict(m2.1, type = "response", se.fit = TRUE)[1:2])
df3 <- cbind(df3, "resp" = predict(m3.1, type = "response", se.fit = TRUE)[1:2])
df4 <- cbind(df4, "resp" = predict(m4.1, type = "response", se.fit = TRUE)[1:2])
df5 <- cbind(df5, "resp" = predict(m5.1, type = "response", se.fit = TRUE)[1:2])

DF=rbind(df1,df2,df3,df4,df5)
DF$ethnicity2_labs<-factor(DF$ethnicity2_labs,levels=c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other"))

# prediction -non covid - counterfactual trace
df1_counter <- subset(df1, select=-c(fit,se.fit))
df1_counter$covid=as.factor(0)
df1_counter$time.since=0
df1_counter  <- cbind(df1_counter, "resp" = predict(m1.1, type = "response", se.fit = TRUE, newdata = df1_counter)[1:2])
df1_counter_final=df1_counter%>%filter(date>=as.Date("2020-03-01"))

df2_counter <- subset(df2, select=-c(fit,se.fit))
df2_counter$covid=as.factor(0)
df2_counter$time.since=0
df2_counter  <- cbind(df2_counter, "resp" = predict(m2.1, type = "response", se.fit = TRUE, newdata = df2_counter)[1:2])
df2_counter_final=df2_counter%>%filter(date>=as.Date("2020-03-01"))

df3_counter <- subset(df3, select=-c(fit,se.fit))
df3_counter$covid=as.factor(0)
df3_counter$time.since=0
df3_counter  <- cbind(df3_counter, "resp" = predict(m3.1, type = "response", se.fit = TRUE, newdata = df3_counter)[1:2])
df3_counter_final=df3_counter%>%filter(date>=as.Date("2020-03-01"))

df4_counter <- subset(df4, select=-c(fit,se.fit))
df4_counter$covid=as.factor(0)
df4_counter$time.since=0
df4_counter  <- cbind(df4_counter, "resp" = predict(m4.1, type = "response", se.fit = TRUE, newdata = df4_counter)[1:2])
df4_counter_final=df4_counter%>%filter(date>=as.Date("2020-03-01"))

df5_counter <- subset(df5, select=-c(fit,se.fit))
df5_counter$covid=as.factor(0)
df5_counter$time.since=0
df5_counter  <- cbind(df5_counter, "resp" = predict(m5.1, type = "response", se.fit = TRUE, newdata = df5_counter)[1:2])
df5_counter_final=df5_counter%>%filter(date>=as.Date("2020-03-01"))

DF_plot_f= rbind(df1,df2,df3,df4,df5)
DF_plot_f$ethnicity2_labs=factor(DF_plot_f$ethnicity2_labs,levels=c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other"))

DF_counter= rbind(df1_counter,df2_counter,df3_counter,df4_counter,df5_counter)
DF_counter$ethnicity2_labs=factor(DF_counter$ethnicity2_labs,levels=c("White", "Mixed", "Asian or Asian British", "Black or Black British", "Other"))

names(DF_plot_f)[16]="Ethnicity"

DF_plot_f$Ethnicity <- factor(DF_plot_f$Ethnicity, 
                              levels = c("Asian or Asian British",
                                         "Black or Black British",
                                         "Mixed",
                                         "Other",
                                         "White"))

### plot 
plot_ITS<-ggplot(DF_plot_f, aes(x=date, y=fit*1000/population_rounded, group=covid))+ 
  theme_bw()+ 
    #annotate(geom = "rect", xmin = as.Date("2019-12-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60", alpha=0.5)+ 
    annotate(geom = "rect", xmin = as.Date("2020-03-01"), xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey80", alpha=0.5)+ 
    #geom_point(shape=4)+   
  
  ##actual rate point
  geom_point(shape=4, aes(x=date, y=postnatal_8wk_code_present_rounded /population_rounded*1000))+ 
  geom_line(aes(y=postnatal_8wk_code_present_rounded /population_rounded*1000),color="grey")+
  
  #### prediction model  
  geom_line(color="blue")+ 
  geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population_rounded, ymax=((fit+1.96*se.fit)*1000)/population_rounded),alpha=0.2,fill="blue") +
      
  # prediction model: no covid -- counterfactual
  geom_line(aes(y=fit*1000/population_rounded,x=date),color="lightgreen",data = DF_counter)+
  geom_ribbon(aes(ymin=((fit-1.96*se.fit)*1000)/population_rounded, ymax=((fit+1.96*se.fit)*1000)/population_rounded),alpha=0.2,fill="lightgreen",data = DF_counter) +
    
 
  # group by ethnicity  
  facet_grid(rows = vars(Ethnicity),scales="free_y",labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  
  # theme
  theme_bw()+ 
  annotate(geom = "rect", xmin = as.Date("2020-03-01"),xmax = as.Date("2020-04-01"),ymin = -Inf, ymax = Inf,fill="grey60",alpha=0.5)+   
  
  # legend  
  scale_x_date(date_labels = "%m-%Y", 
               breaks = seq(as.Date("2019-01-01"), as.Date(max(DF$date)), 
                            by = "2 months"))+
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
  plot= plot_ITS,
  filename="plot_ITS_eth_1_updated.jpeg", path=here::here("output"), dpi = 300
  )

DF_plot_f <- DF_plot_f[,-c(1,3:5,9:10)]
DF_counter <- DF_counter[,-c(1,3:5,9:10)]

write.csv(DF_plot_f,here::here("output","plot_data_ITS_ethnicity2.csv"))
write.csv(DF_counter,here::here("output","plot_data_ITS_ethnicity2_counterfactual.csv"))


#### creates plot with IRRs and error bars/CIs
#df_plot_overall$Ethnicity <- factor(df_plot_overall$Ethnicity, levels = c("Ethnicity_Level_1", "Ethnicity_Level_2", ...))

plot_ITS_ethnicity2_2<-ggplot(data=df_plot_overall, aes(y=Ethnicity, x=IRR))+
  geom_point()+
  
  geom_errorbarh(aes(xmin=ci_l, xmax=ci_u))+
  
  geom_vline(xintercept=1, color="black", linetype="dashed", alpha=.5)+
  theme_bw()+
  theme(text=element_text(family="times",size=18, color="black"))+
  theme(panel.spacing = unit(1, "lines"))+
  labs(
    title = "",
    x="IRR (95% CI)",
    y=""
  )+
  
  facet_grid(Ethnicity~., scales = "free", space = "free")+
  theme(strip.text.y = element_text(angle = 0),
        axis.title.y =element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.title=element_blank(),
        legend.position="bottom")

ggsave(
  plot= plot_ITS_ethnicity2_2, 
  filename="plot_ITS_ethnicity_IRRs.jpeg", path=here::here("output"), dpi=300
)

