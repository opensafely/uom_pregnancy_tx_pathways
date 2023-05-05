library("ggplot2")
library("data.table")
library("dplyr")
library("tidyverse")
library("MASS")
#library(modelsummary)
#library("gtsummary")

## Import data
df = read.csv(here::here("output", "measures", "measure_postnatal_check_rate.csv")),

# for measures overall,
# or do we want to bind age_cat, eth, etc files

delivery_code_present  = col_double(),
postnatal_8wk_code_present = col_double(),
population  = col_number(),
value = col_number(),

df$date <- as.Date(df$date)
#df$month= format(df$date,"%m")

# creates time since start of study period as 1-52
df$times <- as.numeric(as.factor(df$date))

## define dates
# start of study period, start of lockdown, end of lockdown, end of study?
breaks <- c(as.Date("2019-01-01"), as.Date("2020-03-01"), max(df$date))

df=df%>%mutate(covid=cut(date,breaks,labels = 1:2))

## gives a covid column with 1-2
## as we dont want to exclude any months?
# 1 = before march 2020
# 2 = march 2020 onwards

df=df%>% filter(covid==1 | covid==2)
df$covid= recode(df$covid, '1'="0", '2'="1")
df$covid <- factor(df$covid, levels=c("0","1"))

## creates time.since
# as time since start of obs period
# all covid = 0 are 0
# covid = 1 are 1 to 476 (each row a new no) - is this right?
df=df%>% group_by(covid)%>%mutate(time.since=1:n())
df$time.since <- ifelse(df$covid==0,0,df$time.since)

# do we need this?
# df$value=1-df$percentage
# df$count= df$total-df$count

## new vars so far
# times (months since start of study)
# covid (binary)
# time.since (months since covid)

## next steps
# filter by age_cat? to make split dfs
#this currently loads in overall measures file