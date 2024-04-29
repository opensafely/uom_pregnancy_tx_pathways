## Import libraries---
library("tidyverse") 
library('dplyr')
library('lubridate')

rm(list=ls())
setwd(here::here("output", "joined_12wk"))

# file list
csvFiles_19 = list.files(pattern="input_updated_2019", full.names = FALSE)
csvFiles_20 = list.files(pattern="input_updated_2020", full.names = FALSE)
csvFiles_21 = list.files(pattern="input_updated_2021", full.names = FALSE)
csvFiles_22 = list.files(pattern="input_updated_2022", full.names = FALSE)
csvFiles_23 = list.files(pattern="input_updated_2023", full.names = FALSE)


# date list
date_19= seq(as.Date("2019-01-01"), as.Date("2019-12-01"), "month")
date_20= seq(as.Date("2020-01-01"), as.Date("2020-12-01"), "month")
date_21= seq(as.Date("2021-01-01"), as.Date("2021-12-01"), "month")
date_22= seq(as.Date("2022-01-01"), as.Date("2022-12-01"), "month")
date_23= seq(as.Date("2023-01-01"), as.Date("2023-12-01"), "month")


temp <- vector("list", length(csvFiles_19))
for (i in seq_along(csvFiles_19)){
  temp_df <- read_csv((csvFiles_19[i]))#,

  temp_df$date=date_19[i]

  #add df to list
  temp[[i]] <- temp_df
  rm(temp_df)
}

dat=dplyr::bind_rows(temp)
rm(temp,i,date_19,csvFiles_19)

dat$date <- as.Date(dat$date)

saveRDS(dat, "basic_joined_12wk_records_2019.rds")
rm(dat)

temp <- vector("list", length(csvFiles_20))
for (i in seq_along(csvFiles_20)){
   temp_df <- read_csv((csvFiles_20[i]))#,
 
  temp_df$date=date_20[i]
  
  #add df to list
  temp[[i]] <- temp_df
  rm(temp_df)
}

dat=dplyr::bind_rows(temp)
rm(temp,i,date_20,csvFiles_20)

dat$date <- as.Date(dat$date)

saveRDS(dat, "basic_joined_12wk_records_2020.rds")
rm(dat)

temp <- vector("list", length(csvFiles_21))
for (i in seq_along(csvFiles_21)){
  temp_df <- read_csv((csvFiles_21[i]))#,
  #                     col_types = cols_only(
  
  temp_df$date=date_21[i]
  
  #add df to list
  temp[[i]] <- temp_df
  rm(temp_df)
}

dat=dplyr::bind_rows(temp)
rm(temp,i,date_21,csvFiles_21)

dat$date <- as.Date(dat$date)

saveRDS(dat, "basic_joined_12wk_records_2021.rds")
rm(dat)

temp <- vector("list", length(csvFiles_22))
for (i in seq_along(csvFiles_22)){
  temp_df <- read_csv((csvFiles_22[i]))#,
 
  temp_df$date=date_22[i]
  
  #add df to list
  temp[[i]] <- temp_df
  rm(temp_df)
}

dat=dplyr::bind_rows(temp)
rm(temp,i,date_22,csvFiles_22)

dat$date <- as.Date(dat$date)

saveRDS(dat, "basic_joined_12wk_records_2022.rds")
rm(dat)


##2023
temp <- vector("list", length(csvFiles_23))
for (i in seq_along(csvFiles_23)){
  temp_df <- read_csv((csvFiles_23[i]))#,
 
  temp_df$date=date_23[i]
  
  #add df to list
  temp[[i]] <- temp_df
  rm(temp_df)
}

dat=dplyr::bind_rows(temp)
rm(temp,i,date_23,csvFiles_23)

dat$date <- as.Date(dat$date)

saveRDS(dat, "basic_joined_12wk_records_2023.rds")
rm(dat)
