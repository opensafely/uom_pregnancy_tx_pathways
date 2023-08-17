library('tidyverse')


setwd(here::here("output", "pn8wk"))
df_input<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

#col_types = cols(patient_id = col_integer(),delivery_code_number = col_number(), postnatal_8wk_code_present = col_number())

# no scientific numbers on plots
options(scipen = 999999)

# population with delivery codes
df_input<- df_input %>% filter(delivery_code_number >0)

df_grp <- df_input %>% group_by(patient_id) %>%
  summarise(total_del_codes = sum(delivery_code_number))

df_grpKeep <- df_input %>% group_by(patient_id) %>%
  summarise(total_del_codes = sum(delivery_code_number), .groups="keep")


## num unique patients with delivery codes. 
num_pats <- df_input %>%
  distinct(patient_id) %>%
  n_distinct()

num_pats_rd<- round(num_pats/5)*5

plot_delcode_freq_by_patient1 <- ggplot(data=df_grp, aes(total_del_codes)) +
  geom_histogram() +
  labs (title = "Total Number of Delivery Codes by Patient",
        caption = paste("Data from approximately", num_pats_rd,"patients*"),
        x = "No. of delivery codes")
ggsave(
  plot= plot_delcode_freq_by_patient,
  filename="del_code_histogram_reviewed_byPatient.jpeg", path=here::here("output"),
)


quant_del_codes<- as.data.frame(quantile(df_grp$total_del_codes))
quant_del_codes$quant <- c(0,25,50,75,100)
write_csv(quant_del_codes, here::here("output","del_code_quantiles_by_patients.csv"))


## using same data of delivered women, for the individuals how many pn codes
df_grp_pn <- df_input %>% group_by(patient_id) %>%
  summarise(total_pn_codes = sum(pn8wk_code_number))

## num unique patients with pn codes. 
num_pats_pn <- filter(df_grp_pn, total_pn_codes >0) %>%
  distinct(patient_id) %>%
  n_distinct()

num_pats_pn_rd<- round(num_pats_pn/5)*5

plot_pn_freq_by_patient <- ggplot(data=df_grp_pn, aes(total_pn_codes)) +
  geom_histogram() +
  labs (title = "Total Number of PN Codes for Patient with Delivery Code",
        caption = paste("Data from approximately", num_pats_pn_rd,"patients*"),
        x = "No. of PN codes")
ggsave(
  plot= plot_pn_freq_by_patient,
  filename="pn_code_histogram_reviewed_byPatient.jpeg", path=here::here("output"),
)

quant_pn_codes<- as.data.frame(quantile(df_grp_pn$total_pn_codes))
quant_pn_codes$quant <- c(0,25,50,75,100)
write_csv(quant_pn_codes, here::here("output","pn_code_quantiles_by_patients.csv"))

################
## what are the most common codes? 
## overall or by mum? 

## overall del
table_del_codes <- as.data.frame(table(df_input$delivery_code)) 
table_del_codes <- table_del_codes[order(-table_del_codes$Freq),]
str(table_del_codes)
# redact small counts
table_del_codes$Freq <- as.numeric(table_del_codes$Freq)
#table_del_codes$Freq <- ifelse(table_del_codes$Freq < 10, "Redacted", table_del_codes$Freq)
write_csv(table_del_codes, here::here("output","table_del_codes_reviewed_overall.csv"))


## overall pn
# filter to rows with pn codes
df_input<- df_input %>% filter(pn8wk_code_number >0)
table_pn_codes <- as.data.frame(table(df_input$postnatal_code)) 
table_pn_codes <- table_pn_codes[order(-table_pn_codes$Freq),]
# redact small counts
table_pn_codes$Freq <- as.numeric(table_pn_codes$Freq)
#table_pn_codes$Freq <- ifelse(table_pn_codes$Freq < 10, "Redacted", table_pn_codes$Freq)
write_csv(table_pn_codes, here::here("output","table_pn_codes_reviewed_overall.csv"))

######### merge with code names
## read in codelist csvs

#load delivery codes
df_del <- read_csv(
  here::here("codelists", "user-VickiPalin-pregnancy_delivery_snomed_reviewed_2.csv"),
  col_types = cols(code = col_number(),term = col_character())
)

#load pn codes
df_pn <- read_csv(
  here::here("codelists", "user-VickiPalin-pregnancy_postnatal_8wk_snomed_reviewed.csv"),
  col_types = cols(code = col_number(),term = col_character())
)



### rename Var1 to code to match in codelist csv.

#rename first column in frequency table to match
colnames(table_del_codes)[1]<-"code"
colnames(table_pn_codes)[1]<-"code"

#merge by code number to return full description of codes which are in both files
table_del_codes_merged<-merge(table_del_codes, df_del, by="code")
table_del_codes_merged <- table_del_codes_merged[order(-table_del_codes_merged$Freq),]

table_pn_codes_merged<-merge(table_pn_codes, df_pn, by="code")
table_pn_codes_merged<- table_pn_codes_merged[order(-table_pn_codes_merged$Freq),]


#######
####### changes counts < 6 to "[REDACTED]"
table_del_codes_mergedR <- table_del_codes_merged 
#table_del_codes_mergedR$Freq <- ifelse(table_del_codes_mergedR$Freq <= 7, "[REDACTED]", table_del_codes_mergedR$Freq)
table_del_codes_mergedR$Freq <- as.numeric(table_del_codes_mergedR$Freq)
#rounding to nearest 5
table_del_codes_mergedR$Freq <- round(table_del_codes_mergedR$Freq/5)*5
## now save merged with names and redacted. 
write_csv(table_del_codes_mergedR, here::here("output","table_del_codes_mergedR.csv"))

####### changes counts < 6 to "[REDACTED]"
table_pn_codes_mergedR <- table_pn_codes_merged 
#table_pn_codes_mergedR$Freq <- ifelse(table_pn_codes_mergedR$Freq <= 7, "[REDACTED]", table_pn_codes_mergedR$Freq)
table_pn_codes_mergedR$Freq <- as.numeric(table_pn_codes_mergedR$Freq)
#rounding to nearest 5
table_pn_codes_mergedR$Freq <- round(table_pn_codes_mergedR$Freq/5)*5
## now save merged with names and redacted. 
write_csv(table_pn_codes_mergedR, here::here("output","table_pn_codes_mergedR.csv"))


# already saved histograms and quantiles:
# filename="del_code_histogram_reviewed_byPatient.jpeg"
# "del_code_quantiles_by_patients.csv"
# filename="pn_code_histogram_reviewed_byPatient.jpeg"
# "pn_code_quantiles_by_patients.csv"
## already saved overall codes without descriptions
# "table_del_codes_reviewed_overall.csv"
# "table_pn_codes_reviewed_overall.csv"



