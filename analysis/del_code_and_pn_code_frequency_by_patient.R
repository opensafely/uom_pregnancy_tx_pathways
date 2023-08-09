library('tidyverse')


setwd(here::here("output", "pn8wk"))
df_input<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()

#col_types = cols(patient_id = col_integer(),delivery_code_number = col_number(), postnatal_8wk_code_present = col_number())

# population with delivery codes
df_input<- df_input %>% filter(delivery_code_number >0)

df_grp <- df_input %>% group_by(patient_id) %>%
  summarise(total_del_codes = sum(delivery_code_number))

## num unique patients with delivery codes. 
num_pats <- df_input %>%
  distinct(patient_id) %>%
  n_distinct()

num_pats_rd<- round(num_pats/5)*5

plot_delcode_freq_by_patient <- ggplot(data=df_grp, aes(total_del_codes)) +
  geom_histogram() +
  labs (title = "Total Number of Delivery Codes by Patient",
        caption = paste("Data from approximately", num_pats_rd,"patients"),
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
        caption = paste("Data from approximately", num_pats_pn_rd,"patients"),
        x = "No. of PN codes")
ggsave(
  plot= plot_delcode_freq_by_patient,
  filename="pn_code_histogram_reviewed_byPatient.jpeg", path=here::here("output"),
)

quant_pn_codes<- as.data.frame(quantile(df_grp_pn$total_pn_codes))
quant_pn_codes$quant <- c(0,25,50,75,100)
write_csv(quant_pn_codes, here::here("output","pn_code_quantiles_by_patients.csv"))

################
