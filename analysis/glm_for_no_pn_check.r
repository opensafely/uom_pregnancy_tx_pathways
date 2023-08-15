library('tidyverse')


setwd(here::here("output", "pn8wk"))
df_input<-list.files(pattern = "input", full.names = FALSE) %>% lapply(read.csv, stringsAsFactors=F) %>% bind_rows()
# population with delivery codes
df_input<- df_input %>% filter(delivery_code_number >0)


## dependant var is if they had a pn check or not. 
df_input$postnatal_8wk_code_present <- as.factor(df_input$postnatal_8wk_code_present)
df_input$age_cat<-as.factor(df_input$age_cat)


## simple model 
mod1 <- glm(postnatal_8wk_code_present~age_cat, family=binomial(link=logit), data=df_input)
#summary(mod1)
mod1df<- as.data.frame(mod1$coefficients)
df_capture <-as.data.frame(capture.output(summary(mod1), 'output.txt'))

write_csv(mod1df, here::here("output","mod1_ageadj_coef.csv"))
write_csv(df_capture, here::here("output","mod1_ageadj_capture.csv"))


## Adjusted model 
df_input$ethnicity<-as.factor(df_input$ethnicity)
df_input$ethnicity <- relevel(df_input$ethnicity, "White")
df_input$region<-as.factor(df_input$region)
df_input$region <- relevel(df_input$region, "London")
df_input$imd<-as.factor(df_input$imd) 
#df$imd <- relevel(df$imd, 1) need least deprived as reference cat. 

mod2 <- glm(postnatal_8wk_code_present~age_cat+
              ethnicity+region+imd+bmi, family=binomial(link=logit), data=df_input)
#summary(mod2)
mod2df<- as.data.frame(mod2$coefficients)
df2_capture <-as.data.frame(capture.output(summary(mod2), 'output.txt'))

write_csv(mod2df, here::here("output","mod2_fulladj_coef.csv"))
write_csv(df2_capture, here::here("output","mod2_fulladj_capture.csv"))

##compare with glmer() repeated delcodes per mum.. 
##or select one row per mum as sensitivity.

