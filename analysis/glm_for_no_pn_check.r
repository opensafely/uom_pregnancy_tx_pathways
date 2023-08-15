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
#df_capture <-as.data.frame(capture.output(summary(mod1), 'output.txt'))

write_csv(mod1df, here::here("output","mod1_ageadj_coef.csv"))
#write_csv(df_capture, here::here("output","mod1_ageadj_capture.csv"))


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
#df2_capture <-as.data.frame(capture.output(summary(mod2), 'output.txt'))

write_csv(mod2df, here::here("output","mod2_fulladj_coef.csv"))
#write_csv(df2_capture, here::here("output","mod2_fulladj_capture.csv"))


# get coefs and CIs and plot
library(broom)
tidy <- as.data.frame(mod2$coefficients)
#head(tidy)
#View(tidy)

var.diag <- diag(vcov(mod2))#### use one of the models
tidy$OR <- exp(tidy$`mod2$coefficients`)
tidy$Oddstd <- sqrt((tidy$OR^2)*var.diag)
tidy$LOW <- tidy$OR-(1.96*tidy$Oddstd)
tidy$HIGH <- tidy$OR+(1.96*tidy$Oddstd)
write_csv(tidy, here::here("output","mod2_tidy_OR_CI.csv"))



#head(tidy)
library(forestplot)

plottext<- row.names(tidy)
forestplot(labeltext=plottext, mean = c(tidy$OR), lower = c(tidy$LOW), upper = c(tidy$HIGH), ci.vertices = TRUE,
           ci.vertices.height = 0.2, col=fpColors(box= "royalblue", line="darkblue", zero="gray", hrz_lines="black"),
           title="Odds Ratio [95% CI]", txt_gp=fpTxtGp(label=gpar(cex=1.2), ticks=gpar(cex=1.1)),
           grid=TRUE, boxsize=0.3, zero=1, xticks=c(0.5, 0.75, 1.0,1.5,2.0), lwd.ci=1)




##compare with glmer() repeated delcodes per mum.. 
##or select one row per mum as sensitivity.

