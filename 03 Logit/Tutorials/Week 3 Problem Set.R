rm(list=ls())
setwd("/Users/chaoyocheng/Dropbox/Github/Sandbox/Teaching/AdvQ_sandpit/Week 03/02 Exercise/Data")

library(haven)
library(readxl)
library(tidyverse)
library(stargazer)

##### Import the data #####
dta <- read_spss("ATP W99.sav")

summary(dta) # get summary statistics
head(dta) # see the first couple of rows

names(dta) # list variables
ls(dta) 

##### Codebook #####

### generate codebook from SPSS file
#var_labels <- sapply(dta, attr, "label")
#dta_codebook <- data.frame(variable = colnames(dta),
#                           label = var_labels)
#rownames(dta_codebook) <- NULL

#library(openxlsx)
#write.xlsx(dta_codebook, "Codebook.xlsx")

codebook <- read_xlsx("Codebook.xlsx")

##### Subset data #####
dta_sel <- dta |>
  dplyr::select(QKEY,
                FACEREC2_W99,
                F_METRO, F_AGECAT, F_GENDER, F_EDUCCAT, F_HISP, F_YEARSINUS,
                F_RACECMB, F_RACETHNMOD, F_RELIG, F_ATTEND, F_PARTY_FINAL, F_INC_SDT1,
                F_REG, F_IDEO,
                WEIGHT_W99, F_CREGION, F_CDIVISION) |>
  mutate(yes_fr = if_else(FACEREC2_W99 == 1, 1, 0),
         metro = if_else(F_METRO == 1, 1, 0),
         race_white = if_else(F_RACECMB == 1, 1, 0),
         woman = if_else(F_GENDER == 3, 1, 0)) |>
  unique()

##### Task 1: Logit with no predictor #####
mod_intercept <- glm(yes_fr ~ 1,
                     data = dta_sel,
                     family = binomial)
summary(mod_intercept)

coefs <- coef(mod_intercept) # extracting log odds
exp(coefs) # calculating odds
exp(coefs)/(1+exp(coefs)) # calculating p
mean(dta_sel$yes_fr, na.rm=T) # obtaining p using mean

confint(mod_intercept) # CI of log odds
exp(confint(mod_intercept))/(1+exp(confint(mod_intercept)))

##### Task 2: Logit with one predictor #####
mod_frec <- glm(yes_fr ~ race_white, data = dta_sel, family = binomial)
summary(mod_frec)
ls(mod_frec)

coef(mod_frec)
exp(coef(mod_frec)) # odds ratio

confint(mod_frec)
exp(confint(mod_frec)) # CIs of odds ratio

##### Extra: Survey weights #####
library(survey)
library(srvyr)

### Create the survey design object
dta_sel_survey <- dta_sel %>%
  as_survey(ids = QKEY,
            #strata = F_CDIVISION,
            #fpc = population,
            weights = WEIGHT_W99)
dta_sel_survey

### Fit logit with svyglm
mod_s_frec <- svyglm(yes_fr ~ race_white,
                     design = dta_sel_survey, # the survey design object
                     family = binomial)
summary(mod_s_frec)
exp(coef(mod_s_frec)) # odds ratio

##### Extra: Predicted probabilities #####
data_predict <- data.frame(race_white = c(0,1))
data_predict

fit_prob <- predict(mod_frec, newdata=data_predict, type="response")
fit_log_odds <- predict(mod_frec, newdata=data_predict)

fit_mod_s <- data.frame(race_white = c(0:1),
                        fit_prob = as.matrix(fit_prob),
                        fit_log_odds = as.matrix(fit_log_odds))
fit_mod_s$fit_odds <- exp(fit_mod_s$fit_log_odds)
fit_mod_s

##### Extra: Logit with more than one predictor #####
dta_sel <- dta_sel %>%
  dplyr::select(yes_fr, race_white, metro, woman) %>%
  drop_na()

mod1 <- glm(yes_fr ~ race_white, data = dta_sel, family = binomial)
mod2 <- glm(yes_fr ~ race_white + metro, data = dta_sel, family = binomial)
mod3 <- glm(yes_fr ~ race_white + metro + woman, data = dta_sel, family = binomial)

stargazer(list(mod1, mod2, mod3),
          omit.stat = c("f", "rsq", "ser"),
          covariate.labels = c("White (=1)",
                               "Metropolitan (=1)",
                               "Female (=1)"),
          type = "text",
          digits = 3, 
          no.space = T,
          intercept.bottom = TRUE,
          star.cutoffs = c(0.05, 0.01, 0.001))

## Sum of squared residuals
sum(residuals(mod1)^2)
sum(residuals(mod2)^2)
sum(residuals(mod3)^2)

## Model comparison
anova(mod1, mod2, mod3, test="Chi")

## Simulated probabilities
data_predict <- data.frame(race_white = c(0,1),
                           metro = mean(dta_sel$metro, na.rm=T),
                           woman = mean(dta_sel$woman, na.rm=T))
data_predict

fit_prob <- predict(mod3, newdata=data_predict, type="response")
fit_log_odds <- predict(mod3, newdata=data_predict)

fit_mod_s <- data.frame(ukrspeakhome = c(0,1),
                        fit_prob = as.matrix(fit_prob),
                        fit_log_odds = as.matrix(fit_log_odds))
fit_mod_s$fit_odds <- exp(fit_mod_s$fit_log_odds)
fit_mod_s
