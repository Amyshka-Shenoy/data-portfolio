install.packages("cobalt")

library(broom)
library(cluster)
library(estimatr)
library(fishmethods)
library(haven)
library(kableExtra)
library(MatchIt)
library(modelsummary)
library(pwr)
library(rddensity)
library(skimr)
library(texreg)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(foreign)
library(cobalt)

dataset<-read_dta("evaluation-2.dta")

## first, switch the dataset from long (two time periods per obs) to wide

df_w<-dataset%>%pivot_wider(names_from = round, values_from= c(health_expenditures,
                     poverty_index, age_hh, age_sp, educ_hh, educ_sp, female_hh, indigenous,
                hhsize, dirtfloor, bathroom, land, hospital_distance, hospital))%>%
  filter(!is.na(health_expenditures_0))

## check the new dataset (df_w): it has doubled the number of variables (every var, has _0 and _1 now)

# estimate propensity score models: calculate the prob of enrolment from different baseline characteristics

## first, with exact matching
exact_match<-matchit(enrolled ~ age_hh_0 + educ_hh_0 + age_sp_0 + educ_sp_0,
                     exact = ~ age_hh_0 + educ_hh_0 + age_sp_0 + educ_sp_0, data = df_w)


## if you try with many more vars (as below), you'll get the message "no matches were found"
## this means that it's impossible to find an exact match for every treated obs on so many covariates!
exact_match2<-matchit(enrolled ~ age_hh_0 + educ_hh_0 + age_sp_0 + educ_sp_0 +
                         female_hh_0 + indigenous_0 + hhsize_0 + dirtfloor_0 +
                         bathroom_0 + land_0 + hospital_distance_0, exact= ~ age_hh_0 + 
                       educ_hh_0 + age_sp_0 + educ_sp_0 +
                         female_hh_0 + indigenous_0 + hhsize_0 + dirtfloor_0 +
                         bathroom_0 + land_0 + hospital_distance_0, data = df_w)



## second, with propensity score matching
psm_r<-matchit(enrolled~age_hh_0 + educ_hh_0, data=df_w %>% 
                 dplyr::select(-hospital_0, -hospital_1), distance="glm", link="probit")


psm_ur<-matchit(enrolled~age_hh_0 + educ_hh_0 + age_sp_0 + educ_sp_0 +
                  female_hh_0 + indigenous_0 + hhsize_0 + dirtfloor_0 +
                  bathroom_0 + land_0 + hospital_distance_0, data=df_w %>% 
                  dplyr::select(-hospital_0, -hospital_1), 
                distance="glm", link="probit")

htmlreg(list(psm_r$model, psm_ur$model), file="Matchit.doc", doctype=FALSE,
        custom.coef.map=list('age_hh_0'="Age (HH) at Baseline", 'educ_hh_0'="Education (HH) at Baseline", 
                             'age_sp_0'="Age (Spouse) at Baseline", 'educ_sp_0'=
                               "Education (Spouse) at Baseline", 'female_hh_0'=
                               "Female Head of Household (HH) at Baseline", 
                             'indigenous_0'="Indigenous Language Spken at Baseline",
                             'hhsize_0'="Number of Household Members at Baseline", 
                             'dirtfloor_0'="Dirt floor at Baseline", 'bathroom_0'=
                               "Private Bathroom at Baseline", 'land_0'="Hectares of
                            Land at Baseline", 'hospital_distance_0'="Distance from
                            Hospital at Baseline"), caption="Estimating the Propensity
                            Score Based on Baseline Observed Characteristics", custom.model.names=
          c("Limited Set", "Full Set"))

## plot the distribution of the PSM by enrollment
df_w<-df_w%>%mutate(ps_r=psm_ur$model$fitted.values)

df_w%>%mutate(enrolled_lab=ifelse(enrolled==1, "Enrolled", "Not Enrolled")) %>%
  ggplot(aes(x=ps_r, group=enrolled_lab, colour=enrolled_lab, fill=enrolled_lab)) +
  geom_density(alpha=I(0.2)) + xlab("Propensity Score") + scale_fill_viridis_d("Status:", 
                                                                               end=0.7) + scale_colour_viridis_d("Status:", end=0.7) + theme(legend.position="bottom")
)


## this is a very convenient "balance" graph where you can see mean diffs btw the two groups
## the red dots tell you the distance btw the two groups before matching; the blue ones the distance after matching (ideally, it should be zero!)
love.plot(exact_match)
love.plot(psm_ur, drop.distance = TRUE)

## you can see that 'psm_ur' does pretty good without the strong restriction of exact matching


## calculate the estimated impact of the program with matching
match_df_r<-match.data(psm_r)

match_df_ur<-match.data(psm_ur)

## 'ps_r' is the propensity score matching var
mean(match_df_ur$ps_r)

## another way to show that there is balance on matching before 'treatment'
aggregate(x = ps_r ~ enrolled,
          data = match_df_ur,
          FUN = mean)

## a simple way to test if the treatment has an impact, after matching the obs
t.test(health_expenditures_1 ~ enrolled, data = match_df_r)

## finally, run the regression models
out_lm_r<-lm_robust(health_expenditures_1 ~ enrolled, data=match_df_r, 
                    clusters=locality_identifier, weights=weights)

out_lm_ur<-lm_robust(health_expenditures_1 ~ enrolled, data=match_df_ur,
                     clusters=locality_identifier, weights=weights)

summary(out_lm_r)
summary(out_lm_ur)


## now, the impact of civil war legacies on contemporary electoral outcomes in Peru

sendero<-read_dta("sendero and fujimori.dta")

## first, run a simple regression with a few controls. What's wrong with this model?
model1<-lm_robust(keiko2011 ~ difviolence + full_control + spanish  + homicidepercapita + 
                    lpop2015 + lima  , data=sendero) 
summary(model1)


## next, we want to run a matching strategy to get rid of the issues raised above. We use PSM
## the treatment must be binary; I create 'treat'='difviolence'>0
## we match on prewar levels of exclusion (spanish, distancia), and political mobilization (dummypeasant)

psm_sendero<-matchit(treat ~ spanish + dummypeasant + distancia,  data=sendero)

## check the balance of your covariates
love.plot(psm_sendero)

## create a new dataset that balances the obs on the treatment
match_sendero<-match.data(psm_sendero)

## is the new dataset balanced? 'distance' is the PSM variable in this example
aggregate(x = distance ~ treat,
          data = match_sendero,
          FUN = mean)

## finally, run the regression model

model2<-lm_robust(keiko2011 ~ treat + lima + homicidepercapita + lpop2015, data=match_sendero, weights=weights)

summary(model2)

## Interpretation? The civil war seems to have strong legacies on voting for Keiko Fujimori, even if
## we control for prewar and postwar factors, and if we use a matching strategy

## if you want to go further, try the two other DVs ('keiko2016', and 'keiko2021') to see if the effect holds over time
model3<-lm_robust(keiko2016 ~ treat + lima + homicidepercapita + lpop2015, data=match_sendero, weights=weights)

summary(model3)

model4<-lm_robust(keiko2021 ~ treat + lima + homicidepercapita + lpop2015, data=match_sendero, weights=weights)

summary(model4)

