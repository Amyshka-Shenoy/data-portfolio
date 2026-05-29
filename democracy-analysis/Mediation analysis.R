QOG <- read.csv("qog_std_ts_jan25.csv")

QOG$log_gdppc <- log(QOG$mad_gdppc)

install.packages("mediation")
install.packages("sandwich")
install.packages("lmtest")

library(mediation)
library(sandwich)
library(lmtest)
library(tidyverse)

med_data <- QOG %>%
  dplyr::select(
    ht_ipolity2,
    log_gdppc,
    wdi_gerpf,
    wdi_popurb,
    top_top10_income_share,
    vdem_pubcorr
  ) %>%
  na.omit()

model.m <- lm(
  wdi_gerpf ~ log_gdppc + wdi_popurb + top_top10_income_share + vdem_pubcorr,
  data = med_data
)

summary(model.m)

model.y <- lm(
  ht_ipolity2 ~ log_gdppc + wdi_gerpf + wdi_popurb +
    top_top10_income_share + vdem_pubcorr,
  data = med_data
)

summary(model.y)

med.out <- mediate(
  model.m,
  model.y,
  treat = "log_gdppc",
  mediator = "wdi_gerpf",
  sims = 1000,
  boot = TRUE
)

summary(med.out)

install.packages("dplyr")
install.packages("knitr")

library(dplyr)
library(knitr)
library(kableExtra)

med_table <- data.frame(
  Effect = c("ACME (Indirect effect)", "ADE (Direct effect)", "Total Effect", "Proportion Mediated"),
  Estimate = c(med.out$d0, med.out$z0, med.out$tau.coef, med.out$n0),
  CI_Lower = c(med.out$d0.ci[1], med.out$z0.ci[1], med.out$tau.ci[1], med.out$n0.ci[1]),
  CI_Upper = c(med.out$d0.ci[2], med.out$z0.ci[2], med.out$tau.ci[2], med.out$n0.ci[2]),
  p_value = c(med.out$d0.p, med.out$z0.p, med.out$tau.p, med.out$n0.p)
)

# Print table
kable(med_table, caption = "Mediation Analysis: GDP per Capita → Female Education → Democracy")
