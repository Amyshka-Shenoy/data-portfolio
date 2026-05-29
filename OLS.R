QOG <- read.csv("qog_std_ts_jan25.csv")

QOG$log_gdppc <- log(QOG$mad_gdppc)

install.packages("skimr")
library(skimr)
skim(QOG[, c("ht_ipolity2",
             "log_gdppc",
             "wdi_popurb",
             "wdi_gerpf",
             "top_top10_income_share",
             "vdem_pubcorr")])

sum(complete.cases(QOG[, c('ht_ipolity2','log_gdppc','wdi_popurb','wdi_gerpf','top_top10_income_share','vdem_pubcorr')]))

library(estimatr)
library(texreg)

m_ols_naive <- lm_robust(ht_ipolity2 ~ log_gdppc, data = QOG)

summary(m_ols_naive)


m_ols_extended <- lm_robust(
  ht_ipolity2 ~ log_gdppc + wdi_popurb +wdi_gerpf + top_top10_income_share +vdem_pubcorr,
  data = QOG
)

summary(m_ols_extended)

htmlreg(
  list(m_ols_naive, m_ols_extended),
file = "OLS_comparison.doc",
doctype= FALSE,
custom.model.names = c("Naive OLS", "Extended OLS"),
custom.coef.map = list(
  'log_gdppc' = "Log GDP per Capita",
  'wdi_popurb' = "Urbanisation",
  'wdi_gerpf' = "Female School Enrollment",
  'top_top10_income_share' = "Top 10% Income Share",
  'vdem_pubcorr' = "Public Sector Corruption",
  '(Intercept)' = "Intercept"
),
caption = "OLS Regressions : Modernisation and Democracy",
caption.above = TRUE
)
