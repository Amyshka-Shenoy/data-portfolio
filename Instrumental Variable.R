QOG <- read.csv("qog_std_ts_jan25.csv")

QOG$log_gdppc <- log(QOG$mad_gdppc)

install.packages("ivreg", dependencies = TRUE)
library(ivreg)
library(dplyr)    

QOG <- QOG %>%
  arrange(cname_qog, year) %>%
  group_by(cname_qog) %>%
  mutate(log_gdppc_lag5 = lag(log_gdppc, 5)) %>%
  ungroup()

med_data <- QOG %>%
  filter(!is.na(log_gdppc) & !is.na(log_gdppc_lag5) &
           !is.na(ht_ipolity2) & !is.na(wdi_popurb) &
           !is.na(top_top10_income_share) & !is.na(vdem_pubcorr))


first_stage <- lm(log_gdppc ~ log_gdppc_lag5 + wdi_popurb + 
                    top_top10_income_share + vdem_pubcorr, data = QOG)
summary(first_stage)   

iv_model <- ivreg(ht_ipolity2 ~ log_gdppc + wdi_popurb + 
                    top_top10_income_share + vdem_pubcorr |
                    log_gdppc_lag5 + wdi_popurb + 
                    top_top10_income_share + vdem_pubcorr,
                  data = QOG)
summary(iv_model, diagnostics = TRUE)  

install.packages("stargazer")
library(stargazer)


# Assuming your IV model is called 'iv_model'
stargazer(iv_model,
          type = "html",           
          title = "IV Regression: GDP per Capita → Polity Score",
          dep.var.labels = "Polity Score",
          covariate.labels = c("Log GDP per Capita (IV)",
                               "Urbanisation",
                               "Top 10% Income Share",
                               "Public Sector Corruption"),
          digits = 3,
          ci = TRUE,               
          ci.level = 0.95,
          single.row = TRUE,
          out = "IV_only_results.doc")  




