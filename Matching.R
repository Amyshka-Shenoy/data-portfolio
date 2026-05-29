QOG <- read.csv("qog_std_ts_jan25.csv")

QOG$log_gdppc <- log(QOG$mad_gdppc)

install.packages(c("MatchIt", "cobalt", "estimatr", "tidyverse", "kableExtra"))

library(MatchIt)       
library(cobalt)        
library(estimatr)      
library(tidyverse)    
library(kableExtra)   

QOG <- QOG %>% 
  mutate(treat_gdp = ifelse(log_gdppc > median(log_gdppc, na.rm=TRUE), 1, 0))

QOG_clean <- QOG %>%
  filter(!is.na(treat_gdp),
         !is.na(wdi_popurb),
         !is.na(top_top10_income_share),
         !is.na(wdi_gerp))

psm_lit <- matchit(
  treat_gdp ~ wdi_popurb + top_top10_income_share + wdi_gerp,
  data = QOG_clean,
  method = "nearest",
  distance = "logit",
  caliper = 0.2
)

summary(psm_lit)

love.plot(
  psm_lit,
  stats = "mean.diffs",
  threshold = 0.1,
  abs = TRUE
)

match_df <- match.data(psm_lit)

aggregate(distance ~ treat_gdp, data = match_df, mean)

model_match <- lm_robust(
  ht_ipolity2 ~ treat_gdp,
  data = match_df,
  weights = weights
)

summary(model_match)

library(modelsummary)

out_lm <- lm_robust(
  ht_ipolity2 ~ treat_gdp,
  data = match_df,
  weights = weights
)
install.packages("pandoc")
library(pandoc)


modelsummary(
  list("Matched Sample" = out_lm),
  stars = TRUE,
  statistic = "std.error",
  output = "matched_results.docx"
)

