# Does Modernization Impact Democratization?

### A Causal Analysis Using R (OLS, Mediation, IV, and Matching)

## Overview

This project investigates whether economic modernization, measured through GDP per capita, has a causal impact on democratization. The analysis combines multiple econometric techniques to strengthen causal inference and address key identification challenges such as reverse causality, omitted variable bias, and selection bias.

The dataset spans country-year observations from 1972–2014 and draws on multiple sources including the Quality of Government dataset, World Development Indicators, Maddison Project Database, Varieties of Democracy, and other cross-national political economy datasets.

---

## Research Question

Does modernization (GDP per capita) have a positive causal effect on democratization?

## Hypothesis

Higher GDP per capita leads to higher levels of democratization, consistent with modernization theory.

---

## Methodology

To strengthen causal interpretation, the project applies four complementary identification strategies:

### 1. Ordinary Least Squares (OLS) Regression

* Baseline and extended models estimating the relationship between GDP per capita and democracy scores
* Controls include:

  * Urbanization
  * Female education
  * Income inequality
  * Corruption
* Used to assess sensitivity to omitted variable bias

### 2. Mediation Analysis

* Tests whether the effect of GDP per capita operates through **female education**
* Estimates direct and indirect effects of modernization on democratization

### 3. Instrumental Variable (IV) Approach

* Uses 5-year lagged GDP per capita as an instrument for current income
* Addresses reverse causality concerns
* First-stage regression confirms strong predictive power of the instrument

### 4. Matching (Propensity Score Style Design)

* Countries classified as “treated” if GDP per capita is above the global median
* Matching used to reduce selection bias and improve comparability between groups
* Covariate balance assessed using standard diagnostics

---

## Key Variables

* **Modernization (Treatment):** Log GDP per capita
* **Democratization (Outcome):** Imputed Polity score (Polity + Freedom House integration)
* **Mediator:** Female education
* **Instrument:** 5-year lagged GDP per capita
* **Unit of analysis:** Country-year (1972–2014)

---

## Key Findings

* GDP per capita is positively associated with democratization in baseline models
* The relationship weakens significantly once controls are included, suggesting strong confounding factors
* Mediation analysis suggests a large proportion of the effect operates indirectly through **female education**
* IV estimates confirm a positive causal effect, though larger than OLS estimates, suggesting attenuation bias in baseline models
* Matching results confirm robustness, showing higher-income countries exhibit higher democracy scores even after covariate adjustment

---

## Interpretation

The results suggest that modernization does not operate as a purely direct driver of democratization. Instead, its effects appear to be **indirect and mechanism-driven**, particularly through improvements in education and broader structural development.

While all methods consistently indicate a positive relationship between income and democracy, the strength and pathway of this relationship depend heavily on model specification and identification strategy.

---

## Limitations

* GDP per capita is an imperfect proxy for modernization
* Democracy indices (Polity/Freedom House) contain measurement and coding limitations
* IV assumptions (especially exclusion restriction) cannot be fully tested
* Matching approach relies on observed covariates and cannot eliminate unobserved bias

---

## Skills Demonstrated

* Causal inference in observational data
* OLS regression and multivariate modelling
* Instrumental variables (two-stage estimation)
* Mediation analysis
* Propensity score matching
* Data cleaning and transformation in R
* Interpretation of econometric results
* Policy-relevant communication of quantitative findings
