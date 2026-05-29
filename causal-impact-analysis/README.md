# Causal Impact Evaluation Using Observational Data (R)

## Overview

This project applies causal inference techniques to evaluate the impact of a program/intervention using observational panel data. It focuses on addressing selection bias and estimating treatment effects using propensity score matching.

## Methods Used

* Data restructuring from long to wide format
* Propensity score estimation (logistic/probit models)
* Exact matching and nearest-neighbour matching
* Covariate balance diagnostics (Love plots, mean comparison tests)
* Regression analysis on matched samples
* Robustness checks across multiple time periods

## Key Techniques

* Propensity Score Matching (PSM)
* Causal inference using observational data
* Balance diagnostics and covariate adjustment
* Panel-style outcome comparison over time

## Key Insights

* Matching improves comparability between treated and control groups by reducing selection bias
* Naïve regression models may produce biased estimates without adjusting for observable differences
* Treatment effects vary over time, highlighting the importance of longitudinal analysis

## Skills Demonstrated

* R programming (tidyverse, MatchIt, estimatr)
* Causal inference methodology
* Data cleaning and transformation
* Statistical modelling and interpretation
* Policy-relevant impact evaluation
