Code Sample Introduction

The project measures pre-COVID housing market tightness using the Zillow Sale-to-List Ratio (SLR) and combines Zillow housing data with metropolitan characteristics derived from ACS data. The final modeling dataset is then used to estimate heterogeneous relationships using causal forest methods. The goal is to examine whether the relationship between pre-pandemic housing tightness and subsequent home value growth varied systematically across metropolitan areas with different pre-pandemic characteristics.

This code sample is a two-stage empirical workflow in Stata and Python. Data cleaning and construction began in Stata, and analysis was completed in Python.

Research Workflow

1. Stata: Dataset construction

This stage constructs the metropolitan-level modeling dataset used in estimation.

Required files :
- clean_IPUMS_01.dta (omitted here for size)
- Med_SaleList.csv
- acs_zillow_crosswalk.csv

Code:
- clean and aggregate ACS data
- merge Zillow housing indicators
- apply metropolitan crosswalks
- construct metropolitan covariates
- export the final modeling file

Output:
- cf_input_acs_valueh.csv

2. Python: Model estimation and analysis

This stage uses the final modeling dataset to estimate heterogeneous treatment effects and summarize results.

Required file:
- cf_input_acs_valueh.csv

Code:
- define treatment, outcome, and covariates
- estimate the causal forest model
- generate unit-level treatment effect estimates
- compute grouped summaries of treatment effects

Outputs:
- cf_results_with_tau.csv: metropolitan-level treatment effect estimates
- gate_by_slr_quartile.csv: grouped average treatment effects by SLR quartile
