Code Sample Introduction:

This is a sample of the work I completed in my Machine Learning for Economists Course. This project investigates how pre-pandemic housing market tightness influenced post-pandemic home value growth across U.S. metropolitan areas. Pre-COVID tightness is measured using the Zillow Sale-to-List Ratio (SLR). The analysis combines ACS metropolitan characteristics with Zillow housing data and uses Causal Forests with honest trees to estimate heterogeneous treatment effects of housing market tightness, conditional on pre-pandemic regional characteristics.

1. Stata: Dataset construction

	—> Required Files(Omitted for size) :

		- clean_IPUMS_01.dta
		- Med_SaleList.csv
		- acs_zillow_crosswalk.csv

	—> Output 
		- cf_input_acs_valueh.csv

2. Python: Model estimation and analysis

	—> Required Files:
		- cf_input_acs_valueh.csv

	—> Output
		- cf_results_with_tau.csv
		- gate_by_slr_quartile.csv