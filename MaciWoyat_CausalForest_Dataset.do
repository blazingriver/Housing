* Insert global path and directory as needed*
global root "."
cd "$root/."

use clean_IPUMS_01.dta
*---------------------------------------------------------------------*
preserve
keep if year>=2013
collapse (sum) pop = perwt, by(met2013 year)

save metro_tables_2013, replace

egen city_size = xtile(pop), n(4)
tab city_size
tab year city_size
summ pop, detail

save metro2013_size_groups, replace
restore

merge m:1 met2013 year using metro2013_size_groups, nogen keep(match)
save "IPUMS_01_CityGroups.dta", replace
* Show total observations per city_size group
tab city_size


* Show number of unique met2013 per city_size group
bys city_size: egen n_metros = tag(met2013)
bys city_size: egen unique_metros = total(n_metros)
bys city_size: keep if _n == 1
list city_size unique_metros, noobs

use IPUMS_01_CityGroups.dta

twoway ///
    (histogram hhincome if city_size==1, width(25000) color(blue%40)) ///
    (histogram hhincome if city_size==2, width(25000) color(green%40)) ///
    (histogram hhincome if city_size==3, width(25000) color(orange%40)) ///
    (histogram hhincome if city_size==4, width(25000) color(red%40)) ///
, legend(order(1 "Small" 2 "Mid-Small" 3 "Mid-Large" 4 "Large")) ///
  title("Distribution of Household income by City Size") ///
  xtitle("House value ($)") ///
  ytitle("Density") ///
    xlabel(, format(%9.0gc)) ///
  graphregion(color(white))

graph export "$root/Data/IncomeDist_unclean.pdf", replace

**A lot of outliers at the high end of income

* 1. Compute mean and SD for household income
preserve
summ hhincome
local mu = r(mean)
local sd = r(sd)

* 2. Drop outliers beyond +/- 2 SD
drop if hhincome < `mu' - 1.75*`sd' | hhincome > `mu' + 1.75*`sd'

twoway ///
    (histogram hhincome if city_size==1, width(25000) color(blue%40)) ///
    (histogram hhincome if city_size==2, width(25000) color(green%40)) ///
    (histogram hhincome if city_size==3, width(25000) color(orange%40)) ///
    (histogram hhincome if city_size==4, width(25000) color(red%40)) ///
, legend(order(1 "Small" 2 "Mid-Small" 3 "Mid-Large" 4 "Large")) ///
  title("Distribution of Household Income by City Size (within 2 SD)") ///
  xtitle("Household income ($)") ///
  ytitle("Density") ///
  graphregion(color(white))
  
  graph export "$root/Data/IncomeDist_clean.pdf", replace
  restore

*******************************************************************************
****-----				Treatment Variable (SLR)					  -----****
*******************************************************************************


*--------------------------------------------------------------------*
* 1. Import Zillow Sale-to-List Ratio CSV
*--------------------------------------------------------------------*

import delimited "$root/Data/Med_SaleList.csv", clear

keep regionid sizerank regionname regiontype statename v*

*--------------------------------------------------------------------*
* 2. Extract year and month from variable labels
*--------------------------------------------------------------------*

ds v*
tempfile yearmap
postfile handle int monthnum int year using `yearmap'

local i = 1
foreach var of varlist v* {
    local lbl : variable label `var'
    local yr  = real(substr("`lbl'",1,4))     // first 4 chars = year
    post handle (`i') (`yr')
    local ++i
}
postclose handle

*--------------------------------------------------------------------*
* 3. Reshape wide → long
*--------------------------------------------------------------------*

reshape long v, i(regionid) j(monthnum)
rename v ratio

merge m:1 monthnum using `yearmap', nogen keep(match)

*--------------------------------------------------------------------*
* 4. Collapse to annual average SLR
*--------------------------------------------------------------------*

collapse (mean) avgratio = ratio ///
         (first) sizerank regionname regiontype statename, ///
         by(regionid year)

save region_annual_avg, replace

*--------------------------------------------------------------------*
* 5. Create PRE-COVID Tightness (2018–2019)
*--------------------------------------------------------------------*

preserve
    keep if inrange(year, 2018, 2019)

    collapse (mean) pre_covid_ratio = avgratio ///
             (first) sizerank regionname regiontype statename, ///
             by(regionid)

    save pre_covid_ratio, replace
restore

*--------------------------------------------------------------------*
* 6. Inspect Distribution of Pre-COVID Tightness
*--------------------------------------------------------------------*

use pre_covid_ratio, clear

* Summary statistics
summarize pre_covid_ratio

* Histogram
histogram pre_covid_ratio, width(.01) ///
    xtitle("Pre-COVID Sale-to-List Ratio (2018-2019)") ///
    title("Distribution of Pre-COVID Market Tightness")
	
graph export "SLR_Dist_Histogram.png", replace

* Kernel density
kdensity pre_covid_ratio, ///
    xtitle("Pre-COVID Sale-to-List Ratio") ///
    title("Density of Pre-COVID Tightness")

graph export "SLR_Dist_Kernel.png", replace

*--------------------------------------------------------------------*
* 7. Create Quantiles or Categories (optional)
*--------------------------------------------------------------------*

xtile tight_q = pre_covid_ratio, nq(4)    // quartiles
generate tight_binary = pre_covid_ratio > 1   // "tight" vs "slack"

save pre_covid_tightness, replace


*********************************************************************
****-----					MSA Level						-----****
*********************************************************************

*--------------------------------------------------------------------*
* 1. Load ACS microdata
*--------------------------------------------------------------------*
use "$root/Data/clean_IPUMS_01.dta", clear

* Keep only pre-COVID years
keep if year == 2016

*--------------------------------------------------------------------*
* 2. Create key ACS indicators
*--------------------------------------------------------------------*

* Education (college+)
gen college = inrange(educd, 100, 116)

* Renter indicator
gen renter = rent > 0

* Age groups
gen age25_44 = inrange(age,25,44)
gen age45_64 = inrange(age,45,64)

* Employment
gen employed = empstat == 1

label list met2013

*--------------------------------------------------------------------*
* 3. Collapse to MSA level (weighted means using hhwt)
*--------------------------------------------------------------------*
collapse (mean) ///
    hhinc      = hhincome ///
    pov        = poverty ///
    college    = college ///
    employed   = employed ///
    renter     = renter ///
    age25_44   = age25_44 ///
    age45_64   = age45_64 ///
    commute    = trantime ///
    density    = density ///
    vacancy    = vacancy ///
    valueh     = valueh ///
    rooms      = rooms ///
    bedrooms   = bedrooms ///
    (first) statefip ///
    [pw=hhwt], by(met2013)

save acs_msa_1819, replace


*******************************************************************************
****-----					Zillow ACS MERGE						  -----****
*******************************************************************************
*----------------------------------------------------------------------
* 0. Import crosswalk and save as .dta
*----------------------------------------------------------------------
import delimited "acs_zillow_crosswalk.csv", clear
save acs_zillow_crosswalk, replace


*----------------------------------------------------------------------
* 1. Prepare ACS covariates with proper MSA string name
*----------------------------------------------------------------------
use acs_msa_1819, clear
decode met2013, gen(acs_met2013)

keep acs_met2013 hhinc pov college employed renter ///
     age25_44 age45_64 commute density vacancy valueh rooms bedrooms pop

save acs_msa_named, replace


*----------------------------------------------------------------------
* 2. Load Zillow pre-COVID SLR at region level
*----------------------------------------------------------------------
use pre_covid_ratio, clear   // contains regionid, pre_covid_ratio, regionname, etc.


*----------------------------------------------------------------------
* 3. Link Zillow regions → ACS MSAs using the crosswalk
*----------------------------------------------------------------------
merge m:1 regionid using acs_zillow_crosswalk, keep(match) nogen
* Now have: regionid, pre_covid_ratio, acs_met2013, acs_regionname


*----------------------------------------------------------------------
* 4. Merge ACS covariates & population
*----------------------------------------------------------------------
merge m:1 acs_met2013 using acs_msa_named, keep(match) nogen
* Now each Zillow region has ACS covariates + ACS pop


*----------------------------------------------------------------------
* 5. Compute pop-weighted pre-COVID SLR at MSA level
*----------------------------------------------------------------------
gen slr_pop = pre_covid_ratio * pop

collapse ///
    (sum) slr_pop ///
    (sum) pop ///
    (mean) hhinc pov college employed renter ///
           age25_44 age45_64 commute density vacancy valueh rooms bedrooms, ///
    by(acs_met2013)

gen pre_covid_slr_weighted = slr_pop / pop

save pre_covid_slr_weighted_msa, replace




*********************************************************************
****-----   DISTRIBUTION OF POP-WEIGHTED PRE-COVID SLR        -----****
*********************************************************************

use pre_covid_slr_weighted_msa, clear

summarize pre_covid_slr_weighted

kdensity pre_covid_slr_weighted, ///
    xtitle("Pre-COVID SLR (Pop-Weighted, MSA Level)") ///
    title("Kernel Density: Pop-Weighted Pre-COVID Tightness (MSAs)")


histogram pre_covid_slr_weighted, width(.01) ///
    xtitle("Pre-COVID SLR (Pop-Weighted, MSA Level)") ///
    title("Histogram: Pop-Weighted Pre-COVID Tightness (MSAs)")



use pre_covid_slr_weighted_msa, clear
gsort -pre_covid_slr_weighted
list acs_met2013 pre_covid_slr_weighted in 1/5, noobs
gsort pre_covid_slr_weighted
list acs_met2013 pre_covid_slr_weighted in 1/5, noobs

** Laredo, TX and Lincoln, NE have 0, let's cut them. **

use pre_covid_slr_weighted_msa, clear

* Sort ascending (lowest first)
gsort pre_covid_slr_weighted

* See the bottom 5 to verify
list acs_met2013 pre_covid_slr_weighted in 1/5, noobs

* Drop bottom 2 markets
drop in 1/2

* Save trimmed file
save pre_covid_slr_weighted_msa_trimmed, replace

use pre_covid_slr_weighted_msa_trimmed, clear

* Histogram
histogram pre_covid_slr_weighted, width(.01) ///
    percent title("Trimmed SLR Distribution")
graph export "SLR_MSA_Weighted_Histogram.png", replace


* Kernel density
kdensity pre_covid_slr_weighted, ///
    title("Trimmed Kernel Density: SLR")
graph export "SLR_MSA_Weighted_Kernel.png", replace





/*********************************************************************
****-----   BUILD HOME VALUE PANEL USING PERWT                  -----*
*********************************************************************/

use "$root/Data/clean_IPUMS_01.dta", clear

* Keep only relevant ACS years
keep if inlist(year, 2016, 2021, 2022)

* Convert numeric MSA to name
decode met2013, gen(acs_met2013)

* ---------------------------------------------------------------
* Collapse to MSA × year using perwt
* ---------------------------------------------------------------
collapse ///
    (mean) valueh = valueh ///
    (sum)  pop    = perwt ///
    [pw=perwt], by(acs_met2013 year)

save acs_valueh_msa, replace


/*********************************************************************
****-----           PRE = 2016     POST = 2021–22                -----*
*********************************************************************/

use acs_valueh_msa, clear

* ---- PRE ----
preserve
    keep if year == 2016
    rename valueh valueh_pre
    keep acs_met2013 valueh_pre
    save acs_valueh_pre, replace
restore

* ---- POST ----
preserve
    keep if inlist(year, 2021, 2022)
    collapse (mean) valueh_post = valueh, by(acs_met2013)
    save acs_valueh_post, replace
restore

* ---- Merge PRE + POST ----
use acs_valueh_pre, clear
merge 1:1 acs_met2013 using acs_valueh_post, nogen keep(match)

* Log change in home values
gen log_value_change = ln(valueh_post) - ln(valueh_pre)

save acs_homevalue_change, replace


/*********************************************************************
****-----  FIX ACS COVARIATES (RE-WEIGHT USING PERWT)          -----*
*********************************************************************/

use "$root/Data/clean_IPUMS_01.dta", clear
keep if year == 2016

decode met2013, gen(acs_met2013)

* Create indicator variables BEFORE collapsing
gen college     = (educd>=100 & educd<=116)
gen employed    = (empstat==1)
gen renter      = (rent>0)
gen age25_44    = inrange(age,25,44)
gen age45_64    = inrange(age,45,64)

* Now collapse using perwt
collapse ///
    (mean) hhinc     = hhincome ///
    (mean) pov       = poverty  ///
    (mean) college   = college  ///
    (mean) employed  = employed ///
    (mean) renter    = renter   ///
    (mean) age25_44  = age25_44 ///
    (mean) age45_64  = age45_64 ///
    (mean) commute   = trantime ///
    (mean) density   = density  ///
    (mean) vacancy   = vacancy  ///
    (mean) valueh    = valueh   ///
    (mean) rooms     = rooms    ///
    (mean) bedrooms  = bedrooms ///
    (sum)  pop       = perwt ///
    [pw=perwt], by(acs_met2013)


save acs_msa_1819_named, replace


/*********************************************************************
****-----       MERGE TREATMENT + OUTCOME + COVARIATES        -----*
*********************************************************************/

use pre_covid_slr_weighted_msa_trimmed, clear

merge 1:1 acs_met2013 using acs_homevalue_change, nogen keep(match)
merge 1:1 acs_met2013 using acs_msa_1819_named, nogen keep(match)


/*********************************************************************
****-----  FINAL CLEANING & EXPORT                              -----*
*********************************************************************/

keep acs_met2013 pre_covid_slr_weighted log_value_change ///
     hhinc pov college employed renter ///
     age25_44 age45_64 commute density vacancy valueh ///
     rooms bedrooms pop

order acs_met2013 pre_covid_slr_weighted log_value_change hhinc pov renter

save final_acs_slr_dataset, replace
export delimited using "cf_input_acs_valueh.csv", replace




