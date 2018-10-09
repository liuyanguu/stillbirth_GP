
version 11
capture log close
pause on
set more off
cd "SET TO DIRECTORY WHERE THE ACCOMPANYING FILES ARE SAVED"

**********************************************************************************************

global model "ln_sbr ln_nmr ln_gni mean_edu i.n_context5 anc4 lbw_final i.shmdg2||country:"

**********************************************************************************************
use "dta\SBR_input", clear

gen bstrap=1

**************************************************************************************************
*  get dataset for national predictions
append using "dta\national_covariates"

sort country year
 
*  Save file for bootstrap
preserve
  save "dta\2015bootstrap_data",replace
restore 

**************************************************************************************************
* Section to fit the model
   
   quietly xtmixed $model
   capture drop ln_sbrhat
   predict ln_sbrhat,xb
   
   capture drop ln_blup
   predict ln_blup, fitted
   
   capture drop blup
   gen blup=ln_blup-ln_sbrhat
   
   capture drop resid
   gen resid=ln_sbr-ln_blup
   
   label variable ln_blup "Predicted log(SBR)"
   label variable blup "Estimated country-level random effects"
   label variable resid "Residual ln(SBR) (observed-predicted)"
   
   capture drop predsbr
   gen predsbr=exp(ln_blup)/1000
   replace predsbr=exp(ln_sbrhat)/1000 if predsbr==.
   label variable predsbr "Predicted SBR"

  
  *diagnostics based on last model
   noisily twoway (scatter ln_sbr ln_blup) (function y=x, range(0 5)), ///
      xtitle("Predicted ln(SBR)") ytitle("Observed ln(SBR)") legend(off)
	  graph export predictedvsobs.gph, replace
   noisily twoway (scatter resid ln_blup, ylabel(-1.5(0.5)1.5) yline(0) ///
      xtitle("Predicted ln(SBR)") ytitle("Residuals") legend(off))
      graph export scatter_residuals.gph, replace
   sort country
   capture drop count_n
   by country:gen count_n=_n
   noisily histogram blup if count_n==1,normal xlabel(-1.0(0.5)1.0)
   graph export hist_RE.gph, replace
   noisily histogram resid,normal xlabel(-1.5(0.5)1.5)
   graph export hist_residual.gph, replace


*******************************************************************************************
*  Section to produce national level predictions for aech year 2000 to 2015

 forvalues year=2000(1)2015{
   capture drop count
   preserve
   keep if national==1
   * adding in reported rates 
   append using "dta\reported_loess.dta"
   sort iso3 year
   capture drop count
   by iso3 year:gen count=_N
   drop if count>1 & report==.
 
   encode country,gen(idcode)
   gen stillbirths=predsbr*nlb/(1-predsbr)
   replace stillbirths=sbr28r*nlb/(1-sbr28r) if stillbirths==.
   keep country whoname idcode nmdg nmr_final stillbirths predsbr sbr28r nlb year iso3 

   gen total_stillbirths=stillbirths  
   gen ave_sbr=total_stillbirths/(nlb+total_stillbirths)
  
   gen tot_births=nlb+total_stillbirths
   
   keep if year==`year'
   table nmdg, c(sum tot_births sum total_stillbirths) row 
   egen sbreg=sum(total_stillbirths),by(nmdg)
   egen birthsreg=sum(tot_births),by(nmdg)
   gen sbrreg=sbreg*1000/birthsreg
   sort nmdg
   by nmdg: gen count=_n
   table nmdg, c (mean sbrreg)
   sort whoname
   gen sbr=predsbr*1000
   replace sbr=sbr28r*1000 if sbr==.
   brow whoname year total_stillbirths tot_births sbr
   save `year'predictions, replace
  
   drop sbreg birthsreg sbrreg
   summ ave_sbr,detail
   quietly {
      summ nlb
	  local lbtot=r(sum)
	  summ total_stillbirths
	  local sb=r(sum)
	  local sbr= `sb'*1000/(`sb'+`lbtot')
      noisily di "Global SBR = ",`sbr'
   }
   save `year'_global,replace
restore
}

*************************************************************************************

*   Generate uncertainty estimates

use "dta\2015bootstrap_data", clear

gen reportcountry=.

replace reportcountry=1 if iso3=="KWT"| iso3=="ROU"| iso3=="CHL"| iso3=="CRI"| iso3=="CUB"| iso3=="MUS"| iso3=="ARG"| iso3=="AUS"| iso3=="AUT"| iso3=="BGR"| iso3=="HRV"|iso3=="CZE"|iso3=="DNK"|iso3=="EST"|iso3=="FIN"|iso3=="DEU"|iso3=="HUN"|iso3=="ISL"|iso3=="IRL"|iso3=="ISR"|iso3=="ITA"|iso3=="JPN"|iso3=="LVA"|iso3=="LTU"|iso3=="LUX"|iso3=="MLT"|iso3=="NLD"|iso3=="NZL"|iso3=="NOR"|iso3=="POL"|iso3=="PRT"|iso3=="RUS"|iso3=="SVK"|iso3=="SVN"|iso3=="ESP"|iso3=="SWE"|iso3=="MKD"|iso3=="GBR"|iso3=="USA"

save "dta\2015bootstrap_data", replace   

 forvalues year=2000(1)2015{ 
 use "dta\2015bootstrap_data", clear
preserve
   keep if national==1 & reportcountry!=1
   keep if year==`year'
   save `year'temp_national,replace
restore

*  set number of bootstrap samples

local n_bstrap=1000

*  set counter for successful bootstrap samples
local i=1
*  set counter for total bootstrap attempts
local j=1
 
quietly while `i'<=`n_bstrap' {
   use "dta\2015bootstrap_data",clear
   keep if bstrap==1
   summ ln_sbr
   local N=r(N)
   bsample `N'
   append using `year'temp_national
   
   *  Fit model 
   if mod(`i',50)==1 {
      noisily display in green `i'
   }
   set maxiter 20
   capture xtmixed $model
   if _rc!=0 {
      local j=`j'+1
	  continue
   }  
   local ln_sigma_u=[lns1_1_1]:_cons
   local sigma_u=exp(`ln_sigma_u')
   
   capture drop ln_sbrhat
   predict ln_sbrhat,xb 
   predict ln_blup,fitted
   gen blup=ln_blup-ln_sbrhat

   gen predsbr=exp(ln_blup)/1000
   replace predsbr=exp(ln_sbrhat+`sigma_u'*rnormal())/1000 if predsbr==. 
   keep if national==1

   gen stillbirths=predsbr*nlb/(1-predsbr)
   keep country nmdg stillbirths predsbr nlb 
   gen n_sample=`j'
   if `i'>1 {
      append using `year'bsoutput
   }
   save `year'bsoutput,replace
 
   local i=`i'+1
   local j=`j'+1
 }


use `year'bsoutput,clear
 
encode country,gen(idcode)
gen total_stillbirths=stillbirths 
gen ave_sbr=total_stillbirths/(nlb+total_stillbirths)
gen tot_births=nlb+total_stillbirths
   
save `year'bsoutput, replace

use "dta\reportedrates_sbr_loess.dta", clear
save "dta\reported_rates", replace
keep if year==`year'

local i=1
local numsim=1000
quietly while `i'<=1000 {
    use "dta\reported_rates", clear
	keep if year==`year'
	gen stillbirths=sbr28r*nlb/(1-sbr28r)
    gen n_sample=`i'
    gen se=sqrt(stillbirths)
    gen uncertain=round(stillbirths+se*rnormal())
    replace stillbirths=max(0,uncertain)
    gen sbr=(stillbirths/(stillbirths+nlb))*1000
    if `i'>1 {
       append using `year'RR_range
    }
    save `year'RR_range, replace
 
    local i=`i'+1
 }

use `year'RR_range, clear
gen total_stillbirths=stillbirths 
save `year'RR_range, replace

use `year'bsoutput,clear
append using `year'RR_range
save `year'bsoutput,replace

preserve
  collapse (sum) total_stillbirths, by(n_sample)
  save `year'global_range,replace
  centile total_stillbirths, centile(2.5 97.5)
restore

preserve
  collapse (sum) total_stillbirths, by(n_sample nmdg)
  save `year'region_range,replace
  sort nmdg total_stillbirths
  keep nmdg total_stillbirths
  by nmdg: gen count=_n
  keep if count==25 | count==975
  save `year'region_range_final,replace
  reshape wide total_stillbirths, i(nmdg) j(count)
  list
restore

preserve
  sort country total_stillbirths
  keep country total_stillbirths
  by country: gen count=_n
  keep if count==25 | count==975
  reshape wide total_stillbirths, i(country) j(count)
  save `year'country_range,replace
restore
}

*



