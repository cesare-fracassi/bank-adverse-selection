/*******************************************************************************
* Title: Generating pseudo data For "Adverse Selection in Corporate Loan Markets" 

* Authors: Mehdi Beyhaghi, Cesare Fracassi, Gergory Weitzner

* Date: 4/11/2025

* Input Data: None
* Output: Pseudo data

* Description: In accordance with the Journal of Finance guidelines, we have created 
a pseudo-dataset that can be used to run the code that is shared and reproduce the 
tables and  figures. Please note that the ouput from the following code is not the 
actual data used in the paper; therefore, results obtained from this pseudo-dataset 
will differ from those presented in the publication.

To ensure consistency with the final results and to facilitate smooth execution, 
including the propensity score matching analysis, we have artificially adjusted 
correlations among certain variables. These adjustments can be observed in the 
following code segment.

*******************************************************************************/

cls

clear all
set seed 12345

********************************************************************************
* Step 1: Create firm-level data 
********************************************************************************

local nbanks = 1000     
set obs `nbanks'
gen bank = _n

local ncounty = 500 // set number of counties 
gen county = ceil(`ncounty' * (runiform()^2)) // skews distribution

gen gsib = (runiform() < 0.2) // set 20% of banks as GSIB

gen dummy = 1 //dummy key for the cross join


tempfile banklevel
save `banklevel'


********************************************************************************
* Step 2: Create quarterly panel data for 2014-2019
********************************************************************************
clear
set obs 24   // quarters = 6 years * 4 quarters
gen qtr = mod(_n-1, 4) + 1
gen year  = 2014 + floor((_n-1)/4)
gen yq = yq(year, qtr)
format yq %tq

gen dummy = 1 //dummy key for the cross join


tempfile timepanel
save `timepanel'


********************************************************************************
* Step 3: Merge bank-level and quarterly data (cross join) to form the panel
********************************************************************************
use `banklevel', clear
joinby dummy using `timepanel'
drop dummy


tempfile panel
save `panel', replace


********************************************************************************
* Step 4: Create County-Time Level Data with Random Variables
********************************************************************************
* Extract unique county-month combinations 
use `panel', clear
preserve
keep county year yq
duplicates drop
tempfile countytime
save `countytime', replace
restore

* Generate county-month level covariates which are correlated to generate sufficient variation in propensity scores 
use `countytime', clear
gen logdensityy = max(0,rnormal(7, 1.5))
gen logpopy     = 2*logdensityy + rnormal(0,1)
gen logwage     = 1.5*logdensityy + rnormal(0, 1) 
gen logwagefin  = logdensityy + 0.4*logwage + rnormal(0, 1)

gen numbankcounty = max(round(runiform(-12,11) + 5*(runiform(0,1)*(logdensityy/15) +  (logwage/12) + (logwagefin/12)+0.07*logpopy)),1)
gen numbank = round(numbankcounty*0.75)
gen numbankcounty1 = (numbankcounty==1)
gen avgherfdepcty = runiform()
gen hhi = 0.3+runiform(0, 0.7)

gen numbankmsa = numbankcounty*3+round(runiform(1,10))
gen numbankmsa1 = numbankmsa==1

tempfile countytimevars
save `countytimevars', replace


********************************************************************************
* Step 5: Merge County-Time Variables into the Panel Data
********************************************************************************
use `panel', clear
merge m:1 county year yq using `countytimevars', nogen


*Similar to the county variables used in propensity scores, here I arbitrarily generate correlation to generate sufficient variation in propensity scores 


********************************************************************************
* Step 6: Generate all remaining key variables  
********************************************************************************

local discrete_vars "taxid d numbankbranch numbranch cyq credit_facility_purpose_cat qtrnum  spread_reported  utilization numotherlenders numprevlenders origination_y origination_m  maturity_y maturity_m   companyID companyIDyq loanid igroup iquarter fedigroup maturity2  line_variability taxidlength timecode banktime estloss estloss2 asc31010 asc31030 locom sen_secured personalguarantee numgsib2015 numbankbranchy14 numbankbranchgsib numgsib2015branch  nloan bankcounty geographicarea msa csa numnongsib sizebin sizebin20 pdlgdbin pdbin nloans_countyq numbankcountynofilter1 numbankcountynofilter2 numbankcountynofilter3 numbankmsanofilter1 numbankmsanofilter2 numbankmsanofilter3 top3 "

local string_vars "stname ctyname"

local continuous_vars "amt logamt  census2010pop areainsquaremilestotalarea areainsquaremileswaterarea areainsquaremileslandarea densitypersquaremileoflandareapo bapop estpop   densitypersquaremileoflandareaho EstablishmentCountc TotalQuarterlyWagesc AverageWeeklyWagec EmploymentLocationQuotientRelc TotalWageLocationQuotientRelc AvgEmpc QendEmpc EmpGrowthc EmpAvgGrowthc TQWageGrowthc EstGrowthc LEmpGrowthc LEmpAvgGrowthc LTQWageGrowthc LEstGrowthc EstablishmentCountfin TotalQuarterlyWagesfin AverageWeeklyWagefin EmploymentLocationQuotientRelfin TotalWageLocationQuotientRelfin AvgEmpfin QendEmpfin EmpGrowthfin EmpAvgGrowthfin TQWageGrowthfin EstGrowthfin LEmpGrowthfin LEmpAvgGrowthfin LTQWageGrowthfin LEstGrowthfin rent wage wagefin   logrent logdensity bbttruist suntrust postorig bbtorig bbtcountyorig suntrustcountyorig mergercountyorig truistcountyorig marketshareorig truistorig stshareorig bbtshareorig truistshareorig minshareorig deltashareorig assets size margin liquidity sales_g_1y_alt sales_g_1y sales_g_2y rating_c_1y rating_c_2y def1 def2   numbankcountynofilter numbankmsanofilter fin maxfin postmerger bbt bbtcounty suntrustcounty mergercounty truistcounty truist stshare bbtshare truistshare minshare deltashare pd99prc pd95prc loannumber annualhhic numbankcy   mhhi  fips annualhhimsa numbankmsay  hhimsa highhhimsa bmarketsharec bmarketsharemsa mediandensity highdensity  avgherfnbrcty taxidloanrecord firmtotalloans lossbin logamtXlogmaturity guaranteeXlogmaturity guaranteeXlogamt hhidecile hhidecile1 hhidecile2 hhidecile3 hhidecile4 hhidecile5 hhidecile6 hhidecile7 hhidecile8 hhidecile9 hhidecile10 hhimsadecile hhimsadecile1 hhimsadecile2 hhimsadecile3 hhimsadecile4 hhimsadecile5 hhimsadecile6 hhimsadecile7 hhimsadecile8 hhimsadecile9 hhimsadecile10 densitydecile densitydecile1 densitydecile2 densitydecile3 densitydecile4 densitydecile5 densitydecile6 densitydecile7 densitydecile8 densitydecile9 densitydecile10 hhiquartile hhiquartile1 hhiquartile2 hhiquartile3 hhiquartile4 hhimsaquartile hhimsaquartile1 hhimsaquartile2 hhimsaquartile3 hhimsaquartile4 densityquartile densityquartile1 densityquartile2 densityquartile3 densityquartile4 ir2 ir3 _reghdfe_resid  ir4 markup2 markup3 markup4 lerner lerner_w markup2_w markup3_w msaid countycode volume amarkup amarkup_w air lerneri asecured volume_msa nloans_msaq amarkup_msa amarkup_w_msa air_msa lerneri_msa asecured_msa newbvol newbvol_msa obvol obvol_msa newbratio newbratio_msa lognewbvol lognewbvol_msa logobvol logobvol_msa sum bsum sum_msa bsum_msa specialized specialized2 specialized3 specialized4  top3amt top3sum top3share myq state  bankcountyear countyyear countyquarter changenbankc changenbankmsa staynbankc staynbankmsa changenbankc1 changenbankmsa1 staynbankc1 staynbankmsa1 changehhic changehhimsa stayhhic stayhhimsa changesize changeleverage changetangibility changeprofitability staysize stayleverage staytangibility stayprofitability staynumprev numbankline numbankline1 totvolcounty numloancounty numfirmcounty  lognumloan lognumfirm  banksum countysum gsibsum countygsibsum marketshare annualhhi  numgsib gsibratio lognumgsib ratiogsib2015 marketsharegsib2015 post marketsharegsibpre numbankpre numgsibpre ratiogsibpre marketsharegsibpost numbankpost numgsibpost ratiogsibpost numbankprepost numgsibprepost mediangsibshare tercilegsibshare quartilegsibshare quintilegsibshare decilegsibshare mediannumgsib tercilenumgsib quartilenumgsib quintilenumgsib decilenumgsib popy densityy  nbankfirst postnumgsib2015 postratiogsib2015 postnumgsib2015branch staygsib postnumgsib2015stay postnumgsib2015gsib postnumgsib2015staygsib postnumgsib2015branchstay postnumgsib2015branchgsib postnumgsib2015branchstaygsib staynumprevlend postnumgsib2015numprevlend postnumgsib2015staynumprevlend postlogpop postlogpopy gsibamt nongsibamt gsibtotvolcounty loggsibvol nongsibtotvolcounty lognongsibvol"

local binary_vars "highhhi changebank variability_fixed variability_float  type_term_all type_term_a type_term_a_b  type_line_revolving  syndicated demand_loan wrc wrf prepayment_penalty_clause obligor_spe guarantee guarantee_full guarantee_usgov newloan leveraged_loan subordinated obligor_public type_line_all first collateraltype1 collateraltype2 collateraltype3 collateraltype4  collateraltype6 collateraltype7   "

local appendix_vars "logempgrowth logbagrowth logfinempgrowth logwagegrowth  logfinwagegrowth numbranchpop logestgrowth logba    logpop  pdlogmaturity pdlogamt pdguarantee pdcollateraltype5 pdsize pdleverage pdtangibility pdprofitability lgdlogmaturity lgdlogamt lgdguarantee lgdcollateraltype5 lgdsize lgdleverage lgdtangibility lgdprofitability numbankcountylogmaturity numbankcountylogamt numbankcountyguarantee numbankcountycollateraltype5 numbankcountysize numbankcountyleverage numbankcountytangibility numbankcountyprofitability numbankcountypd numbankcountylgd numbankcountypdlgd logmaturity2 logamt2 guarantee2 size2 leverage2 tangibility2 profitability2 logdensityy2 logwage2 logwagefin2 logpop2 logpopy2 logmaturity3 logamt3 guarantee3 size3 leverage3 tangibility3 profitability3 logdensityy3 logwage3 logwagefin3 logpop3"


* Special variables with known mean, median, and std. Simulated variable = rnomal(mean, sd) + skew_adjustment, where skew_adjustment = median - mean
* General consistency with summary statistics

set seed 12345
gen amtm = max(1, rnormal(7.17, 14.74) + 2.66-7.17)
gen pd_prc = min(max(0.03, rnormal(1.34, 1.70) + 0.87-1.36 +0.01* numbankcounty ), 20)
gen lgd_prc = min(max(3, rnormal(35.11, 14.68) + 36-35.11), 50)
gen pdlgd = pd_prc*lgd_prc/100
gen pdlgd2 = pdlgd^2
gen interest_rate_prc= max(0, rnormal(2.3, 0.8) + 0.08*(pd_prc+0.1*lgd_prc+pdlgd)+ 0.005*(7-numbankcounty)^2)
gen sizem =  max(1, rnormal(152.50, 706.61)  )
gen salesm = min(max(2, rnormal(229, 1247)  ), 500)
gen leverage = min(max(0, rnormal()  ), .9)
gen profitability = runiform(0, .4)
gen tangibility = min(0.9+runiform(-.5, .4), 1)

gen maturity = round(runiform(5,100))
gen maturity_remaining_months = maturity-4
gen logmaturity = log(maturity)

gen secured = runiform()>0.9
gen collateraltype5 = 0.3*secured
gen staybank =runiform()>.76

gen logvol = (log(100*amtm)+ runiform(1,1.2)*numbankcounty)/3

* All discrete_vars generated with 20 numerical categories 
foreach var of local discrete_vars {
    gen `var' = ceil(20*runiform())
}

* All string_vars generated with 20 string categories 
foreach var of local string_vars {
    gen str10 `var' = "cat" + string(ceil(20*runiform()))
}

* All continuous_vars generated with N(0,1) draws
foreach var of local continuous_vars {
    gen `var' = rnormal(0,1)
}

* All binary_vars generated with 1(uniform[0,1] < 0.5)
foreach var of local binary_vars {
    gen `var' = (runiform() < 0.5)
}

* All appendix_vars generated with N(0,1) draws
foreach var of local appendix_vars {
    gen `var' = rnormal(0,1)
}

* Generate default dummy variables with binary 0/100 due to "roccomp" around line 1180 in "Online Appendix Tests for Cesare" requiring this. 
gen default1_prc = 80*(pd_prc>4.9)+ 20*(runiform()<.02)
gen default2_prc = 80*(pd_prc>5.2)+ 20*(runiform()<.02)
gen delinq1_prc = 80*(pd_prc>4.6)+ 20*(runiform()<.03)
gen delinq2_prc = 80*(pd_prc>4.4)+ 20*(runiform()<.03)
	
egen mediantangibility = median(tangibility)
gen htang = tangibility>mediantangibility

gen public=obligor_public

replace numgsib2015 = round(1+ numgsib2015/6)
replace numbankbranch = round(1+numgsib2015*3)
gen mpd=2.66

gen gsibpost = gsib*post

save "BFW_main.dta", replace


********************************************************************************
* Step 7: Create public
********************************************************************************

keep if public
save "BFW_public.dta", replace





