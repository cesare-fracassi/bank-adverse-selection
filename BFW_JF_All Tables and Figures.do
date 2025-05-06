/*******************************************************************************
* Title: Replication Code For "Adverse Selection in Corporate Loan Markets" 

* Authors: Mehdi Beyhaghi, Cesare Fracassi, Gergory Weitzner

* Date: 4/11/2025

* Input Data: Loan level data as described in the data section
* Output: All tables and figures in the main paper and Internet Appendix.

* Description: The code produces results in 5 parts:
	- Tables in the main paper
	- Figures in the main paper
	- Internet Appendix 1
	- Internet Appendix Propensity Score Matching results
	- Internet Appnedix non-public firm reesults
*******************************************************************************/

cls
clear


***************************************************
* Directories (Update this before running the code) 
***************************************************
global datadir "............................"					//Data directory
global mainlatexdir "......................................"	//LaTex output directory for tables and figures in the main paper
global ialatexdir "......................................"		//LaTex output directory for tables and figures in the internet appendix

global master "BFW_main.dta"									// Main input data
global public "BFW_public.dta"									// Input data related to non-private firms	///
																// with similar structure as the main input data.

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 														Tables - Main Paper															 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
cd "$datadir"
log using "$mainlatexdir/logfile.log", replace

use "$master", clear



***************************************************************************************************************
* Labeling variables, values, etc.
***************************************************************************************************************
la var interest_rate_prc "Interest Rate (\%)"
la var maturity "Maturity (months)"
la var logmaturity "Log(maturity)"
la var amt "Loan Amount (USD)"
la var amtm "Amount (million USD)"
la var logamt "Log(Amount)"
la var secured "Secured"
la var collateraltype5 "Secured by Blanket Lien"
la var guarantee "Guaranteed"
la var syndicated "Syndicated"
la var variability_float "Floating Interest Rate"
la var type_line_all "Line of Credit"
la var wrc "With Rate Ceiling"
la var wrf "With Rate Roof"

*la var borrower_rating "Internal Credit Rating"
la var pd_prc "Probability of Default (\%)"
la var lgd_prc "Loss Given Default (\%)"
la var pdlgd "Expected Loss (\%)"
la var pdlgd2 "Expected Loss\^2"

la var default1_prc "Realized Default (\%)"
la var delinq1_prc "Non-Performance (\%)"
la var default2_prc "Realized Default (2yr) (\%)"
la var delinq2_prc "Non-Performance (2yr) (\%)"

la var sizem "Assets (million USD)"
la var size "Log(Assets)"
la var leverage "Leverage"
la var tangibility "Tangibility"
la var profitability "Profitability"
la var salesm "Net Sales (million USD)"


la var numbankcounty "Number of Banks"
la var numbankcounty1 "One Bank"
la var numbankmsa  "Number of Banks MSA"
la var numbankmsa1 "One Bank MSA"
la var numbankbranch "Number of All Banks"
la var numbankline "Number of Banks $\times$ Line of Credit"
la var numbankline1 "One Bank $\times$ Line of Credit"		

la var nbankfirst "New Borrower $\times$ Number of Banks"
la var first "New Borrower"

la var hhimsa "MSA Loan HHI"
la var hhi "Loan HHI"
la var logdensity "Population Density"
la var logdensityy "Population Density"
la var logpop "Population"
la var logpopy "Population"
la var wage "Wages"
la var wagefin "Financial Industry Wages"
la var logwage "Wages"
la var logrent "Rent"
la var logwagefin "Financial Industry Wages"
la var avgherfdepcty "Deposit HHI"

la var changebank "Change Bank"
la var changenbankc "Change Bank $\times$ N Bank County"
la var changenbankmsa "Change Bank $\times$ N Bank MSA"
la var changenbankc "Change Bank $\times$ N Bank County"
la var changenbankmsa "Change Bank $\times$ N Bank MSA"

la var staybank "Stay Bank"
la var staynbankc "Stay Bank $\times$ N Bank County"
la var staynbankmsa "Stay Bank $\times$ N Bank MSA"
la var staynbankc "Stay Bank $\times$ N Bank County"
la var staynbankmsa "Stay Bank $\times$ N Bank MSA"

la var changesize "Change Bank $\times$ Log(Assets)"
la var changeleverage "Change Bank $\times$ Leverage"
la var changetangibility "Change Bank $\times$ Tangibility"
la var changeprofitability "Change Bank $\times$ Profitability"

la var staysize "Stay Bank $\times$ Log(Assets)"
la var stayleverage "Stay Bank $\times$ Leverage"
la var staytangibility "Stay Bank $\times$ Tangibility"
la var stayprofitability "Stay Bank $\times$ Profitability"

la var numprevlenders "Number of Prior Lenders"
la var staynumprev "Stay Bank $\times$ Number of Prior Lenders"

la var gsib "GSIB"
la var staybank "Stay Bank"
la var first "New Borrower"

la var numgsib2015	"Number of GSIBs (2015)"
la var postnumgsib2015 "Post $\times$ Number of GSIBs (2015)"
la var postratiogsib2015 "Post $\times$ Ratio of GSIBs (2015)"
la var numbank "Number of Banks (Annual)"
la var gsibpost "GSIB$\times$ Post"
la var staygsib "Stay Bank $\times$ GSIB"
la var postnumgsib2015stay "Post $\times$ Number of GSIBs (2015) $\times$ Stay Bank"
la var postnumgsib2015gsib "Post $\times$ Number of GSIBs (2015) $\times$ GSIB"
la var postnumgsib2015staygsib "Post $\times$ Number of GSIBs (2015) $\times$ Stay Bank $\times$ GSIB"
la var staynumprevlend "Stay Bank $\times$ Number of Prior Lenders"
la var postnumgsib2015numprevlend "Post $\times$ Number of GSIBs (2015) $\times$ Number of Prior Lenders"
la var postnumgsib2015staynumprevlend "Post $\times$ Number of GSIBs (2015) $\times$ Stay Bank $\times$ Number of Prior Lenders"
la var postlogpop "Post $\times$ Population"
la var postlogpopy "Post $\times$ Population"

la var numprevlenders "Number of Prior Lenders"
	
***************************************************************************************************************
* Global variables
***************************************************************************************************************
global loancontrols "logmaturity logamt guarantee"		//Loan charactersitics
global firmvarssize "size leverage tangibility profitability "		//Firm charactersitics
global fe "i.banktime i.iquarter i.credit_facility_purpose_cat i.line_variability "		//Fixed effects 1
global fenobanktime "i.iquarter i.credit_facility_purpose_cat i.line_variability "		//Fixed effects 2

global clustervar "taxid"	//Cluster varible, firm ID
global ylimit 2020
global qlimit 1

* Summary statistics variables
global sumstat_loan "amtm interest_rate_prc pd_prc lgd_prc pdlgd variability_float guarantee maturity delinq1_prc numprevlenders first type_line_all delinq1_prc default1_prc secured collateraltype5 staybank gsib"
global sumstat_firm "sizem salesm leverage profitability tangibility"
global sumstat_geog "numbankcounty numbank numgsib2015 numbankbranch logdensityy logwage logwagefin logpopy avgherfdepcty hhi"



cd "$mainlatexdir"
	
	
***************************************************************************************************************
* Table I. Summary Statistics
***************************************************************************************************************

* loan chars
eststo clear  
eststo: estpost tabstat $sumstat_loan, statistics(Mean SD p10 Median p90 count) columns(statistics) 
esttab using "SumStat_loan.tex", cell((mean(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) sd(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) ///
	p10(label(\multicolumn{1}{c}{}) fmt(%9.2fc))  p50(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) p90(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) count(label(\multicolumn{1}{c}{}) fmt(%9.0fc)))) ///
	noobs nostar unstack nonote nonumber label fragment booktabs replace
	
	
* firm chars
eststo clear  
eststo: estpost tabstat $sumstat_firm, statistics(Mean SD p10 Median p90 count) columns(statistics) 
esttab using "SumStat_firm.tex", cell((mean(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) sd(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) ///
	p10(label(\multicolumn{1}{c}{}) fmt(%9.2fc))  p50(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) p90(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) count(label(\multicolumn{1}{c}{}) fmt(%9.0fc)))) ///
	noobs nostar unstack nonote nonumber label fragment booktabs replace

	
* geographical chars 
eststo clear  
eststo: estpost tabstat $sumstat_geog, statistics(mean SD p10 Median p90 count) columns(statistics)  
esttab using "SumStat_geog.tex", cell((mean(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) sd(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) ///
	p10(label(\multicolumn{1}{c}{}) fmt(%9.2fc))  p50(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) p90(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) count(label(\multicolumn{1}{c}{}) fmt(%9.0fc)))) ///
	noobs nostar unstack nonote nonumber label fragment booktabs replace
			
			
				
*********************************************************************************************************************************
* Table II. Correlation Between Measures of Market Concentration			
*********************************************************************************************************************************
corrtex numbankcounty avgherfdepcty hhi numgsib2015 numbankbranch, file(Concent_corr2.tex) digits(2) replace
		

*********************************************************************************************************************************
* Table III. Interest Rates and Risk Assessments
*********************************************************************************************************************************

* Model 1
reghdfe interest_rate_prc $loancontrols , absorb($fe, savefe )  vce(cluster county) 
			est store altmarkupreg, title("")
			estadd local loanchars "YES": altmarkupreg
			estadd local banktimefx "YES": altmarkupreg
			estadd local indtimefx "YES": altmarkupreg
			estadd local loantypefx "YES": altmarkupreg
			estadd local loanpurposefx "YES": altmarkupreg
		
* Model 2
reghdfe interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols, absorb($fe, savefe )  vce(cluster county) 
			est store markupreg, title("")
			estadd local loanchars "YES": markupreg
			estadd local banktimefx "YES": markupreg
			estadd local indtimefx "YES": markupreg
			estadd local loantypefx "YES": markupreg
			estadd local loanpurposefx "YES": markupreg

#delimit;
	esttab altmarkupreg markupreg using "TableMarkupcontrols.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (pd_prc lgd_prc pdlgd $loancontrols ) 
	order (pd_prc lgd_prc pdlgd $loancontrols )
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(2) collabels(none) substitute(_ \_) 
mgroups("Interest Rate (\%)", pattern(1 0) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
	label ("Loan Type" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))

;
#delimit cr

		
			
*********************************************************************************************************************************
* Table IV. Interest Rates, Risk Assessments and Loan Performance
*********************************************************************************************************************************
* Non-Performance
reghdfe delinq1_prc interest_rate_prc $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def22reg, title("")
			estadd local loanchars "YES": def22reg
			estadd local banktimefx "YES": def22reg
			estadd local indtimefx "YES": def22reg
			estadd local loantypefx "YES": def22reg
			estadd local loanpurposefx "YES": def22reg
			
reghdfe delinq1_prc pd_prc lgd_prc pdlgd $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def23reg, title("")
			estadd local loanchars "YES": def23reg
			estadd local banktimefx "YES": def23reg
			estadd local indtimefx "YES": def23reg
			estadd local loantypefx "YES": def23reg
			estadd local loanpurposefx "YES": def23reg
			
reghdfe delinq1_prc interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def24reg, title("")
			estadd local loanchars "YES": def24reg
			estadd local banktimefx "YES": def24reg
			estadd local indtimefx "YES": def24reg
			estadd local loantypefx "YES": def24reg
			estadd local loanpurposefx "YES": def24reg
			
			
* Realized Default
reghdfe default1_prc interest_rate_prc $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def26reg, title("")
			estadd local loanchars "YES": def26reg
			estadd local banktimefx "YES": def26reg
			estadd local indtimefx "YES": def26reg
			estadd local loantypefx "YES": def26reg
			estadd local loanpurposefx "YES": def26reg
			
reghdfe default1_prc pd_prc lgd_prc pdlgd $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def27reg, title("")
			estadd local loanchars "YES": def27reg
			estadd local banktimefx "YES": def27reg
			estadd local indtimefx "YES": def27reg
			estadd local loantypefx "YES": def27reg
			estadd local loanpurposefx "YES": def27reg
			
reghdfe default1_prc interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def28reg, title("")
			estadd local loanchars "YES": def28reg
			estadd local banktimefx "YES": def28reg
			estadd local indtimefx "YES": def28reg
			estadd local loantypefx "YES": def28reg
			estadd local loanpurposefx "YES": def28reg
			
#delimit;
	esttab  def22reg def23reg def24reg  def26reg def27reg def28reg using "TablePDValidity.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (interest_rate_prc pd_prc lgd_prc pdlgd) 
	order (interest_rate_prc pd_prc lgd_prc pdlgd)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(6) collabels(none) substitute(_ \_) 
mgroups("Non-Performance (\%)" "Realized Default (\%)", pattern(1 0 0 1 0 0) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
	label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))

;
#delimit cr
					
*********************************************************************************************************************************
* Table V. Market Structure and Interest Rates
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3 county5 using "TableNumbankIR.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		
		
*********************************************************************************************************************************
* Table VI. Market Structure and Borrower Risk
*********************************************************************************************************************************
eststo clear 	

reghdfe pd_prc numbankcounty $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe pd_prc numbankcounty  $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableNumbankPD.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Probability of Default (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		
		
		


*************************************************************************************************************************************************************************************
* Table VII. Market Structure and Loan Volume
*************************************************************************************************************************************************************************************
preserve
			
	* At county-level
	keep numbankcounty logvol logdensityy logwage logwagefin totvolcounty yq county numloancounty numfirmcounty census2010pop popy logpopy
	duplicates drop

	eststo clear 	

	reghdfe logvol numbankcounty , absorb(yq) vce(cluster: yq county) 
				est store county1, title("")
				estadd local timefx "YES": county1
				
	reghdfe logvol numbankcounty logwage logwagefin logdensityy, absorb(yq) vce(cluster: yq county) 
				est store county2, title("")
				estadd local timefx "YES": county2
				
	reghdfe logvol numbankcounty logwage logwagefin logdensityy logpopy, absorb(yq) vce(cluster: yq county) 
				est store county3, title("")
				estadd local timefx "YES": county3
		
				
	#delimit;
				esttab county1 county2 county3 using "TableNumbankVolume.tex", replace eqlabels(none) nomtitles nodepvars label 
						keep (numbankcounty   logdensityy logwage logwagefin logpopy) 
						order (numbankcounty   logdensityy logwage logwagefin logpopy) 
						cells(b(star fmt(3)) t(fmt(3) par abs)) 
						booktabs modelwidth(3) collabels(none) substitute(_ \_) 
					mgroups("Log(loan volume)",  pattern(1 0 0) 
						prefix(\multicolumn{@span}{c}{) suffix(})   
						span erepeat(\cmidrule(lr){@span}))         
					style(tex) star(* 0.10 ** 0.05 *** 0.01) 
					alignment(D{.}{.}{4,6}) 
					stats(timefx N r2_a, fmt(%~12s %9.0fc %9.2f)  
					layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
						label ("Quarter FE" "Observations" "Adj. R-squared"))
	;					
	#delimit cr		
	
			
restore				

*********************************************************************************************************************************
* Table VIII. Market Structure and Collateralization
*********************************************************************************************************************************		
		
eststo clear 	

reghdfe  collateraltype5 numbankcounty $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe collateraltype5 numbankcounty  $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe collateraltype5 numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
	esttab county1  county3 county5 using "TableBlanketLien.tex", replace eqlabels(none) nomtitles nodepvars label 
			keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
			order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
			cells(b(star fmt(3)) t(fmt(3) par abs)) 
			booktabs modelwidth(3) collabels(none) substitute(_ \_) 
		mgroups("Secured by Blanket Lien",  pattern(1 0 0) 
			prefix(\multicolumn{@span}{c}{) suffix(})   
			span erepeat(\cmidrule(lr){@span}))         
		style(tex) star(* 0.10 ** 0.05 *** 0.01) 
		alignment(D{.}{.}{4,6}) 
		stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
			label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

		
*********************************************************************************************************************************
* Table IX. Market Structure and Markups
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableNumbankIROneStage.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

		


*********************************************************************************************************************************
* Table X. Adverse Selection and Firm Tangibility
*********************************************************************************************************************************
eststo clear 	
* IR
reghdfe interest_rate_prc numbankcounty $loancontrols $firmvarssize logdensityy logwage logwagefin if htang == 1, absorb($fe) vce(cluster county)
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty $loancontrols $firmvarssize logdensityy logwage logwagefin if htang == 0, absorb($fe) vce(cluster county)
			est store county2, title("")
			estadd local loanchars "YES": county2
			estadd local banktimefx "YES": county2
			estadd local indtimefx "YES": county2
			estadd local loantypefx "YES": county2
			estadd local loanpurposefx "YES": county2
			
* PD
reghdfe pd_prc numbankcounty $loancontrols $firmvarssize logdensityy logwage logwagefin if htang == 1, absorb($fe) vce(cluster county)
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			
			
reghdfe pd_prc numbankcounty $loancontrols $firmvarssize logdensityy logwage logwagefin if htang == 0, absorb($fe) vce(cluster county)
			est store county4, title("")
			estadd local loanchars "YES": county4
			estadd local banktimefx "YES": county4
			estadd local indtimefx "YES": county4
			estadd local loantypefx "YES": county4
			estadd local loanpurposefx "YES": county4

* Markup
reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols $firmvarssize logdensityy logwage logwagefin if htang == 1, absorb($fe) vce(cluster county)

			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5
			
			
reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols $firmvarssize logdensityy logwage logwagefin if htang == 0, absorb($fe) vce(cluster county)

			est store county6, title("")
			estadd local loanchars "YES": county6
			estadd local banktimefx "YES": county6
			estadd local indtimefx "YES": county6
			estadd local loantypefx "YES": county6
			estadd local loanpurposefx "YES": county6

			
#delimit;
	esttab county1 county2 county3 county4 county5 county6 using "TableNumbankTangibility2groups.tex", replace eqlabels(none) nomtitles nodepvars label 
			keep (numbankcounty  pd_prc lgd_prc pdlgd $loancontrols $firmvarssize logdensityy logwage logwagefin) 
			order (numbankcounty  pd_prc lgd_prc pdlgd $loancontrols $firmvarssize logdensityy logwage logwagefin) 
			cells(b(star fmt(3)) t(fmt(3) par abs)) 
			booktabs modelwidth(6) collabels(none) substitute(_ \_) 
		mgroups("High Tangibility" "Low Tangibility" "High Tangibility" "Low Tangibility" "High Tangibility" "Low Tangibility",  pattern(1 1 1 1 1 1) 
			prefix(\multicolumn{@span}{c}{) suffix(})   
			span erepeat(\cmidrule(lr){@span}))         
		style(tex) star(* 0.10 ** 0.05 *** 0.01) 
		alignment(D{.}{.}{4,6}) 
		stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
			label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr			

		
		
*****************************************************************************************************************
* Table XI. Switching Banks and Markups
*****************************************************************************************************************			
eststo clear
	 
* with fixed effects, but no firm characteristics
reghdfe interest_rate_prc staybank pd_prc lgd_prc pdlgd $loancontrols  , absorb(i.cyq $fe) vce(cluster county)
			est store staycounty2, title("")
			estadd local loanchars "YES": staycounty2
			estadd local countyyqfe "YES": staycounty2
			estadd local banktimefx "YES": staycounty2
			estadd local indtimefx "YES": staycounty2
			estadd local loantypefx "YES": staycounty2
			estadd local loanpurposefx "YES": staycounty2
* main
reghdfe interest_rate_prc staybank pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize , absorb(i.cyq $fe) vce(cluster county) 
			est store staycounty3, title("")
			estadd local loanchars "YES": staycounty3
			estadd local countyyqfe "YES": staycounty3
			estadd local firmchars "YES": staycounty3
			estadd local banktimefx "YES": staycounty3
			estadd local indtimefx "YES": staycounty3
			estadd local loantypefx "YES": staycounty3
			estadd local loanpurposefx "YES": staycounty3
			
* with interactions with number of previous lenders
reghdfe interest_rate_prc staybank numprevlenders staynumprev pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize , absorb(i.cyq $fe) vce(cluster county) 
			est store staycounty4, title("")
			estadd local loanchars "YES": staycounty4
			estadd local countyyqfe "YES": staycounty4
			estadd local firmchars "YES": staycounty4
			estadd local banktimefx "YES": staycounty4
			estadd local indtimefx "YES": staycounty4
			estadd local loantypefx "YES": staycounty4
			estadd local loanpurposefx "YES": staycounty4
			
			
# delimit;
esttab staycounty2 staycounty3 staycounty4 using "TableStayCountyOneStage.tex", replace eqlabels(none) nomtitles nodepvars label 
		keep (staybank numprevlenders staynumprev pd_prc lgd_prc pdlgd $firmvarssize ) 
		order (staybank numprevlenders staynumprev pd_prc lgd_prc pdlgd  $firmvarssize ) 
		cells(b(star fmt(3)) t(fmt(3) par abs)) 
		booktabs modelwidth(3) collabels(none) substitute(_ \_) 
	mgroups("Interest Rate (\%)", pattern(1 0 0 ) 
		prefix(\multicolumn{@span}{c}{) suffix(})   
		span erepeat(\cmidrule(lr){@span}))         
	style(tex) star(* 0.10 ** 0.05 *** 0.01) 
	alignment(D{.}{.}{4,6}) 
	stats(loanchars countyyqfe banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %~12s %9.0fc %9.2f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") 
		label ("Loan Characteristics Controls" "County-Quarter FE" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared" "Observations" "R-squared"))
;
# delimit cr




*************************************************************************************************************************************************************************************
* Table XII. GSIB Surcharges and Market Outcomes (Reduced Form Difference-in-Differences)
***********************************************************************************************************************************************************************************				
* GSIB 
eststo clear
reghdfe numbank postnumgsib2015 $loancontrols staybank if year>2014 & year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
	est store gsib1, title("")
	estadd local bankcounty "YES": gsib1
	estadd local loanchars "YES": gsib1
	estadd local banktimefx "YES": gsib1
	estadd local indtimefx "YES": gsib1

reghdfe logvol postnumgsib2015 $loancontrols staybank if  year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)	//includes 2014
	est store gsib2, title("")
	estadd local bankcounty "YES": gsib2
	estadd local loanchars "YES": gsib2
	estadd local banktimefx "YES": gsib2
	estadd local indtimefx "YES": gsib2

reghdfe interest_rate_prc postnumgsib2015 $loancontrols staybank if  year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)	//includes 2014
	est store gsib3, title("")
	estadd local bankcounty "YES": gsib3
	estadd local loanchars "YES": gsib3
	estadd local banktimefx "YES": gsib3
	estadd local indtimefx "YES": gsib3

reghdfe pd_prc postnumgsib2015 $loancontrols staybank if  year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)	//includes 2014
	est store gsib4, title("")
	estadd local bankcounty "YES": gsib4
	estadd local loanchars "YES": gsib4
	estadd local banktimefx "YES": gsib4
	estadd local indtimefx "YES": gsib4

reghdfe interest_rate_prc postnumgsib2015 staybank pd_prc lgd_prc pdlgd $loancontrols if  year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)	//includes 2014
	est store gsib5, title("")
	estadd local bankcounty "YES": gsib5
	estadd local loanchars "YES": gsib5
	estadd local banktimefx "YES": gsib5
	estadd local indtimefx "YES": gsib5

#delimit;
	esttab gsib1 gsib2 gsib3 gsib4 gsib5 using "TableGSIBOneStageNew.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (postnumgsib2015  pd_prc lgd_prc pdlgd) 
	order (postnumgsib2015  pd_prc lgd_prc pdlgd)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(5) collabels(none) substitute(_ \_) 
mgroups("Number of Banks" "log(Loan Volume)" "Interest Rate (\%)" "Probability of Default (\%)" "Interest Rate (\%)" , pattern(1 1 1 1 1) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars bankcounty banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %~12s %9.0fc %9.2f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}"  "\multicolumn{1}{c}{@}")
	label ("Loan Controls" "Bank-County FE" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;

#delimit cr			
		
			
*************************************************************************************************************************************************************************************
* Table XIII. GSIB Surcharges and Market Outcomes (Two-Stage Least Squares)
***********************************************************************************************************************************************************************************		

egen countygr = group(county)		
eststo clear
ivreghdfe logvol $loancontrols staybank (numbank=postnumgsib2015) if year>2014 & year <= 2019, absorb($fe bankcounty banktime) cluster(countygr)	
		est store gsib1, title("")
		estadd local bankcounty "YES": gsib1
		estadd local loanchars "YES": gsib1
		estadd local banktimefx "YES": gsib1
		estadd local indtimefx "YES": gsib1

ivreghdfe interest_rate_prc $loancontrols staybank (numbank=postnumgsib2015) if year>2014 & year <= 2019 , absorb($fe bankcounty banktime) cluster(countygr)				
		est store gsib2, title("")
		estadd local bankcounty "YES": gsib2
		estadd local loanchars "YES": gsib2
		estadd local banktimefx "YES": gsib2
		estadd local indtimefx "YES": gsib2

ivreghdfe pd_prc $loancontrols staybank (numbank=postnumgsib2015) if year>2014 & year <= 2019, absorb($fe bankcounty banktime) cluster(countygr)	
		est store gsib3, title("")
		estadd local bankcounty "YES": gsib3
		estadd local loanchars "YES": gsib3
		estadd local banktimefx "YES": gsib3
		estadd local indtimefx "YES": gsib3

ivreghdfe interest_rate_prc staybank pd_prc lgd_prc pdlgd $loancontrols (numbank=postnumgsib2015) if year>2014 & year <= 2019, absorb($fe bankcounty banktime) cluster(countygr)
		est store gsib4, title("")
		estadd local bankcounty "YES": gsib4
		estadd local loanchars "YES": gsib4
		estadd local banktimefx "YES": gsib4
		estadd local indtimefx "YES": gsib4
		
#delimit;
	esttab gsib1 gsib2 gsib3 gsib4 using "TableGSIBIvregNew.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (numbank pd_prc lgd_prc pdlgd) 
	order (numbank pd_prc lgd_prc pdlgd)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(4) collabels(none) substitute(_ \_) 
mgroups("log(Loan Volume)" "Interest Rate (\%)" "Probability of Default (\%)" "Interest Rate (\%)" , pattern(1 1 1 1) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars bankcounty banktimefx indtimefx N, fmt(%~12s %~12s %~12s %~12s %9.0fc )  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}"  "\multicolumn{1}{c}{@}" )
	label ("Loan Controls" "Bank-County FE" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" ))
;

#delimit cr	
drop countygr				
				
				
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 													  Figures - Main Paper															 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

cd "$datadir"
use "$master", clear


global loancontrols "logmaturity logamt guarantee"
global firmvarssize "size leverage tangibility profitability "
global fe "i.banktime i.iquarter i.credit_facility_purpose_cat i.line_variability "
global clustervar "taxid"


***************************************************************************************************************
* Figure 1. Average Realized Default Rates
***************************************************************************************************************
global default default1_prc

* 5 PDLGD buckets
drop pdbin mpd

xtile pdbin = pd_prc, nquantiles(5)
xtile irbin = interest_rate_prc, nquantiles(5)
		
reghdfe interest_rate_prc $loancontrols $loancontrols_interact, absorb($fe, savefe ) vce(cluster $clustervar) residual(res)

egen mdefault = mean($default), by(pdbin)
egen mdefaultir = mean($default), by(irbin) 
 
egen mpd = mean(pd_prc), by(pdbin) 
gen rmpd = round(mpd,.1)
egen mir = mean(interest_rate_prc), by(irbin) 
gen rmir = round(mir,.1)

		
graph bar mdefaultir, over(rmir) ytitle("Average Realized Default (%)") bgcolor(white) graphregion(color(white)) 
graph export "`$mainlatexdir\'mdefaultir5.png", replace

graph bar mdefault, over(rmpd) ytitle("Average Realized Default (%)")bgcolor(white) graphregion(color(white))
graph export "`$mainlatexdir\'mdefault5.png", replace


***************************************************************************************************************
* Figure 2. Number of Banks in the County and Interest Rates
***************************************************************************************************************
use "$master", clear

g ncat = 1 if numbankcounty==1
replace ncat = 2 if numbankcounty==2
replace ncat = 3 if 2<numbankcounty & numbankcounty<8
replace ncat = 4 if 7<numbankcounty & numbankcounty<13
replace ncat = 5 if 12<numbankcounty & numbankcounty<18
replace ncat = 6 if 17<numbankcounty 

sort ncat
tab ncat, gen(ncatdummy)
			
reghdfe interest_rate_prc ib(#1).ncat $loancontrols $firmvarssize , absorb($fe) vce(cluster county)
		
	#delimit ;
		coefplot,  drop(_cons $loancontrols $firmvarssize) xlabel( 1 "1" 2 "2" 3 "3-7" 4 "8-12" 5 "13-17" 6 "18-23",labsize(small) )   
		ciopts(lwidth(*1.5) recast(rcap) color(navy)) mcolor(navy) msize(*1.2)  levels(90) vertical 
		 baselevels yline(0, lcolor(maroon) lpattern(l)) omitted  bgcolor(white) graphregion(color(white)); 					
	#delimit cr

graph export "`$mainlatexdir\'IRfigure.png", as(png) replace	
 
***************************************************************************************************************
* Figure 3. Number of Banks in the County and PDs
***************************************************************************************************************
			
reghdfe pd_prc ib(#1).ncat $loancontrols $firmvarssize , absorb($fe) vce(cluster county)
		
	#delimit ;
		coefplot,  drop(_cons $loancontrols $firmvarssize) xlabel( 1 "1" 2 "2" 3 "3-7" 4 "8-12" 5 "13-17" 6 "18-23",labsize(small) )   
		ciopts(lwidth(*1.5) recast(rcap) color(navy)) mcolor(navy) msize(*1.2)  levels(90) vertical 
		 baselevels yline(0, lcolor(maroon) lpattern(l)) omitted  bgcolor(white) graphregion(color(white));
	#delimit cr	

graph export "`$mainlatexdir\'PD figure.png", as(png) replace			
								
								
*************************************************************************************************************************************************************************************
* Figure 4. Number of Banks in the County and Loan Volume
*************************************************************************************************************************************************************************************
use "$master", clear
preserve
	* at county-level
	keep numbankcounty numbankcounty1 logvol lognumloan lognumfirm logdensity logdensityy logwage logwagefin totvolcounty yq county numloancounty numfirmcounty
	duplicates drop

	reghdfe logvol numbankcounty numbankcounty1 logdensity logwage logwagefin, absorb(yq) vce(cluster: yq county)
	reghdfe logvol numbankcounty logdensityy logwage logwagefin, absorb(yq) vce(cluster: yq county)

	g ncat = 1 if numbankcounty==1
	replace ncat = 2 if numbankcounty==2
	replace ncat = 3 if 2<numbankcounty & numbankcounty<8
	replace ncat = 4 if 7<numbankcounty & numbankcounty<13
	replace ncat = 5 if 12<numbankcounty & numbankcounty<18
	replace ncat = 6 if 17<numbankcounty 

	sort ncat
	tab ncat, gen(ncatdummy)

	reghdfe logvol ib(#1).ncat logdensityy logwage logwagefin , absorb(yq) vce(cluster county yq)

	#delimit ;
	coefplot,  drop(_cons logdensityy logwage logwagefin) xlabel( 1 "1" 2 "2" 3 "3-7" 4 "8-12" 5 "13-17" 6 "18-23",labsize(small) )   
	ciopts(lwidth(*1.5) recast(rcap)  
	) msize(*1.2) graphregion(color(white))  recast(rcap) levels(90) vertical lwidth(*1.5) bgcolor(white) 
	xlabel(,labsize(small)) xsize(7) baselevels yline(0) omitted  bgcolor(white) graphregion(color(white))					;
	#delimit cr	

	graph export "`$mainlatexdir\'LoanVolCounty.png", as(png) replace			
restore 								

*************************************************************************************************************************************************************************************
* Figure 5. Market Structure and PD Accuracy
*************************************************************************************************************************************************************************************

egen mnumbankcounty = median(numbankcounty)
egen p25numbankcounty = pctile(numbankcounty), p(25)
egen p75numbankcounty = pctile(numbankcounty), p(75)

gen hnumbankcounty= (numbankcounty > mnumbankcounty)
gen h75numbankcounty= (numbankcounty > p75numbankcounty)

gen h75vs25numbankcount = h75numbankcounty
replace h75vs25numbankcount=. if numbankcounty<p75numbankcounty & numbankcounty>p25numbankcounty	// keeping only above 75% and below 25%

*default needs to be between 0 and 1 
gen default = default1_prc>0
gen np = delinq1_prc>0


* default vs pd
graph set window fontface "Liberation Sans"
 
*** Step 1: Calculate AUC values from roctaqb
qui roctab default pd_prc if hnumbankcounty==0
scalar scalararea0 = r(area)
local area0: display %8.3f `r(area)'
local area0 `area0'
local n0: display %10.0fc `r(N)'
local n0 `n0'
 
qui roctab default pd_prc if hnumbankcounty==1
scalar scalararea1 = r(area)
local area1: display %8.3f `r(area)'
local area1 `area1'
local n1: display %10.0fc `r(N)'
local n1 `n1'
 
*** Step 2: Calculate Chi2 and p-stats from roccomp
qui roccomp default pd_prc, by(hnumbankcounty)
local chi2: di %8.2f `r(chi2)'
local chi2 `chi2'
local pval: di %8.3f `r(p)'
local pval `pval'
 
scalar darea = abs(scalararea1 - scalararea0)
local darea: display %8.3f darea
local darea `darea'
 
*** Step 3: Plot everything
roccomp default pd_prc, by(hnumbankcounty) graph ///
plot1opts(msize(3pt)) plot2opts(msymbol(Dh) msize(3pt)) ///
text(0.30 0.75 "|{&Delta}AUC|: `darea'" "{&Chi}{sup:2} stat: `chi2'"  "p-value: `pval'",placement(c) size(medlarge)) ///
legend(pos(6) ring(1) col(1) bmargin(zero) size(medlarge) span order(1 "Low Number of Banks (AUC=`area0',N=`n0')" 2 "High Number of Banks (AUC=`area1', N=`n1')")) ///
xtitle("False positive rate", size(large)) ytitle("True positive rate", size(large)) ///
yl(,nogrid labsize(large)) xl(,nogrid labsize(large)) ///
xsize(4) ysize(3.5) graphregion(color(white)) bgcolor(white)

graph export "`$mainlatexdir\'ROC_Default_PD.png", replace

* nonperforming vs pd
graph set window fontface "Liberation Sans"
 
*** Step 1: Calculate AUC values from roctaqb
qui roctab np pd_prc if hnumbankcounty==0
scalar scalararea0 = r(area)
local area0: display %8.3f `r(area)'
local area0 `area0'
local n0: display %10.0fc `r(N)'
local n0 `n0'
 
qui roctab np pd_prc if hnumbankcounty==1
scalar scalararea1 = r(area)
local area1: display %8.3f `r(area)'
local area1 `area1'
local n1: display %10.0fc `r(N)'
local n1 `n1'
 
*** Step 2: Calculate Chi2 and p-stats from roccomp
qui roccomp np pd_prc, by(hnumbankcounty)
local chi2: di %8.2f `r(chi2)'
local chi2 `chi2'
local pval: di %8.3f `r(p)'
local pval `pval'
 
scalar darea = abs(scalararea1 - scalararea0)
local darea: display %8.3f darea
local darea `darea'
 
*** Step 3: Plot everything
roccomp np pd_prc, by(hnumbankcounty) graph ///
plot1opts(msize(3pt)) plot2opts(msymbol(Dh) msize(3pt)) ///
text(0.30 0.75 "|{&Delta}AUC|: `darea'" "{&Chi}{sup:2} stat: `chi2'"  "p-value: `pval'",placement(c) size(medlarge)) ///
legend(pos(6) ring(1) col(1) bmargin(zero) size(medlarge) span order(1 "Low Number of Banks (AUC=`area0',N=`n0')" 2 "High Number of Banks (AUC=`area1', N=`n1')")) ///
xtitle("False positive rate", size(large)) ytitle("True positive rate", size(large)) ///
yl(,nogrid labsize(large)) xl(,nogrid labsize(large)) ///
xsize(4) ysize(3.5)  graphregion(color(white)) bgcolor(white)

graph export "`$mainlatexdir\'ROC_NonPerforming_PD.png", replace


*************************************************************************************************************************************************************************************
* Figure 6. The Frequency of Firms Staying With Their Existing Banks
*************************************************************************************************************************************************************************************
use "$master", clear

g ncat = 1 if numbankcounty==1
replace ncat = 2 if numbankcounty==2
replace ncat = 3 if 2<numbankcounty & numbankcounty<8
replace ncat = 4 if 7<numbankcounty & numbankcounty<13
replace ncat = 5 if 12<numbankcounty & numbankcounty<18
replace ncat = 6 if 17<numbankcounty 

sort ncat
tab ncat, gen(ncatdummy)

g staybankprc =  staybank*100
graph bar staybankprc  if numbankcounty > 1, over(ncat, relabel(1 "2" 2 "3-7" 3 "8-12" 4 "13-17" 5 "18-23")) ytitle("Stay Bank (%)") bar(1, color(navy))  bgcolor(white) graphregion(color(white))

graph export "`$mainlatexdir\'changebank figure2.png", as(png) replace		

*************************************************************************************************************************************************************************************
* Figure 7. The Frequency of Firms Staying With Their Existing Banks (Regression Analysis)
*************************************************************************************************************************************************************************************
reghdfe staybank ib(#1).ncat $loancontrols $firmvarssize if numbankcounty > 1 ,  absorb($fe) vce(cluster county)
#delimit ;
		coefplot,  drop( _cons $loancontrols $firmvarssize) xlabel( 1 "2" 2 "3-7" 3 "8-12" 4 "13-17" 5 "18-23",labsize(small) )   
		ciopts(lwidth(*1.5) recast(rcap) color(navy)) mcolor(navy) msize(*1.2)  levels(90) vertical 
		 baselevels yline(0, lcolor(maroon) lpattern(l)) omitted  bgcolor(white) graphregion(color(white)); 				
	#delimit cr	
graph export "`$mainlatexdir\'changebank figure.png", as(png) replace					
	

*************************************************************************************************************************************************************************************
* Figure 8. Number of Banks in the County and Markups
*************************************************************************************************************************************************************************************

reghdfe interest_rate_prc ib(#1).ncat pd_prc lgd_prc pdlgd $loancontrols $firmvarssize , absorb($fe) vce(cluster county)
		
	#delimit ;
		coefplot,  drop(_cons pd_prc lgd_prc pdlgd $loancontrols $firmvarssize ) xlabel( 1 "1" 2 "2" 3 "3-7" 4 "8-12" 5 "13-17" 6 "18-23",labsize(small) )   
		ciopts(lwidth(*1.5) recast(rcap) color(navy)) mcolor(navy) msize(*1.2)  levels(90) vertical 
		 baselevels yline(0, lcolor(maroon) lpattern(l)) omitted  bgcolor(white) graphregion(color(white));
	#delimit cr	

graph export "`$mainlatexdir\'OneStageMainFigure.png", as(png) replace		


*************************************************************************************************************************************************************************************
* Figure 9. The Effect of GSIB Surcharges on the Number of Banks.
*************************************************************************************************************************************************************************************
reghdfe numbank numgsib2015 ib(#1).year#c.numgsib2015 staybank $loancontrols if year>2014 & year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
					
	#delimit ;
		coefplot,  drop(_cons numgsib2015 staybank $loancontrols) xlabel( 1 "2015" 2 "2016" 3 "2017" 4 "2018" 5 "2019" ,labsize(small) )   
		ciopts(lwidth(*1.5) recast(rcap) color(navy)) mcolor(navy) msize(*1.2)  levels(90) vertical 
		 baselevels yline(0, lcolor(maroon) lpattern(l)) omitted  bgcolor(white) graphregion(color(white)); 
	#delimit cr	
						
graph export "`$mainlatexdir\'FigGSIBNumbank.png", as(png) replace

*************************************************************************************************************************************************************************************
* Figure 10. The Effect of GSIB Surcharges on the Number of GSIB and Non-GSIBs
*************************************************************************************************************************************************************************************
reghdfe numgsib numgsib2015 ib(#1).year#c.numgsib2015 staybank $loancontrols if year>2014 & year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
estimates store GSIB
reghdfe numnongsib numgsib2015 ib(#1).year#c.numgsib2015 staybank $loancontrols if year>2014 & year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
estimates store NonGSIB

	#delimit ;
		coefplot GSIB NonGSIB, drop(_cons numgsib2015 staybank $loancontrols) xlabel( 1 "2015" 2 "2016" 3 "2017" 4 "2018" 5 "2019" ,labsize(small) )   
		ciopts(lwidth(*1.5) recast(rcap)) msize(*1.2) graphregion(color(white))  recast(rcap) levels(90) vertical lwidth(*1.5) bgcolor(white) 
		xlabel(,labsize(small)) xsize(7) baselevels yline(0) omitted   bgcolor(white) graphregion(color(white)) legend(order(2 "Number of GSIBs" 4 "Number of Non-GSIBs") region(col(white)))		;
	#delimit cr	
						
graph export "`$mainlatexdir\'FigGSIBNumbankByTypeV1.png", as(png) replace

*************************************************************************************************************************************************************************************
* Figure 11. The Effect of GSIB Surcharges on Lending Volume
*************************************************************************************************************************************************************************************
reghdfe logvol numgsib2015 ib(#2).year#c.numgsib2015 if  year <= 2019, absorb(county year) vce(cluster county)		
		
	#delimit ;
		coefplot,  drop(_cons numgsib2015 staybank $loancontrols) xlabel( 1 "2014" 2 "2015" 3 "2016" 4 "2017" 5 "2018" 6 "2019" ,labsize(small) ) 
		ciopts(lwidth(*1.5) recast(rcap)  
		) msize(*1.2) graphregion(color(white))  recast(rcap) levels(90) vertical lwidth(*1.5) bgcolor(white) 
		xlabel(,labsize(small)) xsize(7) baselevels yline(0) omitted   bgcolor(white) graphregion(color(white));
	#delimit cr	
				
graph export "`$mainlatexdir\'FigGSIB1logvolquarterly.png", as(png) replace			

*************************************************************************************************************************************************************************************
* Figure 12. The Effect of GSIB Surcharges on Interest Rates
*************************************************************************************************************************************************************************************
reghdfe interest_rate_prc numgsib2015 ib(#2).year#c.numgsib2015 staybank $loancontrols if year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
					
	#delimit ;
		coefplot,  drop(_cons numgsib2015 staybank $loancontrols) xlabel( 1 "2014" 2 "2015" 3 "2016" 4 "2017" 5 "2018" 6 "2019"   ,labsize(small) )   
		ciopts(lwidth(*1.5) recast(rcap)  
		) msize(*1.2) graphregion(color(white))  recast(rcap) levels(90) vertical lwidth(*1.5) bgcolor(white) 
		xlabel(,labsize(small)) xsize(7) baselevels yline(0) omitted   bgcolor(white) graphregion(color(white));
	#delimit cr	
						
graph export "`$mainlatexdir\'FigGSIB2ir.png", as(png) replace			
			
*************************************************************************************************************************************************************************************
* Figure 13. The Effect of GSIB Surcharges on PDs
*************************************************************************************************************************************************************************************
reghdfe pd_prc numgsib2015 ib(#2).year#c.numgsib2015 staybank $loancontrols if year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
					
	#delimit ;
		coefplot,  drop(_cons numgsib2015 staybank $loancontrols) xlabel( 1 "2014" 2 "2015" 3 "2016" 4 "2017" 5 "2018" 6 "2019"   ,labsize(small) )    
		ciopts(lwidth(*1.5) recast(rcap)  
		) msize(*1.2) graphregion(color(white))  recast(rcap) levels(90) vertical lwidth(*1.5) bgcolor(white) 
		xlabel(,labsize(small)) xsize(7) baselevels yline(0) omitted   bgcolor(white) graphregion(color(white));
	#delimit cr	
						
graph export "`$mainlatexdir\'FigGSIB3pd.png", as(png) replace
				
*************************************************************************************************************************************************************************************
* Figure 14. The Effect of GSIB Surcharges on Markups
*************************************************************************************************************************************************************************************
reghdfe interest_rate_prc numgsib2015 ib(#2).year#c.numgsib2015 staybank pd_prc lgd_prc pdlgd $loancontrols if  year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
					
	#delimit ;
		coefplot,  drop(_cons numgsib2015 staybank pd_prc lgd_prc pdlgd $loancontrols) xlabel( 1 "2014" 2 "2015" 3 "2016" 4 "2017" 5 "2018" 6 "2019"   ,labsize(small) )  
		ciopts(lwidth(*1.5) recast(rcap)  
		) msize(*1.2) graphregion(color(white))  recast(rcap) levels(90) vertical lwidth(*1.5) bgcolor(white) 
		xlabel(,labsize(small)) xsize(7) baselevels yline(0) omitted   bgcolor(white) graphregion(color(white));
	#delimit cr	
						
graph export "`$mainlatexdir\'FigGSIB4ir.png", as(png) replace			
			

			
			
			
			
			
				
				
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 													  Internet Appendix (1)															 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

* This section produces Internet Appendix tables and figures excluding Tables 2, 6, 7, 8, 10 and Figure 2

cd "$datadir"
use "$master", clear


cd "$ialatexdir"
	
***************************************************************************************************************
* Labeling variables, values, etc.
***************************************************************************************************************
la var interest_rate_prc "Interest Rate (\%)"
la var maturity "Maturity (months)"
la var logmaturity "Log(maturity)"
la var amt "Loan Amount (USD)"
la var amtm "Amount (million USD)"
la var logamt "Log(Amount)"
la var secured "Secured"
la var collateraltype5 "Secured by Blanket Lien"
la var guarantee "Guaranteed"
la var syndicated "Syndicated"
la var variability_float "Floating Interest Rate"
la var type_line_all "Line of Credit"
la var wrc "With Rate Ceiling"
la var wrf "With Rate Roof"

*la var borrower_rating "Internal Credit Rating"
la var pd_prc "Probability of Default (\%)"
la var lgd_prc "Loss Given Default (\%)"
la var pdlgd "Expected Loss (\%)"
la var pdlgd2 "Expected Loss\^2"

la var default1_prc "Realized Default (\%)"
la var delinq1_prc "Non-Performance (\%)"
la var default2_prc "Realized Default (2yr) (\%)"
la var delinq2_prc "Non-Performance (2yr) (\%)"

la var sizem "Assets (million USD)"
la var size "Log(Assets)"
la var leverage "Leverage"
la var tangibility "Tangibility"
la var profitability "Profitability"
la var salesm "Net Sales (million USD)"

la var numbankcounty "Number of Banks"
la var numbankcounty1 "One Bank"
la var numbankmsa  "Number of Banks MSA"
la var numbankmsa1 "One Bank MSA"
la var numbankbranch "Number of All Banks"
la var numbankline "Number of Banks $\times$ Line of Credit"
la var numbankline1 "One Bank $\times$ Line of Credit"		

la var hhimsa "MSA Loan HHI"
la var hhi "Loan HHI"
la var logdensity "Population Density"
la var logdensityy "Population Density"
la var logpop "Population"
la var logpopy "Population"
la var wage "Wages"
la var wagefin "Financial Industry Wages"
la var logwage "Wages"
la var logrent "Rent"
la var logwagefin "Financial Industry Wages"
la var avgherfdepcty "Deposit HHI"

la var changebank "Change Bank"
la var changenbankc "Change Bank $\times$ N Bank County"
la var changenbankmsa "Change Bank $\times$ N Bank MSA"
la var changenbankc "Change Bank $\times$ N Bank County"
la var changenbankmsa "Change Bank $\times$ N Bank MSA"

la var staybank "Stay Bank"
la var staynbankc "Stay Bank $\times$ N Bank County"
la var staynbankmsa "Stay Bank $\times$ N Bank MSA"
la var staynbankc "Stay Bank $\times$ N Bank County"
la var staynbankmsa "Stay Bank $\times$ N Bank MSA"

la var changesize "Change Bank $\times$ Log(Assets)"
la var changeleverage "Change Bank $\times$ Leverage"
la var changetangibility "Change Bank $\times$ Tangibility"
la var changeprofitability "Change Bank $\times$ Profitability"

la var staysize "Stay Bank $\times$ Log(Assets)"
la var stayleverage "Stay Bank $\times$ Leverage"
la var staytangibility "Stay Bank $\times$ Tangibility"
la var stayprofitability "Stay Bank $\times$ Profitability"

la var numprevlenders "Number of Prior Lenders"
la var staynumprev "Stay Bank $\times$ Number of Prior Lenders"

la var gsib "GSIB"
la var staybank "Stay Bank"
la var first "New Borrower"

la var numgsib2015	"Number of GSIBs (2015)"
la var postnumgsib2015 "Post $\times$ Number of GSIBs (2015)"
la var postratiogsib2015 "Post $\times$ Ratio of GSIBs (2015)"
la var numbank "Number of Banks (Annual)"
la var gsibpost "GSIB$\times$ Post"
la var staygsib "Stay Bank $\times$ GSIB"
la var postnumgsib2015stay "Post $\times$ Number of GSIBs (2015) $\times$ Stay Bank"
la var postnumgsib2015gsib "Post $\times$ Number of GSIBs (2015) $\times$ GSIB"
la var postnumgsib2015staygsib "Post $\times$ Number of GSIBs (2015) $\times$ Stay Bank $\times$ GSIB"
la var staynumprevlend "Stay Bank $\times$ Number of Prior Lenders"
la var postnumgsib2015numprevlend "Post $\times$ Number of GSIBs (2015) $\times$ Number of Prior Lenders"
la var postnumgsib2015staynumprevlend "Post $\times$ Number of GSIBs (2015) $\times$ Stay Bank $\times$ Number of Prior Lenders"
la var postlogpop "Post $\times$ Population"
la var postlogpopy "Post $\times$ Population"

la var numprevlenders "Number of Prior Lenders"
la var logempgrowth "Jobs Growth"
la var logbagrowth "Business Applications Growth"
la var logfinempgrowth "Financial Industry Jobs Growth"
la var logwagegrowth "Wages Growth"
la var logfinwagegrowth "Financial Industry Wages Growth"
la var numbranchpop "Number of Branches Per Capita"
la var logestgrowth "Number of Establishments Growth"
la var logba "Business Applications"
la var bapop "Business Applications Per Capita"
la var estpop "Establishment Per Capita"

la var pdlogmaturity "Probability of Default (\%) $\times$ Log(maturity)"
la var pdlogamt "Probability of Default (\%) $\times$ Log(Amount)"
la var pdguarantee "Probability of Default (\%) $\times$ Guaranteed"
la var pdcollateraltype5 "Probability of Default (\%) $\times$ Secured by Blanket Lien"
la var pdsize "Probability of Default (\%) $\times$ Log(Assets)"
la var pdleverage "Probability of Default (\%) $\times$ Leverage"
la var pdtangibility "Probability of Default (\%) $\times$ Tangibility"
la var pdprofitability "Probability of Default (\%) $\times$ Profitability"
la var lgdlogmaturity "Loss Given Default (\%) $\times$ Log(maturity)"
la var lgdlogamt "Loss Given Default (\%) $\times$ Log(Amount)"
la var lgdguarantee "Loss Given Default (\%) $\times$ Guaranteed"
la var lgdcollateraltype5 "Loss Given Default (\%) $\times$ Secured by Blanket Lien"
la var lgdsize "Loss Given Default (\%) $\times$ Log(Assets)"
la var lgdleverage "Loss Given Default (\%) $\times$ Leverage"
la var lgdtangibility "Loss Given Default (\%) $\times$ Tangibility"
la var lgdprofitability "Loss Given Default (\%) $\times$ Profitability"	     
la var numbankcountylogmaturity "Number of Banks $\times$ Log(maturity)"
la var numbankcountylogamt "Number of Banks $\times$ Log(Amount)"
la var numbankcountyguarantee "Number of Banks $\times$ Guaranteed"
la var numbankcountycollateraltype5 "Number of Banks $\times$ Secured by Blanket Lien"
la var numbankcountysize "Number of Banks $\times$ Log(Assets)"
la var numbankcountyleverage "Number of Banks $\times$ Leverage"
la var numbankcountytangibility "Number of Banks $\times$ Tangibility"
la var numbankcountyprofitability "Number of Banks $\times$ Profitability"
la var numbankcountypd "Number of Banks $\times$ Probability of Default (\%)"
la var numbankcountylgd "Number of Banks $\times$ Loss Given Default (\%)"
la var numbankcountypdlgd "Number of Banks $\times$ Expected Loss (\%)"

la var logmaturity2 "Log(maturity)\textsuperscript{2}"
la var logamt2 "Log(Amount)\textsuperscript{2}"
la var guarantee2 "Guaranteed\textsuperscript{2}"
la var size2 "Log(Assets)\textsuperscript{2}"
la var leverage2 "Leverage\textsuperscript{2}"
la var tangibility2 "Tangibility\textsuperscript{2}"
la var profitability2 "Profitability\textsuperscript{2}"
la var logdensityy2 "Population Density\textsuperscript{2}"
la var logwage2 "Wages\textsuperscript{2}"
la var logwagefin2 "Financial Industry Wages\textsuperscript{2}"
la var logpop2 "Population\textsuperscript{2}"
la var logpopy2 "Population\textsuperscript{2}"
la var logmaturity3 "Log(maturity)\textsuperscript{3}"
la var logamt3 "Log(Amount)\textsuperscript{3}"
la var guarantee3 "Guaranteed\textsuperscript{3}"
la var size3 "Log(Assets)\textsuperscript{3}"
la var leverage3 "Leverage\textsuperscript{3}"
la var tangibility3 "Tangibility\textsuperscript{3}"
la var profitability3 "Profitability\textsuperscript{3}"
la var logdensityy3 "Population Density\textsuperscript{3}"
la var logwage3 "Wages\textsuperscript{3}"
la var logwagefin3 "Financial Industry Wages\textsuperscript{3}"
la var logpop3 "Population\textsuperscript{3}"

***************************************************************************************************************
* Global variables
***************************************************************************************************************
global loancontrols "logmaturity logamt guarantee"		//Loan charactersitics
global firmvarssize "size leverage tangibility profitability "		//Firm charactersitics
global fe "i.banktime i.iquarter i.credit_facility_purpose_cat i.line_variability "		//Fixed effects 1
global fenobanktime "i.iquarter i.credit_facility_purpose_cat i.line_variability "		//Fixed effects 2
global clustervar "taxid"	//Cluster varible, firm ID
global pdinteracts "pdlogmaturity pdlogamt pdguarantee pdsize pdleverage pdtangibility pdprofitability"
global lgdinteracts "lgdlogmaturity lgdlogamt lgdguarantee lgdsize lgdleverage lgdtangibility lgdprofitability"
global numbankcountyinteracts "numbankcountylogmaturity numbankcountylogamt numbankcountyguarantee numbankcountysize numbankcountyleverage numbankcountytangibility numbankcountyprofitability"
global numbankcountyriskinteracts "numbankcountypd numbankcountylgd numbankcountypdlgd"
global squareds = "logmaturity logamt guarantee size leverage tangibility profitability logdensityy logwage logwagefin logmaturity2 logamt2 size2 leverage2 tangibility2 profitability2 logdensityy2 logwage2 logwagefin2"
global ylimit 2020
global qlimit 1

*********************************************************************************************************************************
* Online Appendix Figure 1: Distribution of Loan Observations Across Market Structures	
*********************************************************************************************************************************
* Number of observations per bucket
g ncat = 1 if numbankcounty==1
replace ncat = 2 if numbankcounty==2
replace ncat = 3 if 2<numbankcounty & numbankcounty<8
replace ncat = 4 if 7<numbankcounty & numbankcounty<13
replace ncat = 5 if 12<numbankcounty & numbankcounty<18
replace ncat = 6 if 17<numbankcounty 

sort ncat
tab ncat, gen(ncatdummy)
graph bar (count), over(ncat, relabel( 1 "1" 2 "2" 3 "3-7" 4 "8-12" 5 "13-17" 6 "18-23")) ytitle("Number of Loans")  bgcolor(white) graphregion(color(white)) 
graph export "NLoansInBucket.png", as(png) replace

*********************************************************************************************************************************
* Online Appendix Table 1: Correlation Between Loan Characteristics	
*********************************************************************************************************************************
corrtex interest_rate_prc $loancontrols type_line_all variability_float sen_secured, file(Loanchars_corr3.tex) digits(2) replace

*********************************************************************************************************************************
* Online Appendix Table 2: Summary Statistics (Top Quartile of Number of Banks Versus the Rest of Sample)	
*********************************************************************************************************************************
* Provided in a separate file

*********************************************************************************************************************************
* Online Appendix Table 3: Market Structure and Bank Specialization	
*********************************************************************************************************************************
* Specialization 		
		
eststo clear 	
reghdfe specialized numbankcounty $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe specialized numbankcounty  $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe specialized numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3 county5 using "TableSpecialization.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Specialization",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

*********************************************************************************************************************************
* Online Appendix Table 4: Market Structure and Relationship Dynamics	
*********************************************************************************************************************************
		
eststo clear 
reghdfe interest_rate_prc first nbankfirst $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe cyq) vce(cluster county)
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local countyquarter "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			estadd local quartertimefx "YES": county1
			
			
reghdfe pd_prc first nbankfirst $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe cyq) vce(cluster county)
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local countyquarter "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			estadd local quartertimefx "YES": county3
			
reghdfe interest_rate_prc first nbankfirst  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe cyq) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local countyquarter "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5
			estadd local quartertimefx "YES": county5

#delimit;
	esttab county1  county3  county5  using "TableExtMarginAll.tex", replace eqlabels(none) nomtitles nodepvars label 
			keep ( first nbankfirst  pd_prc lgd_prc pdlgd $firmvarssize) 
			order ( first nbankfirst  pd_prc lgd_prc pdlgd $firmvarssize) 
			cells(b(star fmt(3)) t(fmt(3) par abs)) 
			booktabs modelwidth(3) collabels(none) substitute(_ \_) 
		mgroups("Interest Rate (\%)" "Probability of Default (\%)" "Interest Rate (\%)",  pattern(1 1 1) 
			prefix(\multicolumn{@span}{c}{) suffix(})   
			span erepeat(\cmidrule(lr){@span}))         
		style(tex) star(* 0.10 ** 0.05 *** 0.01) 
		alignment(D{.}{.}{4,6}) 
		stats(loanchars countyquarter banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %~12s %9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
			label ("Loan Characteristics Controls" "County-Quarter FE" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr	

*********************************************************************************************************************************
* Online Appendix Table 5: Market Structure and Interest Rates (The Effect of Bank-Time Fixed Effects)
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
reghdfe interest_rate_prc numbankcounty $loancontrols , absorb($fenobanktime) vce(cluster county) 
			est store county2, title("")
			estadd local loanchars "YES": county2
			estadd local banktimefx "": county2
			estadd local indtimefx "YES": county2
			estadd local loantypefx "YES": county2
			estadd local loanpurposefx "YES": county2
			
			
reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			
reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize , absorb($fenobanktime) vce(cluster county) 
			est store county4, title("")
			estadd local loanchars "YES": county4
			estadd local banktimefx "": county4
			estadd local indtimefx "YES": county4
			estadd local loantypefx "YES": county4
			estadd local loanpurposefx "YES": county4
			

reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5
			
reghdfe interest_rate_prc numbankcounty $loancontrols $firmvarssize logdensityy logwage logwagefin, absorb($fenobanktime) vce(cluster county) 
			est store county6, title("")
			estadd local loanchars "YES": county6
			estadd local banktimefx "": county6
			estadd local indtimefx "YES": county6
			estadd local loantypefx "YES": county6
			estadd local loanpurposefx "YES": county6

			
#delimit;
			esttab county1 county2 county3 county4 county5 county6 using "TableNoBankTimeFE.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(6) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0 0 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr	
	

*********************************************************************************************************************************
* Online Appendix Table 6: Market Structure and Interest Rates (Public Firms)	
*********************************************************************************************************************************
* Provided in a separate file

*********************************************************************************************************************************
* Online Appendix Table 7: Market Structure and Borrower Risk (Public Firms)	
*********************************************************************************************************************************
* Provided in a separate file

*********************************************************************************************************************************
* Online Appendix Table 8: Market Structure and Markups (Public Firms)	
*********************************************************************************************************************************
* Provided in a separate file	
	
*********************************************************************************************************************************
* Online Appendix Table 9: Market Structure and Lending Outcomes Controlling for Rental Rates
*********************************************************************************************************************************
eststo clear 	

la var logrent "Rent"

reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize logdensity logwage logwagefin logrent, absorb($fe) vce(cluster county)
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logrent, absorb($fe) vce(cluster county)
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin logrent, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableLogRent.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin logrent) 
					order (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin logrent) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)" "Probability of Default (\%)" "Interest Rate (\%)",  pattern(1 1 1) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr	

*********************************************************************************************************************************
* Online Appendix Table 10: Matching Counties	
*********************************************************************************************************************************
* Provided in a separate file

*********************************************************************************************************************************
* Online Appendix Table 11: Market Structure (Based on Branch Data) and Interest Rates
*********************************************************************************************************************************
eststo clear 	

la var numbankbranch "Number of All Banks"

reghdfe interest_rate_prc numbankbranch $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankbranch  $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankbranch  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3 county5 using "TableNumbankbranchIR.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankbranch   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankbranch   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		
			
					

*********************************************************************************************************************************
* Online Appendix Table 12: Market Structure (Based on Branch Data) and Borrower Risk
*********************************************************************************************************************************
eststo clear 	

reghdfe pd_prc numbankbranch $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe pd_prc numbankbranch  $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			
reghdfe pd_prc numbankbranch  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableNumbankbranchPD.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankbranch   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankbranch   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Probability of Default (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr	


*********************************************************************************************************************************
* Online Appendix Table 13: Market Structure (Based on Branch Data) and Markups
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankbranch pd_prc lgd_prc pdlgd $loancontrols , absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankbranch  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankbranch  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableNumbankbranchIROneStage.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankbranch  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					order (numbankbranch  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


*********************************************************************************************************************************
* Online Appendix Table 14: Market Concentration and Interest Rates
*********************************************************************************************************************************		
eststo clear 	
							
* hhi (county)
reghdfe interest_rate_prc hhi $loancontrols , absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			
reghdfe interest_rate_prc hhi $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county4, title("")
			estadd local loanchars "YES": county4
			estadd local banktimefx "YES": county4
			estadd local indtimefx "YES": county4
			estadd local loantypefx "YES": county4
			estadd local loanpurposefx "YES": county4

* Numbank MSA
reghdfe interest_rate_prc numbankmsa $loancontrols , absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5
			
reghdfe interest_rate_prc numbankmsa $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county6, title("")
			estadd local loanchars "YES": county6
			estadd local banktimefx "YES": county6
			estadd local indtimefx "YES": county6
			estadd local loantypefx "YES": county6
			estadd local loanpurposefx "YES": county6
			
			
* hhi (MSA-level)
reghdfe interest_rate_prc hhimsa $loancontrols , absorb($fe) vce(cluster county) 
			est store county7, title("")
			estadd local loanchars "YES": county7
			estadd local banktimefx "YES": county7
			estadd local indtimefx "YES": county7
			estadd local loantypefx "YES": county7
			estadd local loanpurposefx "YES": county7
			
reghdfe interest_rate_prc hhimsa $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county8, title("")
			estadd local loanchars "YES": county8
			estadd local banktimefx "YES": county8
			estadd local indtimefx "YES": county8
			estadd local loantypefx "YES": county8
			estadd local loanpurposefx "YES": county8
			
* Doposit HHI
reghdfe interest_rate_prc avgherfdepcty $loancontrols , absorb($fe) vce(cluster county) 
			est store county9, title("")
			estadd local loanchars "YES": county9
			estadd local banktimefx "YES": county9
			estadd local indtimefx "YES": county9
			estadd local loantypefx "YES": county9
			estadd local loanpurposefx "YES": county9
			
reghdfe interest_rate_prc avgherfdepcty  $loancontrols  $firmvarssize , absorb($fe) vce(cluster county) 
			est store county10, title("")
			estadd local loanchars "YES": county10
			estadd local banktimefx "YES": county10
			estadd local indtimefx "YES": county10
			estadd local loantypefx "YES": county10
			estadd local loanpurposefx "YES": county10
			
#delimit;
			esttab county3 county4 county5 county6 county7 county8 county9 county10 using "OtherConcentration.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (hhi numbankmsa hhimsa avgherfdepcty $firmvarssize) 
					order (hhi numbankmsa hhimsa avgherfdepcty $firmvarssize) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(8) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0 0 0 0 0 0 0 ) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr


*********************************************************************************************************************************
* Online Appendix Table 15: Market Structure and Interest Rates (Credit Lines Only)
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty $loancontrols if type_line_all, absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize if type_line_all, absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin if type_line_all, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3 county5 using "TableNumbankIRLine.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


	
*********************************************************************************************************************************
* Online Appendix Table 16: Market Structure and Markups (Credit Lines Only)
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols if type_line_all, absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize if type_line_all, absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin if type_line_all, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableNumbankIROneStageLineOnly.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr	
		
		
*********************************************************************************************************************************
* Online Appendix Table 17: Market Structure and Interest Rates (Term Loans Only)
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty $loancontrols if !type_line_all, absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize if !type_line_all, absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin if !type_line_all, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3 county5 using "TableNumbankIRTerm.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


*********************************************************************************************************************************
* Online Appendix Table 18: Market Structure and Markups (Term Loans Only)
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols if !type_line_all, absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize if !type_line_all, absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin if !type_line_all, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableNumbankIROneStageTerm.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr				
		
		
*********************************************************************************************************************************
* Online Appendix Table 19: Market Structure and Interest Rates with Additional Loan Demand Controls
*********************************************************************************************************************************
eststo clear  	

reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logempgrowth, absorb($fe) vce(cluster county) 
	est store reg1, title("")
	estadd local loanchars "YES": reg1
	estadd local banktimefx "YES": reg1
	estadd local indtimefx "YES": reg1
	estadd local loantypefx "YES": reg1
	estadd local loanpurposefx "YES": reg1
													
reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logwagegrowth, absorb($fe) vce(cluster county) 
	est store reg2, title("")
	estadd local loanchars "YES": reg2
	estadd local banktimefx "YES": reg2
	estadd local indtimefx "YES": reg2
	estadd local loantypefx "YES": reg2
	estadd local loanpurposefx "YES": reg2
									
reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin estpop, absorb($fe) vce(cluster county) 
	est store reg3, title("")
	estadd local loanchars "YES": reg3
	estadd local banktimefx "YES": reg3
	estadd local indtimefx "YES": reg3
	estadd local loantypefx "YES": reg3
	estadd local loanpurposefx "YES": reg3
											
reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin bapop, absorb($fe) vce(cluster county) 
	est store reg4, title("")
	estadd local loanchars "YES": reg4
	estadd local banktimefx "YES": reg4
	estadd local indtimefx "YES": reg4
	estadd local loantypefx "YES": reg4
	estadd local loanpurposefx "YES": reg4

reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logestgrowth, absorb($fe) vce(cluster county) 
	est store reg5, title("")
	estadd local loanchars "YES": reg5
	estadd local banktimefx "YES": reg5
	estadd local indtimefx "YES": reg5
	estadd local loantypefx "YES": reg5
	estadd local loanpurposefx "YES": reg5
					
reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logbagrowth, absorb($fe) vce(cluster county) 
	est store reg6, title("")
	estadd local loanchars "YES": reg6
	estadd local banktimefx "YES": reg6
	estadd local indtimefx "YES": reg6
	estadd local loantypefx "YES": reg6
	estadd local loanpurposefx "YES": reg6
									
reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin numbranchpop, absorb($fe) vce(cluster county) 
	est store reg7, title("")
	estadd local loanchars "YES": reg7
	estadd local banktimefx "YES": reg7
	estadd local indtimefx "YES": reg7
	estadd local loantypefx "YES": reg7
	estadd local loanpurposefx "YES": reg7
					
	
#delimit;
esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 using "TableNumbankIRDemand.tex", replace eqlabels(none) nomtitles nodepvars label 
keep (numbankcounty $firmvarssize logdensityy logwage logwagefin logempgrowth logwagegrowth estpop bapop logestgrowth logbagrowth numbranchpop) 
order (numbankcounty $firmvarssize logdensityy logwage logwagefin logempgrowth logwagegrowth estpop bapop logestgrowth logbagrowth numbranchpop) 
cells(b(star fmt(3)) t(fmt(3) par abs)) 
booktabs modelwidth(7) collabels(none) substitute(_ \_) 
mgroups("Interest Rate (\%)",  pattern(1 0 0 0 0 0 0) 
prefix(\multicolumn{@span}{c}{) suffix(})   
span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


*********************************************************************************************************************************
* Online Appendix Table 20: Market Structure and Borrower Risk with Additional Loan Demand Controls
*********************************************************************************************************************************
eststo clear  	

reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logempgrowth, absorb($fe) vce(cluster county) 
	est store reg1, title("")
	estadd local loanchars "YES": reg1
	estadd local banktimefx "YES": reg1
	estadd local indtimefx "YES": reg1
	estadd local loantypefx "YES": reg1
	estadd local loanpurposefx "YES": reg1
													
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logwagegrowth, absorb($fe) vce(cluster county) 
	est store reg2, title("")
	estadd local loanchars "YES": reg2
	estadd local banktimefx "YES": reg2
	estadd local indtimefx "YES": reg2
	estadd local loantypefx "YES": reg2
	estadd local loanpurposefx "YES": reg2
									
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin estpop, absorb($fe) vce(cluster county) 
	est store reg3, title("")
	estadd local loanchars "YES": reg3
	estadd local banktimefx "YES": reg3
	estadd local indtimefx "YES": reg3
	estadd local loantypefx "YES": reg3
	estadd local loanpurposefx "YES": reg3
											
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin bapop, absorb($fe) vce(cluster county) 
	est store reg4, title("")
	estadd local loanchars "YES": reg4
	estadd local banktimefx "YES": reg4
	estadd local indtimefx "YES": reg4
	estadd local loantypefx "YES": reg4
	estadd local loanpurposefx "YES": reg4

reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logestgrowth, absorb($fe) vce(cluster county) 
	est store reg5, title("")
	estadd local loanchars "YES": reg5
	estadd local banktimefx "YES": reg5
	estadd local indtimefx "YES": reg5
	estadd local loantypefx "YES": reg5
	estadd local loanpurposefx "YES": reg5
					
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin logbagrowth, absorb($fe) vce(cluster county) 
	est store reg6, title("")
	estadd local loanchars "YES": reg6
	estadd local banktimefx "YES": reg6
	estadd local indtimefx "YES": reg6
	estadd local loantypefx "YES": reg6
	estadd local loanpurposefx "YES": reg6
									
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin numbranchpop, absorb($fe) vce(cluster county) 
	est store reg7, title("")
	estadd local loanchars "YES": reg7
	estadd local banktimefx "YES": reg7
	estadd local indtimefx "YES": reg7
	estadd local loantypefx "YES": reg7
	estadd local loanpurposefx "YES": reg7
					

	#delimit;
esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 using "TableNumbankPDDemand.tex", replace eqlabels(none) nomtitles nodepvars label 
keep (numbankcounty $firmvarssize logdensityy logwage logwagefin logempgrowth logwagegrowth estpop bapop logestgrowth logbagrowth numbranchpop) 
order (numbankcounty $firmvarssize logdensityy logwage logwagefin logempgrowth logwagegrowth estpop bapop logestgrowth logbagrowth numbranchpop) 
cells(b(star fmt(3)) t(fmt(3) par abs)) 
booktabs modelwidth(7) collabels(none) substitute(_ \_) 
mgroups("Probability of Default (\%)",  pattern(1 0 0 0 0 0 0) 
prefix(\multicolumn{@span}{c}{) suffix(})   
span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


*********************************************************************************************************************************
* Online Appendix Table 21: Market Structure and Markups with Additional Loan Demand Controls
*********************************************************************************************************************************
eststo clear  		

reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin logempgrowth, absorb($fe) vce(cluster county) 
	est store reg1, title("")
	estadd local loanchars "YES": reg1
	estadd local banktimefx "YES": reg1
	estadd local indtimefx "YES": reg1
	estadd local loantypefx "YES": reg1
	estadd local loanpurposefx "YES": reg1
													
reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin logwagegrowth, absorb($fe) vce(cluster county) 
	est store reg2, title("")
	estadd local loanchars "YES": reg2
	estadd local banktimefx "YES": reg2
	estadd local indtimefx "YES": reg2
	estadd local loantypefx "YES": reg2
	estadd local loanpurposefx "YES": reg2
									
reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin estpop, absorb($fe) vce(cluster county) 
	est store reg3, title("")
	estadd local loanchars "YES": reg3
	estadd local banktimefx "YES": reg3
	estadd local indtimefx "YES": reg3
	estadd local loantypefx "YES": reg3
	estadd local loanpurposefx "YES": reg3
											
reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin bapop, absorb($fe) vce(cluster county) 
	est store reg4, title("")
	estadd local loanchars "YES": reg4
	estadd local banktimefx "YES": reg4
	estadd local indtimefx "YES": reg4
	estadd local loantypefx "YES": reg4
	estadd local loanpurposefx "YES": reg4

reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin logestgrowth, absorb($fe) vce(cluster county) 
	est store reg5, title("")
	estadd local loanchars "YES": reg5
	estadd local banktimefx "YES": reg5
	estadd local indtimefx "YES": reg5
	estadd local loantypefx "YES": reg5
	estadd local loanpurposefx "YES": reg5
					
reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin logbagrowth, absorb($fe) vce(cluster county) 
	est store reg6, title("")
	estadd local loanchars "YES": reg6
	estadd local banktimefx "YES": reg6
	estadd local indtimefx "YES": reg6
	estadd local loantypefx "YES": reg6
	estadd local loanpurposefx "YES": reg6
									
reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin numbranchpop, absorb($fe) vce(cluster county) 
	est store reg7, title("")
	estadd local loanchars "YES": reg7
	estadd local banktimefx "YES": reg7
	estadd local indtimefx "YES": reg7
	estadd local loantypefx "YES": reg7
	estadd local loanpurposefx "YES": reg7
					

	
#delimit;
esttab reg1 reg2 reg3 reg4 reg5 reg6 reg7 using "TableNumbankMarkupDemand.tex", replace eqlabels(none) nomtitles nodepvars label 
keep (numbankcounty pd_prc lgd_prc pdlgd $firmvarssize logdensityy logwage logwagefin logempgrowth logwagegrowth estpop bapop logestgrowth logbagrowth numbranchpop) 
order (numbankcounty pd_prc lgd_prc pdlgd $firmvarssize logdensityy logwage logwagefin logempgrowth logwagegrowth estpop bapop logestgrowth logbagrowth numbranchpop) 
cells(b(star fmt(3)) t(fmt(3) par abs)) 
booktabs modelwidth(7) collabels(none) substitute(_ \_) 
mgroups("Interest Rate (\%)",  pattern(1 0 0 0 0 0 0) 
prefix(\multicolumn{@span}{c}{) suffix(})   
span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


*********************************************************************************************************************************
* Online Appendix Table 22: Market Structure and Interest Rates (Interactions with Firm and Loan Characteristics)
*********************************************************************************************************************************				
		
/* 	 NOTE: UPDATE Averge Marginal Effect & p-value manually.
	 First regression to provide marginal effect. The second is similar but for LaTex output purposes	*/

eststo clear 	

reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin , absorb($fe) vce(cluster county) 
margins, dydx(numbankcounty)
reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin , absorb($fe) vce(cluster county) 
		est store county1, title("")
		estadd local loanchars "YES": county1
		estadd local banktimefx "YES": county1
		estadd local indtimefx "YES": county1
		estadd local loantypefx "YES": county1
		estadd local loanpurposefx "YES": county1
		


reghdfe interest_rate_prc c.numbankcounty##c.($loancontrols  $firmvarssize) logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
margins, dydx(numbankcounty)
reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize $numbankcountyinteracts logdensityy logwage logwagefin, absorb($fe) vce(cluster county)
		est store county5, title("")
		estadd local loanchars "YES": county5
		estadd local banktimefx "YES": county5
		estadd local indtimefx "YES": county5
		estadd local loantypefx "YES": county5
		estadd local loanpurposefx "YES": county5

		
#delimit;
		esttab county1 /* county3 */ county5 using "TableNumbankIRInteractions.tex", replace eqlabels(none) nomtitles nodepvars label 
				keep (numbankcounty $loancontrols  $firmvarssize $numbankcountyinteracts logdensityy logwage logwagefin) 
				order (numbankcounty $loancontrols  $firmvarssize $numbankcountyinteracts logdensityy logwage logwagefin) 
				cells(b(star fmt(3)) t(fmt(3) par abs)) 
				booktabs modelwidth(2) collabels(none) substitute(_ \_) 
			mgroups("Interest Rate (\%)",  pattern(1 0) 
				prefix(\multicolumn{@span}{c}{) suffix(})   
				span erepeat(\cmidrule(lr){@span}))         
			style(tex) star(* 0.10 ** 0.05 *** 0.01) 
			alignment(D{.}{.}{4,6}) 
			stats(average pvalue loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %~12s %~12s %9.0fc %9.2f)  
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
				label ("Average Marginal Effect" "p-value" "Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

*********************************************************************************************************************************
* Online Appendix Table 23: Market Structure and Borrower Risk ( Interactions with Firm and Loan Characteristics)
*********************************************************************************************************************************				
		
/* 	 NOTE: UPDATE Averge Marginal Effect & p-value manually.
	 First regression to provide marginal effect. The second is similar but for LaTex output purposes	*/

eststo clear 	

reghdfe pd_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
		margins, dydx(numbankcounty)
reghdfe pd_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
		est store county1, title("")
		estadd local loanchars "YES": county1
		estadd local banktimefx "YES": county1
		estadd local indtimefx "YES": county1
		estadd local loantypefx "YES": county1
		estadd local loanpurposefx "YES": county1
		

reghdfe pd_prc c.numbankcounty##c.($loancontrols  $firmvarssize) logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
		margins, dydx(numbankcounty)
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize $numbankcountyinteracts logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
		est store county5, title("")
		estadd local loanchars "YES": county5
		estadd local banktimefx "YES": county5
		estadd local indtimefx "YES": county5
		estadd local loantypefx "YES": county5
		estadd local loanpurposefx "YES": county5

		
#delimit;
		esttab county1 /* county3 */ county5 using "TableNumbankPDInteractions.tex", replace eqlabels(none) nomtitles nodepvars label 
				keep (numbankcounty $loancontrols  $firmvarssize $numbankcountyinteracts logdensityy logwage logwagefin) 
				order (numbankcounty $loancontrols  $firmvarssize $numbankcountyinteracts logdensityy logwage logwagefin)
				cells(b(star fmt(3)) t(fmt(3) par abs)) 
				booktabs modelwidth(2) collabels(none) substitute(_ \_) 
			mgroups("Probability of Default (\%)",  pattern(1 0) 
				prefix(\multicolumn{@span}{c}{) suffix(})   
				span erepeat(\cmidrule(lr){@span}))         
			style(tex) star(* 0.10 ** 0.05 *** 0.01) 
			alignment(D{.}{.}{4,6}) 
			stats(average pvalue loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %~12s %~12s %9.0fc %9.2f)  
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
				label ("Average Marginal Effect" "p-value" "Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

*********************************************************************************************************************************
* Online Appendix Table 24: Market Structure and Markups (Interactions with Firm and Loan Characteristics)
*********************************************************************************************************************************				
		
/* 	 NOTE: UPDATE Averge Marginal Effect & p-value manually.
	 First regression to provide marginal effect. The second is similar but for LaTex output purposes	*/

eststo clear  	

reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county)
			margins, dydx(numbankcounty)
reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd  $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
		est store county1, title("")
		estadd local loanchars "YES": county1
		estadd local banktimefx "YES": county1
		estadd local indtimefx "YES": county1
		estadd local loantypefx "YES": county1
		estadd local loanpurposefx "YES": county1
		
		


reghdfe interest_rate_prc c.numbankcounty##c.($loancontrols  $firmvarssize pd_prc lgd_prc pdlgd) logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
			margins, dydx(numbankcounty)
reghdfe interest_rate_prc numbankcounty $loancontrols  $firmvarssize $numbankcountyinteracts numbankcountypd numbankcountylgd numbankcountypdlgd pd_prc lgd_prc pdlgd logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
		est store county4, title("")
		estadd local loanchars "YES": county4
		estadd local banktimefx "YES": county4
		estadd local indtimefx "YES": county4
		estadd local loantypefx "YES": county4
		estadd local loanpurposefx "YES": county4
			
#delimit;
		esttab county1 /* county2 */ county4 using "TableNumbankIROneStageInteractions.tex", replace eqlabels(none) nomtitles nodepvars label 
				keep (numbankcounty pd_prc lgd_prc pdlgd numbankcountypd numbankcountylgd numbankcountypdlgd  $loancontrols  $firmvarssize $numbankcountyinteracts logdensityy logwage logwagefin) 
				order (numbankcounty pd_prc lgd_prc pdlgd numbankcountypd numbankcountylgd numbankcountypdlgd  $loancontrols  $firmvarssize $numbankcountyinteracts logdensityy logwage logwagefin) 
				cells(b(star fmt(3)) t(fmt(3) par abs)) 
				booktabs modelwidth(2) collabels(none) substitute(_ \_) 
			mgroups("Interest Rate (\%)",  pattern(1 0) 
				prefix(\multicolumn{@span}{c}{) suffix(})   
				span erepeat(\cmidrule(lr){@span}))         
			style(tex) star(* 0.10 ** 0.05 *** 0.01) 
			alignment(D{.}{.}{4,6}) 
			stats(average pvalue loanchars banktimefx indtimefx N r2_a, fmt(%9.3fc %9.3fc %~12s %~12s %~12s %9.0fc %9.2f)  
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
				label ("Average Marginal Effect" "p-value" "Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr				

*********************************************************************************************************************************
* Online Appendix Table 25: Risk Assessments and Interest Rates (Interactions with Firm and Loan Characteristics)
*********************************************************************************************************************************				
		
/* 	 NOTE: UPDATE Averge Marginal Effect & p-value manually.
	 First regression to provide marginal effect. The second is similar but for LaTex output purposes	*/

eststo clear  
reghdfe interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols $firmvarssize, absorb($fe, savefe )  vce(cluster county) 
	margins, dydx(pd_prc)
	margins, dydx(lgd_prc)
	est store reg1, title("")
	estadd local loanchars "YES": reg1
	estadd local banktimefx "YES": reg1
	estadd local indtimefx "YES": reg1
	estadd local loantypefx "YES": reg1
	estadd local loanpurposefx "YES": reg1

	
reghdfe interest_rate_prc pd_prc lgd_prc pdlgd c.pd_prc##c.($loancontrols) c.pd_prc##c.($firmvarssize) c.lgd_prc##c.($loancontrols) c.lgd_prc##c.($firmvarssize), absorb($fe, savefe )  vce(cluster county) 
	margins, dydx(pd_prc)
	margins, dydx(lgd_prc)
reghdfe interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols $firmvarssize $pdinteracts $lgdinteracts, absorb($fe, savefe )  vce(cluster county)
	est store reg4, title("")
	estadd local loanchars "YES": reg4
	estadd local banktimefx "YES": reg4
	estadd local indtimefx "YES": reg4
	estadd local loantypefx "YES": reg4
	estadd local loanpurposefx "YES": reg4
				
		* Reg output;
		#delimit;
			esttab reg1 reg4 using "TableIRInteractions.tex", replace eqlabels(none) nomtitles nodepvars label
			keep (pd_prc lgd_prc pdlgd $loancontrols $firmvarssize $pdinteracts $lgdinteracts) 
			order (pd_prc lgd_prc pdlgd $loancontrols $firmvarssize $pdinteracts $lgdinteracts)
			cells(b(star fmt(3)) t(fmt(3) par abs)) 
			booktabs modelwidth(2) collabels(none) substitute(_ \_) 
		mgroups("Interest Rate (\%)", pattern(1 0) 
			prefix(\multicolumn{@span}{c}{) suffix(})   
			span erepeat(\cmidrule(lr){@span}))         
		style(tex) star(* 0.10 ** 0.05 *** 0.01) 
		alignment(D{.}{.}{4,6}) 
		stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
			label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))

		;
		#delimit cr

		
*********************************************************************************************************************************
* Online Appendix Table 26: Market Structure and Interest Rates (Two-Stage Regression)
*********************************************************************************************************************************				
reghdfe interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols $firmvarssize, absorb($fe, savefe )  vce(cluster county) resid
predict residual1, resid

reghdfe interest_rate_prc pd_prc lgd_prc pdlgd c.pd_prc##c.($loancontrols) c.pd_prc##c.($firmvarssize) c.lgd_prc##c.($loancontrols) c.lgd_prc##c.($firmvarssize), absorb($fe, savefe )  vce(cluster county) resid
predict residual2, resid		

reg residual1 numbankcounty
	est store res1, title("")
reg residual2 numbankcounty
	est store res2, title("")
	
	* Reg output;
	#delimit;
		esttab res1 res2 using "Table2ndStage.tex", replace eqlabels(none) nomtitles nodepvars label
		cells(b(star fmt(3)) t(fmt(3) par abs)) 
		booktabs modelwidth(2) collabels(none) substitute(_ \_) 
	mgroups("Residual from Baseline Model" "Residual from Saturated Model", pattern(1 1) 
		prefix(\multicolumn{@span}{c}{) suffix(})   
		span erepeat(\cmidrule(lr){@span}))         
	style(tex) star(* 0.10 ** 0.05 *** 0.01) 
	alignment(D{.}{.}{4,6}) 
	stats(N r2_a, fmt(%9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
		label ("Observations" "Adj. R-squared"))

	;
	#delimit cr	

	
*********************************************************************************************************************************
* Online Appendix Table 27: Market Structure and Interest Rates (Non-linear Effects of Firm and Loan Characteristics)
*********************************************************************************************************************************				
eststo clear 	

reghdfe interest_rate_prc numbankcounty $loancontrols $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
	est store county1, title("")
	estadd local loanchars "YES": county1
	estadd local banktimefx "YES": county1
	estadd local indtimefx "YES": county1
	estadd local loantypefx "YES": county1
	estadd local loanpurposefx "YES": county1

reghdfe interest_rate_prc numbankcounty $squareds, absorb($fe) vce(cluster county) 
	est store county5, title("")
	estadd local loanchars "YES": county5
	estadd local banktimefx "YES": county5
	estadd local indtimefx "YES": county5
	estadd local loantypefx "YES": county5
	estadd local loanpurposefx "YES": county5

	
#delimit;
	esttab county1 county5 using "TableNumbankIRsquared.tex", replace eqlabels(none) nomtitles nodepvars label 
			keep (numbankcounty $squareds) 
			order (numbankcounty $squareds) 
			cells(b(star fmt(3)) t(fmt(3) par abs)) 
			booktabs modelwidth(2) collabels(none) substitute(_ \_) 
		mgroups("Interest Rate (\%)",  pattern(1 0) 
			prefix(\multicolumn{@span}{c}{) suffix(})   
			span erepeat(\cmidrule(lr){@span}))         
		style(tex) star(* 0.10 ** 0.05 *** 0.01) 
		alignment(D{.}{.}{4,6}) 
		stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
			label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		
		
*********************************************************************************************************************************
* Online Appendix Table 28: Market Structure and Borrower Risk (Non-linear Effects of Firm and Loan Characteristics)
*********************************************************************************************************************************				
eststo clear 	

reghdfe pd_prc numbankcounty $loancontrols $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
	est store county1, title("")
	estadd local loanchars "YES": county1
	estadd local banktimefx "YES": county1
	estadd local indtimefx "YES": county1
	estadd local loantypefx "YES": county1
	estadd local loanpurposefx "YES": county1

reghdfe pd_prc numbankcounty $squareds, absorb($fe) vce(cluster county) 
	est store county5, title("")
	estadd local loanchars "YES": county5
	estadd local banktimefx "YES": county5
	estadd local indtimefx "YES": county5
	estadd local loantypefx "YES": county5
	estadd local loanpurposefx "YES": county5

	
#delimit;
	esttab county1 county5 using "TableNumbankPDsquared.tex", replace eqlabels(none) nomtitles nodepvars label 
			keep (numbankcounty $squareds) 
			order (numbankcounty $squareds) 
			cells(b(star fmt(3)) t(fmt(3) par abs)) 
			booktabs modelwidth(2) collabels(none) substitute(_ \_) 
		mgroups("Probability of Default (\%)",  pattern(1 0) 
			prefix(\multicolumn{@span}{c}{) suffix(})   
			span erepeat(\cmidrule(lr){@span}))         
		style(tex) star(* 0.10 ** 0.05 *** 0.01) 
		alignment(D{.}{.}{4,6}) 
		stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
			label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr			
		
*********************************************************************************************************************************
* Online Appendix Table 29: Market Structure and Markups (Non-linear Effects of Firm and Loan Characteristics)
*********************************************************************************************************************************				
eststo clear 

reghdfe interest_rate_prc pd_prc lgd_prc pdlgd numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin, absorb($fe) vce(cluster county) 
	est store county1, title("")
	estadd local loanchars "YES": county1
	estadd local banktimefx "YES": county1
	estadd local indtimefx "YES": county1
	estadd local loantypefx "YES": county1
	estadd local loanpurposefx "YES": county1

reghdfe interest_rate_prc pd_prc lgd_prc pdlgd numbankcounty $squareds, absorb($fe) vce(cluster county) 
	est store county5, title("")
	estadd local loanchars "YES": county5
	estadd local banktimefx "YES": county5
	estadd local indtimefx "YES": county5
	estadd local loantypefx "YES": county5
	estadd local loanpurposefx "YES": county5

	
#delimit;
	esttab county1 county5 using "TableNumbankMarkupsquared.tex", replace eqlabels(none) nomtitles nodepvars label 
			keep (numbankcounty pd_prc lgd_prc pdlgd $squareds) 
			order (numbankcounty pd_prc lgd_prc pdlgd $squareds) 
			cells(b(star fmt(3)) t(fmt(3) par abs)) 
			booktabs modelwidth(2) collabels(none) substitute(_ \_) 
		mgroups("Interest Rate (\%)",  pattern(1 0) 
			prefix(\multicolumn{@span}{c}{) suffix(})   
			span erepeat(\cmidrule(lr){@span}))         
		style(tex) star(* 0.10 ** 0.05 *** 0.01) 
		alignment(D{.}{.}{4,6}) 
		stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
			label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr				
			
*********************************************************************************************************************************
* Online Appendix Figure 2: The Effect of GSIB Surcharges on Lending Volume (All Loans)
*********************************************************************************************************************************
* Provided in a separate file	
	
*************************************************************************************************************************************************************************************
* Online Appendix Table 30: The Effect of GSIB Surcharges on the Number of Banks and Lending Volume
*************************************************************************************************************************************************************************************
eststo clear
* Two columnns are loan-level and two are county-level
preserve
	keep numbankcy year county postnumgsib2015
	duplicates drop
	
	reghdfe numbank postnumgsib2015 if year>2014 & year <= 2019, absorb(county year) vce(cluster county)
		est store gsib1, title("")
		estadd local spec "County-Year": gsib1
		estadd local county "YES": gsib1
		estadd local year "YES": gsib1
restore
		
preserve
	keep logvol county yq postnumgsib2015 year
	duplicates drop
	
	reghdfe logvol postnumgsib2015 if year <= 2019, absorb(county yq) vce(cluster county)	//includes 2014
		est store gsib2, title("")
		estadd local spec "County-Quarter": gsib2
		estadd local county "YES": gsib2
		estadd local quarter "YES": gsib2
restore
		

* Reg output;
#delimit;
	esttab gsib1 gsib2 using "TableGSIBOneStageAppendix.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (postnumgsib2015) 
	order (postnumgsib2015)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(2) collabels(none) substitute(_ \_) 
mgroups("Number of Banks" "log(Loan Volume)", pattern(1 1) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(spec county year quarter N r2_a, fmt(%~12s %~12s %~12s %~12s %9.0fc %9.2f)  
layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
label ("Observation Level" "County FE" "Year FE" "Quarter FE" "Observations" "Adj. R-squared"))
;
				
#delimit cr		


*************************************************************************************************************************************************************************************
* Online Appendix Table 31: The Effect of GSIB Surcharges on the Number of GSIB and Non-GSIB Banks
*************************************************************************************************************************************************************************************
eststo clear
* Two columnns are loan-level and two are county-level

reghdfe numgsib postnumgsib2015 staybank $loancontrols if year>2014 & year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
	est store gsib1, title("")
	estadd local spec "Loan":gsib1
	estadd local bankcounty "YES": gsib1
	estadd local loanchars "YES": gsib1
	estadd local banktimefx "YES": gsib1
	estadd local indtimefx "YES": gsib1

reghdfe numnongsib postnumgsib2015 staybank $loancontrols if year>2014 & year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
	est store gsib2, title("")
	estadd local spec "Loan":gsib2
	estadd local bankcounty "YES": gsib2
	estadd local loanchars "YES": gsib2
	estadd local banktimefx "YES": gsib2
	estadd local indtimefx "YES": gsib2

preserve
	keep numgsib numnongsib year county postnumgsib2015
	duplicates drop
	
	reghdfe numgsib postnumgsib2015 if year>2014 & year <= 2019, absorb(county year) vce(cluster county)
		est store gsib3, title("")					
		estadd local spec "County-Year":gsib3
		estadd local county "YES": gsib3
		estadd local year "YES": gsib3
	
	reghdfe numnongsib  postnumgsib2015 if year>2014 & year <= 2019, absorb(county year) vce(cluster county)
		est store gsib4, title("")
		estadd local spec "County-Year":gsib4
		estadd local county "YES": gsib4
		estadd local year "YES": gsib4
restore


* Reg output;
#delimit;
	esttab gsib1 gsib3 gsib2 gsib4 using "TableGSIBOneStageAppendix2.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (postnumgsib2015) 
	order (postnumgsib2015)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(4) collabels(none) substitute(_ \_) 
mgroups("Number of GSIBs" "Number of Non-GSIBs" , pattern(1 0 1 0) 
		prefix(\multicolumn{@span}{c}{) suffix(})   
		span erepeat(\cmidrule(lr){@span}))         
	style(tex) star(* 0.10 ** 0.05 *** 0.01) 
	alignment(D{.}{.}{4,6}) 
	stats(spec loanchars bankcounty banktimefx indtimefx county year N r2_a, fmt(%~12s %~12s %~12s %~12s %~12s %~12s %~12s %9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}"  "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
		label ("Observation Level" "Loan Controls" "Bank-County FE" "Bank-Quarter FE" "Industry-Quarter FE" "County FE" "Year FE" "Observations" "Adj. R-squared"))
;
				
#delimit cr			
	
	
	
*************************************************************************************************************************************************************************************
* Online Appendix Table 32: The Effect of GSIB Surcharges on GSIB and Non-GSIB Bank Loan Volume
*************************************************************************************************************************************************************************************
eststo clear

reghdfe loggsibvol postnumgsib2015 staybank $loancontrols if year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
	est store gsib1, title("")
	estadd local spec "Loan":gsib1
	estadd local bankcounty "YES": gsib1
	estadd local loanchars "YES": gsib1
	estadd local banktimefx "YES": gsib1
	estadd local indtimefx "YES": gsib1

reghdfe lognongsibvol postnumgsib2015 staybank $loancontrols if year <= 2019, absorb($fe bankcounty banktime) vce(cluster county)
	est store gsib2, title("")
	estadd local spec "Loan":gsib2
	estadd local bankcounty "YES": gsib2
	estadd local loanchars "YES": gsib2
	estadd local banktimefx "YES": gsib2
	estadd local indtimefx "YES": gsib2

preserve
	keep loggsibvol lognongsibvol year yq county postnumgsib2015
	duplicates drop
	
	reghdfe loggsibvol postnumgsib2015 if year <= 2019, absorb(county yq) vce(cluster county)
		est store gsib3, title("")					
		estadd local spec "County-Quarter":gsib3
		estadd local county "YES": gsib3
		estadd local year "YES": gsib3
	
	reghdfe lognongsibvol  postnumgsib2015 if year <= 2019, absorb(county yq) vce(cluster county)
		est store gsib4, title("")
		estadd local spec "County-Quarter":gsib4
		estadd local county "YES": gsib4
		estadd local year "YES": gsib4
restore


* Reg output;
#delimit;
	esttab gsib1 gsib3 gsib2 gsib4 using "TableGSIBOneStageAppendix2vol.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (postnumgsib2015) 
	order (postnumgsib2015)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(4) collabels(none) substitute(_ \_) 
mgroups("GSIB log(Loan Volume)" "Non-GSIB log(Loan Volume)" , pattern(1 0 1 0) 
		prefix(\multicolumn{@span}{c}{) suffix(})   
		span erepeat(\cmidrule(lr){@span}))         
	style(tex) star(* 0.10 ** 0.05 *** 0.01) 
	alignment(D{.}{.}{4,6}) 
	stats(spec loanchars bankcounty banktimefx indtimefx county year N r2_a, fmt(%~12s %~12s %~12s %~12s %~12s %~12s %~12s %9.0fc %9.2f)  
		layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}"
		"\multicolumn{1}{c}{@}"  "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
		label ("Observation Level" "Loan Controls" "Bank-County FE" "Bank-Quarter FE" "Industry-Quarter FE" "County FE" "Quarter FE" "Observations" "Adj. R-squared"))
;
				
#delimit cr	
	
*************************************************************************************************************************************************************************************
* Online Appendix Table 34: Changes in GSIB Lending Behavior
*************************************************************************************************************************************************************************************
est clear
reghdfe pd_prc gsibpost staybank $loancontrols if  year <= 2019, absorb(i.bank i.iquarter i.credit_facility_purpose_cat i.line_variability) vce(cluster county)
	est store reg1, title("")
	estadd local loanchars "YES": reg1
	estadd local bankfx "YES": reg1
	estadd local indtimefx "YES": reg1
	estadd local loantypefx "YES": reg1
	estadd local loanpurposefx "YES": reg1

reghdfe size gsibpost staybank $loancontrols if  year <= 2019, absorb(i.bank i.iquarter i.credit_facility_purpose_cat i.line_variability) vce(cluster county)
	est store reg2, title("")
	estadd local loanchars "YES": reg2
	estadd local bankfx "YES": reg2
	estadd local indtimefx "YES": reg2
	estadd local loantypefx "YES": reg2
	estadd local loanpurposefx "YES": reg2


reghdfe leverage gsibpost staybank $loancontrols if  year <= 2019, absorb(i.bank i.iquarter i.credit_facility_purpose_cat i.line_variability) vce(cluster county)
	est store reg3, title("")
	estadd local loanchars "YES": reg3
	estadd local bankfx "YES": reg3
	estadd local indtimefx "YES": reg3
	estadd local loantypefx "YES": reg3
	estadd local loanpurposefx "YES": reg3

reghdfe tangibility gsibpost staybank $loancontrols if  year <= 2019, absorb(i.bank i.iquarter i.credit_facility_purpose_cat i.line_variability) vce(cluster county)
	est store reg4, title("")
	estadd local loanchars "YES": reg4
	estadd local bankfx "YES": reg4
	estadd local indtimefx "YES": reg4
	estadd local loantypefx "YES": reg4
	estadd local loanpurposefx "YES": reg4

reghdfe profitability gsibpost staybank $loancontrols if  year <= 2019, absorb(i.bank i.iquarter i.credit_facility_purpose_cat i.line_variability) vce(cluster county)
	est store reg5, title("")
	estadd local loanchars "YES": reg5
	estadd local bankfx "YES": reg5
	estadd local indtimefx "YES": reg5
	estadd local loantypefx "YES": reg5
	estadd local loanpurposefx "YES": reg5

#delimit;
			esttab reg1 reg2 reg3 reg4 reg5 using "TableGSIBBankLendingPost2.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (gsibpost   ) 
					order (gsibpost ) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(5) collabels(none) substitute(_ \_) 
				mgroups("Probability of Default (\%)" "Log(Assets)" "Leverage" "Tangibility" "Profitability",  pattern(1 1 1 1 1) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars bankfx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr				
	
*************************************************************************************************************************************************************************************
* Online Appendix Table 34: GSIB Surcharges and Market Outcomes (Reduced Form Difference-in-Differences, Non-GSIBs Only)
***********************************************************************************************************************************************************************************				
eststo clear

reghdfe numbank postnumgsib2015 staybank $loancontrols if year>2014 & year <= 2019 & gsib == 0, absorb($fe bankcounty banktime) vce(cluster county)
	est store gsib1, title("")
	estadd local bankcounty "YES": gsib1
	estadd local loanchars "YES": gsib1
	estadd local banktimefx "YES": gsib1
	estadd local indtimefx "YES": gsib1

reghdfe logvol postnumgsib2015 staybank $loancontrols if  year <= 2019 & gsib == 0, absorb($fe bankcounty banktime) vce(cluster county)	//includes 2014
	est store gsib2, title("")
	estadd local bankcounty "YES": gsib2
	estadd local loanchars "YES": gsib2
	estadd local banktimefx "YES": gsib2
	estadd local indtimefx "YES": gsib2

reghdfe interest_rate_prc postnumgsib2015 staybank $loancontrols if  year <= 2019 & gsib == 0, absorb($fe bankcounty banktime) vce(cluster county)	//includes 2014
	est store gsib3, title("")
	estadd local bankcounty "YES": gsib3
	estadd local loanchars "YES": gsib3
	estadd local banktimefx "YES": gsib3
	estadd local indtimefx "YES": gsib3

reghdfe pd_prc postnumgsib2015 staybank $loancontrols if  year <= 2019 & gsib == 0, absorb($fe bankcounty banktime) vce(cluster county)	//includes 2014
	est store gsib4, title("")
	estadd local bankcounty "YES": gsib4
	estadd local loanchars "YES": gsib4
	estadd local banktimefx "YES": gsib4
	estadd local indtimefx "YES": gsib4

reghdfe interest_rate_prc postnumgsib2015 staybank pd_prc lgd_prc pdlgd $loancontrols if  year <= 2019 & gsib == 0, absorb($fe bankcounty banktime) vce(cluster county)	//includes 2014
	est store gsib5, title("")
	estadd local bankcounty "YES": gsib5
	estadd local loanchars "YES": gsib5
	estadd local banktimefx "YES": gsib5
	estadd local indtimefx "YES": gsib5

* Reg output;
#delimit;
	esttab gsib1 gsib2 gsib3 gsib4 gsib5 using "TablegsibOneStagebyNongsib.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (postnumgsib2015  pd_prc lgd_prc pdlgd) 
	order (postnumgsib2015  pd_prc lgd_prc pdlgd)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(5) collabels(none) substitute(_ \_) 
mgroups("Number of Banks" "log(Loan Volume)" "Interest Rate (\%)" "Probability of Default (\%)" "Interest Rate (\%)" , pattern(1 1 1 1 1) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars bankcounty banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %~12s %9.0fc %9.2f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}"  "\multicolumn{1}{c}{@}")
	label ("Loan Controls" "Bank-County FE" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;

#delimit cr		
	

*************************************************************************************************************************************************************************************
* Online Appendix Table 35: GSIB Surcharges and Market Outcomes (Two-Stage Least Squares, non-GSIBS only)
***********************************************************************************************************************************************************************************		
eststo clear

ivreghdfe logvol staybank $loancontrols (numbank=postnumgsib2015) if year>2014 & year <= 2019 & gsib == 0, absorb($fe bankcounty banktime) /* cluster(county) */	
		est store gsib1, title("")
		estadd local bankcounty "YES": gsib1
		estadd local loanchars "YES": gsib1
		estadd local banktimefx "YES": gsib1
		estadd local indtimefx "YES": gsib1

ivreghdfe interest_rate_prc staybank $loancontrols (numbank=postnumgsib2015) if year>2014 & year <= 2019 & gsib == 0, absorb($fe bankcounty banktime) /* cluster(county) */				
		est store gsib2, title("")
		estadd local bankcounty "YES": gsib2
		estadd local loanchars "YES": gsib2
		estadd local banktimefx "YES": gsib2
		estadd local indtimefx "YES": gsib2

ivreghdfe pd_prc staybank $loancontrols (numbank=postnumgsib2015) if year>2014 & year <= 2019 & gsib == 0, absorb($fe bankcounty banktime) /* cluster(county) */
		est store gsib3, title("")
		estadd local bankcounty "YES": gsib3
		estadd local loanchars "YES": gsib3
		estadd local banktimefx "YES": gsib3
		estadd local indtimefx "YES": gsib3

ivreghdfe interest_rate_prc staybank pd_prc lgd_prc pdlgd $loancontrols (numbank=postnumgsib2015) if year>2014 & year <= 2019  & gsib == 0, absorb($fe bankcounty banktime) /* cluster(county) */
		est store gsib4, title("")
		estadd local bankcounty "YES": gsib4
		estadd local loanchars "YES": gsib4
		estadd local banktimefx "YES": gsib4
		estadd local indtimefx "YES": gsib4
		
* Reg output;
#delimit;
	esttab gsib1 gsib2 gsib3 gsib4 using "TableGSIBIvregNonGSIB.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (numbank pd_prc lgd_prc pdlgd) 
	order (numbank pd_prc lgd_prc pdlgd)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(4) collabels(none) substitute(_ \_) 
mgroups("log(Loan Volume)" "Interest Rate (\%)" "Probability of Default (\%)" "Interest Rate (\%)" , pattern(1 1 1 1) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars bankcounty banktimefx indtimefx N, fmt(%~12s %~12s %~12s %~12s %9.0fc )  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}"  "\multicolumn{1}{c}{@}" )
	label ("Loan Controls" "Bank-County FE" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" ))
;

#delimit cr	
		

*************************************************************************************************************************************************************************************
* Online Appendix Table 36: County Characteristics and Impact of Capital Surcharges on the Number of Banks
***********************************************************************************************************************************************************************************		
preserve 

keep year yq county numbank numgsib /*logpopy*/ logdensityy logwage logwagefin logestgrowth pd_prc lgd_prc pdlgd $firmvarssize

foreach x of varlist size leverage tangibility profitability{
	egen `x'ave = mean(`x'), by(county yq)
}

keep year yq county numbank numgsib /*logpopy*/ logdensityy logwage logwagefin logestgrowth *ave

duplicates drop 


foreach x of varlist numgsib numbank /*logpopy*/ logdensityy logestgrowth logwage logwagefin /*pd_prcave lgd_prcave pdlgdave */ sizeave leverageave tangibilityave profitabilityave {
	egen post`x' = mean(`x') if year >= 2016, by(county)
	egen maxpost`x' = max(post`x'), by(county)
	replace post`x' = maxpost`x'
	drop maxpost`x'
	
	egen `x'2015 = mean(`x') if year == 2015, by(county)
	egen max`x'2015 = max(`x'2015), by(county)
	replace `x'2015 = max`x'2015
	drop max`x'2015
	
	gen delta`x' = post`x' - `x'2015
}

keep county *2015 post* delta* /*logpopy */
duplicates drop 

gen gsibprc2015 = 100*numgsib2015/numbank2015


la var logdensityy2015 "Population Density (pre)"
la var logwage2015 "Wages (pre)"
la var logwagefin2015 "Financial Industry Wages (pre)"
la var logestgrowth2015 "Number of Establishments Growth (pre)"
la var sizeave2015 "Average Borrower log(assets) (pre)"
la var leverageave2015 "Average Borrower Leverage (pre)"
la var tangibilityave2015 "Average Borrower Tangibility (pre)"
la var profitabilityave2015 "Average Borrower Profitability (pre)"
la var deltalogwage "\Delta Wages"
la var deltalogwagefin "\Delta Financial Industry Wages"
la var deltalogestgrowth "\Delta Number of Establishments Growth"
la var numgsib2015	"Number of GSIBs (2015)"
la var gsibprc2015 "Percentage of GSIBS (2015)"


eststo clear
	
	
reg deltanumbank logdensityy2015 logwage2015 logwagefin2015 *ave2015 numgsib2015 
	est store reg3, title("")

reg deltanumbank c.numgsib2015##c.(logdensityy2015 logwage2015 logwagefin2015 *ave2015)
	est store reg4, title("")
		
			
#delimit;
esttab reg3 reg4 using "TableDeltaGSIB2.tex", replace eqlabels(none) nomtitles nodepvars label cells(b(star fmt(3)) t(fmt(3) par abs)) 
		booktabs modelwidth(2) collabels(none) substitute(_ \_) 
	mgroups("\Delta Num Bank",  pattern(1 0) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span}))         
	style(tex) star(* 0.10 ** 0.05 *** 0.01) 
	alignment(D{.}{.}{4,6}) 
	stats(N r2_a, fmt(%9.0fc %9.2f) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
		label ("Observations" "Adj. R-squared"))
;					
#delimit cr		
				
restore 
		
*********************************************************************************************************************************
* Online Appendix Table 37: Results from the Federal Reserve Bank of Cleveland
*********************************************************************************************************************************
* The following is downloaded from https://www.clevelandfed.org/publications/cd-reports/2022/sr-20220816-clicking-for-credit-experiences-of-online-lender-applicants-from-sbcs

* Open a new .tex file to write to
file open myfile using "survey.tex", write replace

file write myfile "\cline{1-4}" _n
file write myfile "\multicolumn{4}{c}{Financing Sources Applied To}\\ \cline{1-4}" _n
file write myfile "\makecell[ct]{Financing \\Sought}	&\makecell[ct]{Banks\\ Only}	&\makecell[ct]{Banks and \\ Online Lenders}&	\makecell[ct]{Online Lenders\\ Only} \\ \cline{1-4}" _n		
file write myfile "\$25,000 or less &	58\%	&13\%&	29\%		\\" _n
file write myfile "\$25,001-\$50,000	&63\%	&15\%&	22\%		\\" _n
file write myfile "\$50,001-\$100,000&	67\%&	14\%&	19\%	\\	" _n
file write myfile "\$100,001-\$250,000&	78\%&	13\%&	9\%	\\	" _n
file write myfile "\$250,001-\$1M&	83\%	&9\%&	8\%	&	\\" _n
file write myfile "\rowcolor{lightgray} More than \$1M&	96\%	&2\%&	1\%	&	\cellcolor{white} \\ \cline{1-4}\cline{1-4}" _n
file write myfile "\end{tabular}" _n
file write myfile "\end{table}" _n _n

file write myfile "\begin{table}[htbp]" _n
file write myfile "\centering" _n
file write myfile "\begin{tabular}{lccccc}" _n
file write myfile "\hline" _n     
file write myfile "\multicolumn{6}{c}{Financing Sources Applied To}	\\\hline	" _n
file write myfile "Annual Revenue	&\makecell[ct]{Large\\ Bank}&	\makecell[ct]{Small \\ Bank}	& \makecell[ct]{Credit \\ Union}	&\makecell[ct]{Finance \\Company}&	\makecell[ct]{Online\\ Lender}\\\hline" _n
file write myfile "\$100K or less&	48\%&	28\%&	12\%	&15\%	&31\%\\" _n
file write myfile "\$100K-\$1M	&43\%&	32\%&	8\%&	18\%	&26\%\\" _n
file write myfile "\$1M-\$10M	&39\%&	50\%&	6\%	&19\%	&10\%\\" _n
file write myfile "\rowcolor{lightgray} More than \$10M	&48\%&	53\%&	2\%	&10\%	&1\%\\\hline\hline" _n
file write myfile "\end{tabular}" _n
file write myfile "\end{table}" _n _n

* Close the file
file close myfile


*********************************************************************************************************************************
* Online Appendix Table 38: Risk Assessments and Interest Rates (Interactions with Number of Banks)
*********************************************************************************************************************************
eststo clear

* interest rate as dependent variable
reghdfe interest_rate_prc $loancontrols , absorb($fe, savefe )  vce(cluster county) 
			est store altmarkupreg, title("")
			estadd local loanchars "YES": altmarkupreg
			estadd local banktimefx "YES": altmarkupreg
			estadd local indtimefx "YES": altmarkupreg
			estadd local loantypefx "YES": altmarkupreg
			estadd local loanpurposefx "YES": altmarkupreg
		
* main measure
reghdfe interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols , absorb($fe, savefe )  vce(cluster county) 
			est store markupreg, title("")
			estadd local loanchars "YES": markupreg
			estadd local banktimefx "YES": markupreg
			estadd local indtimefx "YES": markupreg
			estadd local loantypefx "YES": markupreg
			estadd local loanpurposefx "YES": markupreg
		
* new column with interactions
reghdfe interest_rate_prc pd_prc lgd_prc pdlgd numbankcounty $numbankcountyriskinteracts $loancontrols , absorb($fe, savefe )  vce(cluster county) 
			est store markupreg2, title("")
			estadd local loanchars "YES": markupreg2
			estadd local banktimefx "YES": markupreg2
			estadd local indtimefx "YES": markupreg2
			estadd local loantypefx "YES": markupreg2
			estadd local loanpurposefx "YES": markupreg2

#delimit;
	esttab altmarkupreg markupreg markupreg2 using "TableMarkupNBankInterac.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (pd_prc lgd_prc pdlgd numbankcounty $numbankcountyriskinteracts $loancontrols) 
	order (pd_prc lgd_prc pdlgd numbankcounty $numbankcountyriskinteracts $loancontrols)
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(3) collabels(none) substitute(_ \_) 
mgroups("Interest Rate (\%)", pattern(1 0 0) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.4f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
	label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))

;
#delimit cr

*********************************************************************************************************************************
* Online Appendix Table 39: Interest Rates, Risk Assessments and Loan Performance (Interactions with Number of Banks)
*********************************************************************************************************************************
eststo clear

reghdfe delinq1_prc interest_rate_prc $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def22reg, title("")
			estadd local loanchars "YES": def22reg
			estadd local banktimefx "YES": def22reg
			estadd local indtimefx "YES": def22reg
			estadd local loantypefx "YES": def22reg
			estadd local loanpurposefx "YES": def22reg
			
reghdfe delinq1_prc pd_prc lgd_prc pdlgd $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def23reg, title("")
			estadd local loanchars "YES": def23reg
			estadd local banktimefx "YES": def23reg
			estadd local indtimefx "YES": def23reg
			estadd local loantypefx "YES": def23reg
			estadd local loanpurposefx "YES": def23reg
			
reghdfe delinq1_prc interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def24reg, title("")
			estadd local loanchars "YES": def24reg
			estadd local banktimefx "YES": def24reg
			estadd local indtimefx "YES": def24reg
			estadd local loantypefx "YES": def24reg
			estadd local loanpurposefx "YES": def24reg
	
reghdfe delinq1_prc interest_rate_prc pd_prc lgd_prc pdlgd numbankcounty $numbankcountyriskinteracts $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def25reg, title("")
			estadd local loanchars "YES": def25reg
			estadd local banktimefx "YES": def25reg
			estadd local indtimefx "YES": def25reg
			estadd local loantypefx "YES": def25reg
			estadd local loanpurposefx "YES": def25reg
			
* default as dependent variable

reghdfe default1_prc interest_rate_prc $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def26reg, title("")
			estadd local loanchars "YES": def26reg
			estadd local banktimefx "YES": def26reg
			estadd local indtimefx "YES": def26reg
			estadd local loantypefx "YES": def26reg
			estadd local loanpurposefx "YES": def26reg
			
reghdfe default1_prc pd_prc lgd_prc pdlgd $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def27reg, title("")
			estadd local loanchars "YES": def27reg
			estadd local banktimefx "YES": def27reg
			estadd local indtimefx "YES": def27reg
			estadd local loantypefx "YES": def27reg
			estadd local loanpurposefx "YES": def27reg
			
reghdfe default1_prc interest_rate_prc pd_prc lgd_prc pdlgd $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def28reg, title("")
			estadd local loanchars "YES": def28reg
			estadd local banktimefx "YES": def28reg
			estadd local indtimefx "YES": def28reg
			estadd local loantypefx "YES": def28reg
			estadd local loanpurposefx "YES": def28reg
	
reghdfe default1_prc interest_rate_prc pd_prc lgd_prc pdlgd numbankcounty $numbankcountyriskinteracts $loancontrols  if yq<yq($ylimit - 1,$qlimit), absorb($fe, savefe )  vce(cluster county) 
			est store def29reg, title("")
			estadd local loanchars "YES": def29reg
			estadd local banktimefx "YES": def29reg
			estadd local indtimefx "YES": def29reg
			estadd local loantypefx "YES": def29reg
			estadd local loanpurposefx "YES": def29reg
			
* Reg output;
#delimit;
	esttab  def22reg def23reg def24reg def25reg def26reg def27reg def28reg def29reg using "TablePDValidityNBankInterac.tex", replace eqlabels(none) nomtitles nodepvars label
	keep (interest_rate_prc pd_prc lgd_prc pdlgd numbankcounty $numbankcountyriskinteracts ) 
	order (interest_rate_prc pd_prc lgd_prc pdlgd numbankcounty $numbankcountyriskinteracts )
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(8) collabels(none) substitute(_ \_) 
mgroups("Non-Performance (\%)" "Realized Default (\%)", pattern(1 0 0 0 1 0 0 0) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.4f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
	label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))

;
#delimit cr


*********************************************************************************************************************************
* Online Appendix Table 40: Market Structure and Interest Rates - Smaller Borrowers
*********************************************************************************************************************************
eststo clear

reghdfe interest_rate_prc numbankcounty $loancontrols if salesm<25 /*45.98*/, absorb($fe) vce(cluster county) 
		est store county1, title("")
		estadd local loanchars "YES": county1
		estadd local banktimefx "YES": county1
		estadd local indtimefx "YES": county1
		estadd local loantypefx "YES": county1
		estadd local loanpurposefx "YES": county1
		
		
reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize  if salesm<25 /*45.98*/, absorb($fe) vce(cluster county) 
		est store county3, title("")
		estadd local loanchars "YES": county3
		estadd local banktimefx "YES": county3
		estadd local indtimefx "YES": county3
		estadd local loantypefx "YES": county3
		estadd local loanpurposefx "YES": county3
		

reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin if salesm<25 /*45.98*/, absorb($fe) vce(cluster county) 
		est store county5, title("")
		estadd local loanchars "YES": county5
		estadd local banktimefx "YES": county5
		estadd local indtimefx "YES": county5
		estadd local loantypefx "YES": county5
		estadd local loanpurposefx "YES": county5

		
#delimit;
		esttab county1  county3 county5 using "TableNumbankIR25mcutoff.tex", replace eqlabels(none) nomtitles nodepvars label 
				keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
				order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
				cells(b(star fmt(3)) t(fmt(3) par abs)) 
				booktabs modelwidth(3) collabels(none) substitute(_ \_) 
			mgroups("Interest Rate (\%)",  pattern(1 0 0) 
				prefix(\multicolumn{@span}{c}{) suffix(})   
				span erepeat(\cmidrule(lr){@span}))         
			style(tex) star(* 0.10 ** 0.05 *** 0.01) 
			alignment(D{.}{.}{4,6}) 
			stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
				label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


*********************************************************************************************************************************
* Online Appendix Table 42: Market Structure and Borrower Risk - Smaller Borrowers
*********************************************************************************************************************************
eststo clear

reghdfe pd_prc numbankcounty $loancontrols  if salesm<25 /*45.98*/, absorb($fe) vce(cluster county) 
		est store county1, title("")
		estadd local loanchars "YES": county1
		estadd local banktimefx "YES": county1
		estadd local indtimefx "YES": county1
		estadd local loantypefx "YES": county1
		estadd local loanpurposefx "YES": county1
		
		
reghdfe pd_prc numbankcounty  $loancontrols  $firmvarssize  if salesm<25 /*45.98*/ , absorb($fe) vce(cluster county) 
		est store county3, title("")
		estadd local loanchars "YES": county3
		estadd local banktimefx "YES": county3
		estadd local indtimefx "YES": county3
		estadd local loantypefx "YES": county3
		estadd local loanpurposefx "YES": county3
		
reghdfe pd_prc numbankcounty $loancontrols  $firmvarssize logdensityy logwage logwagefin  if salesm<25 /*45.98*/, absorb($fe) vce(cluster county) 
		est store county5, title("")
		estadd local loanchars "YES": county5
		estadd local banktimefx "YES": county5
		estadd local indtimefx "YES": county5
		estadd local loantypefx "YES": county5
		estadd local loanpurposefx "YES": county5

		
#delimit;
		esttab county1  county3  county5 using "TableNumbankPD25mcutoff.tex", replace eqlabels(none) nomtitles nodepvars label 
				keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
				order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
				cells(b(star fmt(3)) t(fmt(3) par abs)) 
				booktabs modelwidth(3) collabels(none) substitute(_ \_) 
			mgroups("Probability of Default (\%)",  pattern(1 0 0) 
				prefix(\multicolumn{@span}{c}{) suffix(})   
				span erepeat(\cmidrule(lr){@span}))         
			style(tex) star(* 0.10 ** 0.05 *** 0.01) 
			alignment(D{.}{.}{4,6}) 
			stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
				label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

*********************************************************************************************************************************
* Online Appendix Table 42: Market Structure and Markups - Smaller Borrowers
*********************************************************************************************************************************
eststo clear

reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols  if salesm<25 /*45.98*/ , absorb($fe) vce(cluster county) 
		est store county1, title("")
		estadd local loanchars "YES": county1
		estadd local banktimefx "YES": county1
		estadd local indtimefx "YES": county1
		estadd local loantypefx "YES": county1
		estadd local loanpurposefx "YES": county1
		
		
reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize  if salesm<25 /*45.98*/, absorb($fe) vce(cluster county) 
		est store county3, title("")
		estadd local loanchars "YES": county3
		estadd local banktimefx "YES": county3
		estadd local indtimefx "YES": county3
		estadd local loantypefx "YES": county3
		estadd local loanpurposefx "YES": county3
		

reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin  if salesm<25 /*45.98*/, absorb($fe) vce(cluster county) 
		est store county5, title("")
		estadd local loanchars "YES": county5
		estadd local banktimefx "YES": county5
		estadd local indtimefx "YES": county5
		estadd local loantypefx "YES": county5
		estadd local loanpurposefx "YES": county5

		
#delimit;
		esttab county1  county3  county5 using "TableNumbankIROneStage25mcutoff.tex", replace eqlabels(none) nomtitles nodepvars label 
				keep (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
				order (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
				cells(b(star fmt(3)) t(fmt(3) par abs)) 
				booktabs modelwidth(3) collabels(none) substitute(_ \_) 
			mgroups("Interest Rate (\%)",  pattern(1 0 0) 
				prefix(\multicolumn{@span}{c}{) suffix(})   
				span erepeat(\cmidrule(lr){@span}))         
			style(tex) star(* 0.10 ** 0.05 *** 0.01) 
			alignment(D{.}{.}{4,6}) 
			stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
			layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
				label ("Loan Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		
			

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 													  Internet Appendix (2)															 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

* This section produces Internet Appendix Tables 2 and 10 (Propensity Score Matching tables)

cd "$datadir"
use "$master", clear


**********************************************************************************************************************************
* Propensity Score Matching with population density, log wages, log financial industry wages, and log population
**********************************************************************************************************************************
keep county yq numbank numbankcounty logpopy logdensityy logwage logwagefin
order county yq numbank numbankcounty logpopy logdensityy logwage logwagefin
sort county yq
duplicates drop

egen quantile = xtile(numbankcounty), nq(4) by(yq)
gen treated = (quantile == 4) 
gen unconcentrated = treated==0 
save "__conc4Vars", replace

/*
Following Scharfstein and Sunderam (2016)
For each quarter, we estimate the probability that a county has high concentration (top quartile for that quarter) based on observable characteristics.
We then match each high-concentration county to the county with low concentration that is its nearest neighbor in terms of propensity score. (3 controls with replacement)
We again restrict the sample to exclude counties where the estimated propensity score is close to zero or one and counties where we cannot find a close match in terms of propensity score.
We then run our baseline specifications in the matched sample, replacing market concentration with a dummy indicating that the county was treated (i.e. had high concentration)
*/

***************************************************************************************************************
* Online Appendix Table 10: Matching Counties - Panel A: Estimated Propensity Score
***************************************************************************************************************

logit treated logdensityy logwage logwagefin logpopy i.yq
predict propscore 
sum propscore, d
scalar sdevps = r(sd)	// sdevps = .3468842
egen id = group(county yq)

* LaTex Version (Panel A)
eststo clear
logit treated  logdensityy logwage logwagefin logpopy i.yq
est store ps, title("")
estadd local timefx "YES": ps

#delimit;
esttab ps using "`$ialatexdir\'TablePScoreEstimation4Vars.tex", replace eqlabels(none) nomtitles nodepvars label 
keep (logdensityy logwage logwagefin logpopy ) 
order (logdensityy logwage logwagefin logpopy ) 
cells(b(star fmt(3)) t(fmt(3) par abs)) 
booktabs modelwidth(1) collabels(none) substitute(_ \_) 
mgroups("Treated",  pattern(1) 
prefix(\multicolumn{@span}{c}{) suffix(})   
span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(timefx N r2_p, fmt(%~12s %9.0fc %9.2f)  
layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
label ("Quarter FE" "Observations" " Pseudo R-squared"))
;					
#delimit cr		


* Matching		
preserve	
mahapick propscore, idvar(id) treated(treated) genfile(match) matchon(yq) replace nummatches(1)	
use match, clear
ren _prime_id id_treated
ren id id_control
sort id_control
drop _matchnum
save _match, replace
restore

rename id id_control
count 
joinby id_control using _match, unmatched(both)
keep if _merge == 3

replace id_treat=id_control if missing(id_treat)
egen double cohort = group(id_treat)
drop id_treat
rename id_control id_control
sum cohort, detail

*Drop if difference in propensity between nearest neighbor is greater than a quarter of a standard deviation
gen propscoret = propscore if treated == 1 
egen mpropscoret = max(propscoret),by(cohort)
replace propscoret = mpropscoret if missing(propscoret)
drop mpropscoret
gen pdif = propscore - propscoret if treated == 0 

*Report what this standard deviation is in paper 
gen sdev = sdevps //0.3468842 
drop if pdif > (sdev/4) & !missing(pdif)	

egen count = count(treated), by(cohort)
drop if count == 1


*Test differences  
reg logdensityy treated i.yq, vce(cluster county)
reg logwage treated i.yq, vce(cluster county)
reg logwagefin treated i.yq, vce(cluster county)
reg logpopy treated i.yq, vce(cluster county)


***************************************************************************************************************
* Online Appendix Table 10: Matching Counties - Panel E: Covariate Balance in Matched Pairs
***************************************************************************************************************

eststo clear
reg numbankcounty treated i.yq, vce(cluster county)
est store ps0, title("")
estadd local timefx "YES": ps0

reg logdensityy treated i.yq, vce(cluster county)
est store ps1, title("")
estadd local timefx "YES": ps1

reg logwage treated i.yq, vce(cluster county)
est store ps2, title("")
estadd local timefx "YES": ps2

reg logwagefin treated i.yq, vce(cluster county)
est store ps3, title("")
estadd local timefx "YES": ps3

reg logpopy treated i.yq, vce(cluster county)
est store ps4, title("")
estadd local timefx "YES": ps4


#delimit;
esttab ps0 ps1 ps2 ps3 ps4 using "`$ialatexdir\'TablePScoreCovariates4Vars.tex", replace eqlabels(none) nomtitles nodepvars label 
	keep (treated) 
	order (treated) 
	cells(b(star fmt(3)) t(fmt(3) par abs)) 
	booktabs modelwidth(5) collabels(none) substitute(_ \_) 
mgroups("Number of Banks" "Population Density" "Wages" "Financial Sector Wages" "Population",  pattern(1 1 1 1 1) 
	prefix(\multicolumn{@span}{c}{) suffix(})   
	span erepeat(\cmidrule(lr){@span}))         
style(tex) star(* 0.10 ** 0.05 *** 0.01) 
alignment(D{.}{.}{4,6}) 
stats(timefx N r2, fmt(%~12s %9.0fc %9.2f)  
layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
	label ("Quarter FE" "Observations" "R-squared"))
;					
#delimit cr		


* add loan-level data

joinby county yq using "$master"


* depending on specification (interest rate vs markup) (Table 5 column 3 or Table 8 column 3)
global controls $loancontrols  $firmvarssize logdensityy logwage logwagefin logpopy


*Panel B
reghdfe interest_rate_prc treated $controls if propscore >= .2 & propscore <= .8 , absorb($fe) vce(cluster county)
reghdfe interest_rate_prc treated $controls if propscore >= .2 & propscore <= .4, absorb($fe) vce(cluster county) 
reghdfe interest_rate_prc treated $controls if propscore > .4 & propscore <= .6,  absorb($fe) vce(cluster county) 
reghdfe interest_rate_prc treated $controls if propscore > .6 & propscore <= .8, absorb($fe) vce(cluster county) 

*Panel C 
reghdfe pd_prc treated $controls if propscore >= .2 & propscore <= .8 , absorb($fe) vce(cluster county)
reghdfe pd_prc treated $controls if propscore >= .2 & propscore <= .4, absorb($fe) vce(cluster county) 
reghdfe pd_prc treated $controls if propscore > .4 & propscore <= .6,  absorb($fe) vce(cluster county) 
reghdfe pd_prc treated $controls if propscore > .6 & propscore <= .8, absorb($fe) vce(cluster county) 

*Panel D 
reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd $controls if propscore >= .2 & propscore <= .8 , absorb($fe) vce(cluster county)
reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd $controls if propscore >= .2 & propscore <= .4, absorb($fe) vce(cluster county) 
reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd $controls if propscore > .4 & propscore <= .6,  absorb($fe) vce(cluster county) 
reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd $controls if propscore > .6 & propscore <= .8, absorb($fe) vce(cluster county) 

* Other [0,.2] or [.8,1]
reghdfe interest_rate_prc treated $controls if propscore < .2, absorb($fe) vce(cluster county) 
reghdfe interest_rate_prc treated $controls if propscore >- .8, absorb($fe) vce(cluster county) 
reghdfe pd_prc treated $controls if propscore < .2, absorb($fe) vce(cluster county) 
reghdfe pd_prc treated $controls if propscore >- .8, absorb($fe) vce(cluster county) 
reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd $controls if propscore < .2, absorb($fe) vce(cluster county) 
reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd $controls if propscore >- .8, absorb($fe) vce(cluster county) 


la var treated "Treated"

***************************************************************************************************************
* Online Appendix Table 10: Matching Counties - Panel B: Estimated Treatment Effect on Interest Rate
***************************************************************************************************************

* Interest rate
eststo clear 	
reghdfe interest_rate_prc treated $controls if propscore >= .2 & propscore <= .8 , absorb($fe) vce(cluster county) 
est store ps1, title("")
estadd local loanchars "YES": ps1
estadd local banktimefx "YES": ps1
estadd local indtimefx "YES": ps1
estadd local loantypefx "YES": ps1
estadd local loanpurposefx "YES": ps1


reghdfe interest_rate_prc treated $controls if propscore >= .2 & propscore <= .4, absorb($fe) vce(cluster county) 
est store ps2, title("")
estadd local loanchars "YES": ps2
estadd local banktimefx "YES":ps2
estadd local indtimefx "YES": ps2
estadd local loantypefx "YES": ps2
estadd local loanpurposefx "YES": ps2


reghdfe interest_rate_prc treated $controls if propscore > .4 & propscore <= .6,  absorb($fe) vce(cluster county) 
est store ps3, title("")
estadd local loanchars "YES": ps3
estadd local banktimefx "YES": ps3
estadd local indtimefx "YES": ps3
estadd local loantypefx "YES": ps3
estadd local loanpurposefx "YES": ps3

reghdfe interest_rate_prc treated $controls if propscore > .6 & propscore <= .8, absorb($fe) vce(cluster county)
est store ps4, title("")
estadd local loanchars "YES": ps4
estadd local banktimefx "YES": ps4
estadd local indtimefx "YES": ps4
estadd local loantypefx "YES": ps4
estadd local loanpurposefx "YES": ps4


#delimit;
esttab ps1 ps2 ps3 ps4 using "`$ialatexdir\'TablePScoreIR4Vars.tex", replace eqlabels(none) nomtitles nodepvars label 
		keep (treated $firmvarssize logdensityy logwage logwagefin logpopy) 
		order (treated $firmvarssize logdensityy logwage logwagefin logpopy) 
		cells(b(star fmt(3)) t(fmt(3) par abs)) 
		booktabs modelwidth(4) collabels(none) substitute(_ \_) 
	mgroups("Full matched sample" "P-score [.2, .4]" "P-score [.4, .6]" "P-score [.6, .8]",  pattern(1 1 1 1) 
		prefix(\multicolumn{@span}{c}{) suffix(})   
		span erepeat(\cmidrule(lr){@span}))         
	style(tex) star(* 0.10 ** 0.05 *** 0.01) 
	alignment(D{.}{.}{4,6}) 
	stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
		label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

***************************************************************************************************************
* Online Appendix Table 10: Matching Counties - Panel C: Estimated Treatment Effect on PD
***************************************************************************************************************
* PD
eststo clear 	
reghdfe pd_prc treated $controls if propscore >= .2 & propscore <= .8 , absorb($fe) vce(cluster county) 
est store ps1, title("")
estadd local loanchars "YES": ps1
estadd local banktimefx "YES": ps1
estadd local indtimefx "YES": ps1
estadd local loantypefx "YES": ps1
estadd local loanpurposefx "YES": ps1


reghdfe pd_prc treated $controls if propscore >= .2 & propscore <= .4, absorb($fe) vce(cluster county) 
est store ps2, title("")
estadd local loanchars "YES": ps2
estadd local banktimefx "YES":ps2
estadd local indtimefx "YES": ps2
estadd local loantypefx "YES": ps2
estadd local loanpurposefx "YES": ps2


reghdfe pd_prc treated $controls if propscore > .4 & propscore <= .6,  absorb($fe) vce(cluster county) 
est store ps3, title("")
estadd local loanchars "YES": ps3
estadd local banktimefx "YES": ps3
estadd local indtimefx "YES": ps3
estadd local loantypefx "YES": ps3
estadd local loanpurposefx "YES": ps3

reghdfe pd_prc treated $controls if propscore > .6 & propscore <= .8, absorb($fe) vce(cluster county)
est store ps4, title("")
estadd local loanchars "YES": ps4
estadd local banktimefx "YES": ps4
estadd local indtimefx "YES": ps4
estadd local loantypefx "YES": ps4
estadd local loanpurposefx "YES": ps4


#delimit;
esttab ps1 ps2 ps3 ps4 using "`$ialatexdir\'TablePScorePD4Vars.tex", replace eqlabels(none) nomtitles nodepvars label 
		keep (treated $firmvarssize logdensityy logwage logwagefin logpopy) 
		order (treated $firmvarssize logdensityy logwage logwagefin logpopy) 
		cells(b(star fmt(3)) t(fmt(3) par abs)) 
		booktabs modelwidth(4) collabels(none) substitute(_ \_) 
	mgroups("Full matched sample" "P-score [.2, .4]" "P-score [.4, .6]" "P-score [.6, .8]",  pattern(1 1 1 1) 
		prefix(\multicolumn{@span}{c}{) suffix(})   
		span erepeat(\cmidrule(lr){@span}))         
	style(tex) star(* 0.10 ** 0.05 *** 0.01) 
	alignment(D{.}{.}{4,6}) 
	stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
		label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

***************************************************************************************************************
* Online Appendix Table 10: Matching Counties - Panel D: Estimated Treatment Effect on Markup
***************************************************************************************************************
* Markup
eststo clear 	
reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd  $controls if propscore >= .2 & propscore <= .8 , absorb($fe) vce(cluster county) 
est store ps1, title("")
estadd local loanchars "YES": ps1
estadd local banktimefx "YES": ps1
estadd local indtimefx "YES": ps1
estadd local loantypefx "YES": ps1
estadd local loanpurposefx "YES": ps1


reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd  $controls if propscore >= .2 & propscore <= .4, absorb($fe) vce(cluster county) 
est store ps2, title("")
estadd local loanchars "YES": ps2
estadd local banktimefx "YES":ps2
estadd local indtimefx "YES": ps2
estadd local loantypefx "YES": ps2
estadd local loanpurposefx "YES": ps2


reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd  $controls if propscore > .4 & propscore <= .6,  absorb($fe) vce(cluster county) 
est store ps3, title("")
estadd local loanchars "YES": ps3
estadd local banktimefx "YES": ps3
estadd local indtimefx "YES": ps3
estadd local loantypefx "YES": ps3
estadd local loanpurposefx "YES": ps3

reghdfe interest_rate_prc treated pd_prc lgd_prc pdlgd  $controls if propscore > .6 & propscore <= .8, absorb($fe) vce(cluster county)
est store ps4, title("")
estadd local loanchars "YES": ps4
estadd local banktimefx "YES": ps4
estadd local indtimefx "YES": ps4
estadd local loantypefx "YES": ps4
estadd local loanpurposefx "YES": ps4


#delimit;
esttab ps1 ps2 ps3 ps4 using "`$ialatexdir\'TablePScoreMarkup4Vars.tex", replace eqlabels(none) nomtitles nodepvars label 
		keep (treated pd_prc lgd_prc pdlgd $firmvarssize logdensityy logwage logwagefin logpopy) 
		order (treated pd_prc lgd_prc pdlgd $firmvarssize logdensityy logwage logwagefin logpopy) 
		cells(b(star fmt(3)) t(fmt(3) par abs)) 
		booktabs modelwidth(4) collabels(none) substitute(_ \_) 
	mgroups("Full matched sample" "P-score [.2, .4]" "P-score [.4, .6]" "P-score [.6, .8]",  pattern(1 1 1 1) 
		prefix(\multicolumn{@span}{c}{) suffix(})   
		span erepeat(\cmidrule(lr){@span}))         
	style(tex) star(* 0.10 ** 0.05 *** 0.01) 
	alignment(D{.}{.}{4,6}) 
	stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
	layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
		label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr						


***************************************************************************************************************
* Online Appendix Table 2: Summary Statistics (Top Quartile of Number of Banks Versus the Rest of Sample)
***************************************************************************************************************

global sumstat_loan "amtm interest_rate_prc pd_prc lgd_prc pdlgd variability_float guarantee maturity delinq1_prc numprevlenders first type_line_all delinq1_prc default1_prc secured collateraltype5 staybank gsib"
global sumstat_firm "sizem salesm leverage profitability tangibility"
global sumstat_geog "numbankcounty numbank numgsib2015 numbankbranch logdensityy logwage logwagefin logpopy avgherfdepcty hhi"


* Sum stat comparing concentrated with unconcentrated (Online Appendix Table 2)
use __conc4Vars, clear
keep county yq treated unconcentrated
duplicates drop
merge 1:m county yq using "$master"

* all chars - concentrated
eststo clear  
eststo: estpost tabstat $sumstat_loan $sumstat_firm $sumstat_geog if treated, statistics(Mean Median) columns(statistics) 
esttab using "`$ialatexdir\'SumStat_conc4Vars.tex", cell((mean(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) p50(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) )) ///
	noobs nostar unstack nonote nonumber label fragment booktabs replace
	
* all chars - unconcentrated
eststo clear  
eststo: estpost tabstat $sumstat_loan $sumstat_firm $sumstat_geog if unconcentrated, statistics(Mean Median) columns(statistics) 
esttab using "`$ialatexdir\'SumStat_conc4Vars.tex", cell((mean(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) p50(label(\multicolumn{1}{c}{}) fmt(%9.2fc)) )) ///
	noobs nostar unstack nonote nonumber label fragment booktabs append
	



			

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// 													  Internet Appendix (3)															 //
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

* This section produces Internet Figure2 and Tables 6-8 (Non-private firm results)

cd "$datadir"
use "$public", clear

cd "$ialatexdir"
	
***************************************************************************************************************
* Labeling variables, values, etc.
***************************************************************************************************************
la var interest_rate_prc "Interest Rate (\%)"
la var maturity "Maturity (months)"
la var logmaturity "Log(maturity)"
la var amt "Loan Amount (USD)"
la var amtm "Amount (million USD)"
la var logamt "Log(Amount)"
la var secured "Secured"
la var guarantee "Guaranteed"
la var syndicated "Syndicated"
la var variability_float "Floating Interest Rate"
la var type_line_all "Line of Credit"
la var wrc "With Rate Ceiling"
la var wrf "With Rate Roof"

*la var borrower_rating "Internal Credit Rating"
la var pd_prc "Probability of Default (\%)"
la var lgd_prc "Loss Given Default (\%)"
la var pdlgd "Expected Loss (\%)"
la var pdlgd2 "Expected Loss\^2"

la var default1_prc "Realized Default (\%)"
la var delinq1_prc "Non-Performance (\%)"

la var sizem "Assets (million USD)"
la var size "Log(Assets)"
la var leverage "Leverage"
la var tangibility "Tangibility"
la var profitability "Profitability"

la var numbankcounty "Number of Banks"
la var numbankcounty1 "One Bank"
la var numbankmsa  "Number of Banks MSA"
la var numbankmsa1 "One Bank MSA"
la var numbankbranch "Number of All Banks"
la var numbankline "Number of Banks $\times$ Line of Credit"
la var numbankline1 "One Bank $\times$ Line of Credit"		

la var first "New Borrower"

la var hhimsa "MSA Loan HHI"
la var hhi "Loan HHI"
la var logdensity "Population Density"
la var logdensityy "Population Density"
la var logpop "Population"
la var logpopy "Population"
la var wage "Wages"
la var wagefin "Financial Industry Wages"
la var logwage "Wages"
la var logwagefin "Financial Industry Wages"
la var avgherfdepcty "Deposit HHI"


	
***************************************************************************************************************
* Global variables
***************************************************************************************************************
global loancontrols "logmaturity logamt guarantee"		//Loan charactersitics
global firmvarssize "size leverage tangibility profitability "		//Firm charactersitics
global fe "i.banktime i.iquarter i.credit_facility_purpose_cat i.line_variability "		//Fixed effects 1
global fenobanktime "i.iquarter i.credit_facility_purpose_cat i.line_variability "		//Fixed effects 2

global clustervar "taxid"	//Cluster varible, firm ID


***************************************************************************************************************
* Online Appendix Figure 2: The Effect of GSIB Surcharges on Lending Volume (All Loans)
***************************************************************************************************************

* aggregated 
reghdfe logvol numgsib2015 ib(#2).year#c.numgsib2015 if  year <= 2019 , absorb(county year) vce(cluster county)		
			
#delimit ;
	coefplot,  drop(_cons numgsib2015 staybank $loancontrols) xlabel( 1 "2014" 2 "2015" 3 "2016" 4 "2017" 5 "2018" 6 "2019" ,labsize(small) )   
			ciopts(lwidth(*1.5) recast(rcap) color(navy)) mcolor(navy) msize(*1.2)  levels(90) vertical 
			 baselevels yline(0, lcolor(maroon) lpattern(l)) omitted ;
#delimit cr	

graph export "FigGSIB1logvolall.png", as(png) replace
	


				
*********************************************************************************************************************************
* Online Appendix Table 6: Market Structure and Interest Rates (Public Firms)
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty $loancontrols if public, absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize  if public, absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin if public, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3 county5 using "TableNumbankIRPublic.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		

*********************************************************************************************************************************
* Online Appendix Table 7: Market Structure and Borrower Risk (Public Firms)
*********************************************************************************************************************************
eststo clear 	

reghdfe pd_prc numbankcounty $loancontrols if public, absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe pd_prc numbankcounty  $loancontrols  $firmvarssize if public, absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			
reghdfe pd_prc numbankcounty  $loancontrols  $firmvarssize logdensityy logwage logwagefin if public, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableNumbankPDPublic.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty   $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Probability of Default (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Characteristics Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


		

*********************************************************************************************************************************
* Online Appendix Table 8: Market Structure and Markups (Public Firms)
*********************************************************************************************************************************
eststo clear 	

reghdfe interest_rate_prc numbankcounty pd_prc lgd_prc pdlgd $loancontrols if public, absorb($fe) vce(cluster county) 
			est store county1, title("")
			estadd local loanchars "YES": county1
			estadd local banktimefx "YES": county1
			estadd local indtimefx "YES": county1
			estadd local loantypefx "YES": county1
			estadd local loanpurposefx "YES": county1
			
			
reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize if public, absorb($fe) vce(cluster county) 
			est store county3, title("")
			estadd local loanchars "YES": county3
			estadd local banktimefx "YES": county3
			estadd local indtimefx "YES": county3
			estadd local loantypefx "YES": county3
			estadd local loanpurposefx "YES": county3
			

reghdfe interest_rate_prc numbankcounty  pd_prc lgd_prc pdlgd $loancontrols  $firmvarssize logdensityy logwage logwagefin if public, absorb($fe) vce(cluster county) 
			est store county5, title("")
			estadd local loanchars "YES": county5
			estadd local banktimefx "YES": county5
			estadd local indtimefx "YES": county5
			estadd local loantypefx "YES": county5
			estadd local loanpurposefx "YES": county5

			
#delimit;
			esttab county1  county3  county5 using "TableNumbankIROneStagePublic.tex", replace eqlabels(none) nomtitles nodepvars label 
					keep (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					order (numbankcounty  pd_prc lgd_prc pdlgd  $firmvarssize logdensityy logwage logwagefin) 
					cells(b(star fmt(3)) t(fmt(3) par abs)) 
					booktabs modelwidth(3) collabels(none) substitute(_ \_) 
				mgroups("Interest Rate (\%)",  pattern(1 0 0) 
					prefix(\multicolumn{@span}{c}{) suffix(})   
					span erepeat(\cmidrule(lr){@span}))         
				style(tex) star(* 0.10 ** 0.05 *** 0.01) 
				alignment(D{.}{.}{4,6}) 
				stats(loanchars banktimefx indtimefx N r2_a, fmt(%~12s %~12s %~12s %9.0fc %9.2f)  
				layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}")
					label ("Loan Controls" "Bank-Quarter FE" "Industry-Quarter FE" "Observations" "Adj. R-squared"))
;					
#delimit cr		


log close