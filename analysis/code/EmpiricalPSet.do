/* EmpiricalPSet */
cd "~/Desktop/Independent/BehavioralPublicProblemSet/analysis/" // put your own file path here



* Replicate Table 1
//
// use "input/PreppedTESSData.dta", clear
//
//
// * Try to write out model for regression
//
//
// /* Regressing endline Treatment on endline willingness to pay - have demographic vars as controls and dummies for baseline WTP */
//
// reg WTP2 T $DemographicVars WTP1_C* [pweight=Weight], r



/* REPLICATE TABLE 1 */
global statfontsize = "footnotesize"
global DemographicVars = "Income Education Age Male Liberal Party Environmentalist ConserveEnergy OwnHome"



outreg,clear
use input/PreppedTESSData.dta, clear

/* Average Treatment Effects */
reg WTP2 T [pweight=Weight], robust, if WTP1!=. // Eliminate WTP1==. for sample consistency
	outreg, se merge replace varlabels tex fragment starlevels(10 5 1) sdec(2) statfont($statfontsize) summstat(r2 \ N) ///
		keep(T)

reg WTP2 T WTP1_C* [pweight=Weight], robust
	outreg, se merge replace varlabels tex fragment starlevels(10 5 1) sdec(2) statfont($statfontsize) summstat(r2 \ N) ///
		keep(T)

reg WTP2 T WTP1_C* $DemographicVars [pweight=Weight], robust
	outreg, se merge replace varlabels tex fragment starlevels(10 5 1) sdec(2) statfont($statfontsize) summstat(r2 \ N) ///
		keep(T)

	local baseline = _b[T]
		
* Eliminate top-coded and bottom-coded WTPs, because the largest WTP1 consumers have no way to reveal their increase in WTP.
reg WTP2 T WTP1_C* $DemographicVars [pweight=Weight], robust, if WTP1<=9&WTP1>=-9
	outreg, se merge replace varlabels tex fragment starlevels(10 5 1) sdec(2)  statfont($statfontsize) summstat(r2 \ N) ///
		keep(T)
		
* Consistency bias robustness check
reg WTP2 T EndlineOnly $DemographicVars [pweight=Weight], robust
		*reg WTP2 T [pweight=Weight], robust, if EndlineOnly==1|T==0
	outreg, se merge replace varlabels tex fragment starlevels(10 5 1) sdec(2)  statfont($statfontsize) summstat(r2 \ N) ///
		keep(T EndlineOnly)		
		
* T and TP
reg WTP2 T TP WTP1_C* $DemographicVars [pweight=Weight], robust
	outreg, se merge replace varlabels tex fragment starlevels(10 5 1) sdec(2)  statfont($statfontsize) summstat(r2 \ N) ///
		keep(T TP)

		
	local ratio3 = _b[T]/`baseline' // This is the scaling ratio for welfare calculations.
	display `ratio3'
		
outreg using "output/TESSATEs", replay replace tex fragment statfont($statfontsize) keep(T TP WTP1)  ///
				ctitles("","(1)","(2)","(3)","(4)","(5)","(6)") ///
				addrows("Baseline WTP Dummies $\mu$","No","Yes","Yes","Yes","No","Yes" \ ///
				"Individual Characteristics","No","No","Yes","Yes","Yes","Yes" \ ///
						"Exclude Max./Min. Baseline WTP","No","No","No","Yes","No","No" \ ///
						"Include Endline-Only Group","No","No","No","No","Yes","No")
						

				
				
use input/PreppedTESSData.dta, clear	
				
* Do Figure 3: Plots endline demand curves from TESS experiment using weights
	preserve

	* Get dummies for if WTP2 is greater than or equal to relative price of CFLs
	foreach i in -10 -8 -6 -4 -2 -1 0 1 2 3 4 6 8 10 {
		if (`i' < 0) local k = `i' + 30 
		else local k = `i'
		 /*if (`i' > 0) local neg 
		 else local neg neg */
		 gen buy_`k' = 0
		 replace buy_`k' = 1 if WTP2 >= `i'
	}

	* Get a single categorical variable that indicates treatment status
	gen T_status = 0
	replace T_status = 1 if T == 1 & EndlineOnly == 0
	replace T_status = 2 if EndlineOnly == 1

	// su buy_10 if EndlineOnly == 1 [aweight = Weight]
	// su buy_10 if T_status == 0 [aweight = Weight]

* Get marketshares by treatment status and relative price
	collapse (mean) buy*, by(T_status)

* reshape to get relative prices and market shares in single columns
	reshape long buy_, i(T_status) j(price)

* rename variable
	rename buy_ marketshare

* make prices negative again
	gen temp = price
	replace temp = price - 30 if price > 10

* drop old vars
	drop price 
	rename temp price

	sort T_status price


* graph demand curves by treatment
	tw 	(connected price marketshare if T_status == 0, color(cranberry) lpattern(dash) msymbol(D)) ///
		(connected price marketshare if T_status == 1, color(ebblue) lpattern(longdash_dot) msymbol(D)) ///
		(connected price marketshare if T_status == 2, color(orange) msymbol(D)), ///
		ytitle("CFL relative price (\$)") ylabel(-10(5)10, glpattern(1)) xlabel(0(.1)1, nogrid) xtitle("CFL market share") ///
		legend(order(2 "Treatment: Baseline and endline" 3 "Treatment: Endline only" 1 "Control") ///
		pos(8) ring(0))  yline(0, lstyle(1))
		
	gr export "~/Dropbox/Apps/Overleaf/Behavioral Public Problem Set/fig3.png", replace
	 
	restore
	
	
* Need to do figure 4:
	* Conditional Average Treatment Effect by Baseline WTP(smoothed out)
	* Take regression results
	
// xi: reg WTP2 T i.WTP1 [pweight = Weight] if WTP1smooth == 2.5
//
// di _b[T] + invttail(e(df_r),0.05)*_se[T]

	preserve

	gen tau = .
	gen cilow = .
	gen cihigh = .
	
	levelsof WTP1smooth, local(baselineWTP)
	di "`baselineWTP'"
	foreach x of local baselineWTP {
		di "Regressing for " "`x'"
		
		xi: reg WTP2 T i.WTP1 [pweight = Weight] if WTP1smooth == `x'
		replace tau = _b[T] if WTP1smooth == `x'
		* Construct 90% CI
		replace cilow = _b[T] - invttail(e(df_r),0.05)*_se[T] if WTP1smooth == `x'
		replace cihigh = _b[T] + invttail(e(df_r),0.05)*_se[T] if WTP1smooth == `x'
		
	}

	collapse tau cilow cihigh, by(WTP1smooth)

	tw ///
	(connected tau WTP1smooth, color(black) msymbol(D)) ///
	(line cilow WTP1smooth, lpattern(dash) color(black)) ///
	(line cihigh WTP1smooth, lpattern(dash) color(black)), ///
	yline(0, lpattern(1)) xline(0, lpattern(1)) legend(off) xtitle("Baseline relative WTP for CFL (\$)") ///
	xlabel(-4(2)10,nogrid) ylabel(-2(2)8,glpattern(1))  ytitle("Change in WTP for CFL (\$)")
	
	gr export "~/Dropbox/Apps/Overleaf/Behavioral Public Problem Set/fig4.png", replace
	
	* get average marginal bias
	
	restore

	
* Now figure 5
	* CDF of savings beliefs by treatment status
	preserve
	cumul Q7CFLSavings if T == 1, gen(cum_savingsT)
	cumul Q7CFLSavings if T == 0, gen(cum_savingsC)
	sort T cum_savingsT cum_savingsC
	
	tw	(line cum_savingsT Q7CFLSavings if Q7CFLSavings >= -10 & Q7CFLSavings <= 110, color(ebblue)) ///
		(line cum_savingsC Q7CFLSavings if Q7CFLSavings >= -10 & Q7CFLSavings <= 110, color(cranberry) lpattern(dash)), ///
		xlabel(-10(20)110, nogrid) xtitle("CFL savings belief (\$)") ylabel(0(.1)1,glpattern(1)) ytitle("Cumulative density")  ///
		legend(order(1 "Treatment" 2 "Control") pos(5) ring(0))
		
	gr export "~/Dropbox/Apps/Overleaf/Behavioral Public Problem Set/fig5.png", replace	
	restore	
	
	

	
	* Next steps - begin overleaf resopnse file - export figures to dropbox and add to overleaf
	
	
	
