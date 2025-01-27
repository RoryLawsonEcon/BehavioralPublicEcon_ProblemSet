cd "~/Desktop/Independent/BehavioralPublicProblemSet/analysis/"

use input/PreppedTESSData.dta, clear

/* 
							*How to calculate EPM*
1. Reshape TESS data so that there are two purchase observations per consumer:
	-> one at p_l and one at p_h
	-> S_p is indicator for whether observation is at the lower price
	
2. Estimate the following LPM:
	-> 1(Purchase CFP)_ip = \tau T_i + \eta S_p + \alpha T_i S_p + \epsilon_ip
	-> Cluster SE by consumer

3. Insert coefficeints into equation (5) of AT
	-> SE calculated using delta method
	-> Need to estimate tau hat, alpha hat, eta hat, and the gradient of p
*/


* Reshape data

	* Generate variable for whether or not CFL is purchased
	foreach vmax in 4 /*8 6 4 3 2 1 -1 -2*/ {
		local subsidy = 1
		if `vmax' >= 6 {
			local subsidy = 2
		}
		
		use input/PreppedTESSData.dta, clear
		
		gen purchaseCFL0 = cond(WTP2 >= `vmax',1,0) if !missing(WTP2)
		gen purchaseCFL1 = cond(WTP2 >= `vmax'-`subsidy',1,0) if !missing(WTP2)
		
		
		keep purchaseCFL* T EndlineOnly ID Weight

		reshape long purchaseCFL, i(ID) j(lowerprice)

		* get interaction variable
		gen lowerpricexT = lowerprice*T

		reg purchaseCFL T lowerprice lowerpricexT [pweight=Weight], cluster(ID)
		
		
		* Calculate EPM
		
		* Save in dataset
		

	}
	
	
	
							* EPM Formula *

* Coefficient Matrix - will contain Tau, Eta, and Alpha (in order)
	matrix define b = e(b)
	matrix define b = b[1..1,1..3]
	
* Covariance Matrix
	matrix define V = e(V)
	matrix define V = V[1..3,1..3]
	
* Calculate The Quantity Effect (numerator of EPM)
	* Need to define the gradient to get desired linear comb of coefs = Tau + (Alpha/2)	
	matrix define GradQuantityEffect = [1,0,.5]
	matrix define QuantityEffect = GradQuantityEffect*b' //multiply gradient by transpose of b
	local QuantityEffect = QuantityEffect[1,1]
	
* Standard Errors
	matrix define QuantityEffectSE = GradQuantityEffect*V*GradQuantityEffect'
	local QuantityEffectSE = (QuantityEffectSE[1,1])^(0.5)
	
* Calculate Subsidy Effect (denominator of EPM)
	* Want Gradient of Eta + Alpha
	matrix define GradSubsidyEffect = [0,1,1]
	matrix define SubsidyEffect = GradSubsidyEffect*b' //multiply gradient by transpose of b
	local SubsidyEffect = SubsidyEffect[1,1]
	
* Standard Errors
	matrix define SubsidyEffectSE = GradSubsidyEffect*V*GradSubsidyEffect'
	local SubsidyEffectSE = (SubsidyEffectSE[1,1])^(0.5)

* Calculate Demand Slope
	* Not exactly sure why, but demand slope is subsidy effect divided by subsidy
	local DemandSlope = `SubsidyEffect'/`Subsidy'
* Demand Slope SE
	local DemandSlopeSE = `SubsidyEffectSE'/`Subsidy'
	
* Calculate EPM
	local EPM = (`Subsidy'*`QuantityEffect')/(`SubsidyEffect')
	*assert `EPM' == `QuantityEffect'/(`SubsidyEffect'/`Subsidy')*

* Calculate EPM SE - Use gradient of EPM formula
	matrix define GradEPM = [`Subsidy'/`SubsidyEffect', -`Subsidy'*`QuantityEffect'/(`SubsidyEffect'^(2)), ///
	(0.5*`Subsidy')/(`SubsidyEffect') - (`Subsidy'*`QuantityEffect')/(`SubsidyEffect'^(2)) ]
	
	matrix define EPMSE = GradEPM*V*GradEPM'
	local EPMSE = (EPMSE[1,1])^(0.5)
	
	
* Display Everything
	** Quantity effect and SE
	display `QuantityEffect'
	display `QuantityEffectSE'
	
	** Subsidy effect and SE
	display `SubsidyEffect'
	display `SubsidyEffectSE'
	
	** Demand slope and SE
	display `DemandSlope'
	display `DemandSlopeSE'
	
	** EPM and SE
	display `EPM'
	display `EPMSE'
	

	
	

