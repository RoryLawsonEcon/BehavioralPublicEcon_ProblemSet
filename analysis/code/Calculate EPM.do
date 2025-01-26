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





* Example: get EPM for [2,3]	
gen purchaseCFL0 = cond(WTP2 >= 3,1,0) if !missing(WTP2)
gen purchaseCFL1 = cond(WTP2 >=  3-1,1,0) if !missing(WTP2)

keep purchaseCFL* T EndlineOnly ID Weight
reshape long purchaseCFL, i(ID) j(lowerprice)



* get interaction variable
gen TxpurchaseCFL = T*purchaseCFL

reg purchaseCFL T lowerprice TxpurchaseCFL [pweight=Weight], cluster(ID)






* Reshape data

	* Generate variable for whether or not CFL is purchased
	



	
	
	foreach vmax in 8 6 4 3 2 1 -1 -2 {
		local subsidy = 1
		if `vmax' >= 6 {
			local subsidy = 2
		}
		
		gen purchaseCFL0 = cond(WTP2 >= `vmax',1,0) if !missing(WTP2)
		gen purchaseCFL1 = cond(WTP2 >= `vmax'-`subsidy',1,0) if !missing(WTP2)
		
		
		keep purchaseCFL* T EndlineOnly ID Weight

		reshape long purchaseCFL, i(ID) j(lowerprice)

		* get interaction variable
		gen TxpurchaseCFL = T*purchaseCFL

		reg purchaseCFL T lowerprice TxpurchaseCFL [pweight=Weight], cluster(ID)

	}

	

