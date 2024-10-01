*The roctabber

preserve
sort m_phat0
drop if m_phat0 ==.
local obs = _N
display `obs'
foreach outcome in 0 1 {
	egen rank_`outcome' = rank(m_phat0) if inc_0 == `outcome'
	replace rank_`outcome' = rank_`outcome'[_n-1] if rank_`outcome' ==. 
	replace rank_`outcome' = 0 if rank_`outcome' ==. 
}
foreach outcome in 0 1 {
	local theother = 1-`outcome'
	gen ranksum_`outcome' = 0
	replace ranksum_`outcome' = rank_`outcome' if _n == 1  & inc_0 == `outcome'
	replace ranksum_`outcome' = rank_`theother' + ranksum_`outcome'[_n-1] if _n > 1 & inc_0 == `outcome'
	egen maxranksum_`outcome' = max(ranksum_`outcome')
}

*list rank*
*gen U = (rank_0 * rank_1) + (rank_1*(rank_1-1))/2 - maxranksum_1 if _n ==_N
gen ROC0 = maxranksum_0 / (rank_0 * rank_1)
gen ROC1 = maxranksum_1 / (rank_0 * rank_1)
summ rank_0 rank_1 maxranksum_0 maxranksum_1 ROC* if _n == _N

restore
