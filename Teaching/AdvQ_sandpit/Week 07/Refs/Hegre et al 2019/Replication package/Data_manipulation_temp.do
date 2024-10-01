/* Start of do-file: Data_manipulation.do */
/* Data manipulations for PKO paper */
capture drop _merge
display "Data manipulations"
capture gen primkey=(gwno*10000)+year
capture drop _merge
capture gen m_year=year
replace m_year=2000 if year>2000
replace primkey=(gwno*10000)+m_year
sort primkey
merge primkey using "negihbor.dta"
replace primkey=(gwno*10000)+year
sort primkey

display "primkey done"
