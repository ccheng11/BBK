/*
capture cd C:/Users/ARG/Dropbox/
capture cd C:/Users/Hhegre/Dropbox/
capture cd C:/Users/jonord/Dropbox/
capture cd C:/Users/havnyg/Dropbox/

cd "Programs/Data"
*/
capture drop smlreg
gen smlreg = 0
capture drop region
gen region = 0
capture label drop regions
label define regions 1 "Southern Africa" 2 "Central/South America" 3 "West Europe" 4 "East Europe" ///
	5 "MENA" 6 "West Africa" 7 "East Africa" 8 "Central Asia" 9 "East/South Asia" 10 "Arab League" 11 "Non-Arab League"
label values region regions

/* Region 1: Southern Africa */
/* Smlreg 11: West Southern Africa; 12: South Southern Africa; 13: East Southern Africa */
/* 11: Angola Botswana Namibia Zambia */
replace smlreg = 11 if gwno == 540 | gwno == 571 | gwno == 551 | gwno == 565 
/* 12: South S. Africa Lesotho Swaziland Zimbabwe */
replace smlreg = 12 if gwno == 552 | gwno == 560| gwno == 570 | gwno == 572 
/* 13: Mozambique Comoros Malawi Madagascar Mauritius */
replace smlreg = 13 if gwno == 580 | gwno == 581 | gwno == 590 | gwno == 541| gwno == 553
replace region = 1 if smlreg == 11 | smlreg == 12 | smlreg == 13
/* Region 2: Central/South America */
/* 21: Caribbean: Bahamas Barbados Cuba Dominican Republic Haiti Jamaica Trinidad and Tobago */
replace smlreg = 21 if gwno == 31 | gwno == 40 | gwno == 41 | gwno == 42 | gwno == 51 | gwno == 52 | gwno == 53 
/* 22: Central America */
replace smlreg = 22 if gwno == 70 | gwno == 80 | gwno == 90 | gwno == 91 | gwno == 92 | gwno == 93 | gwno == 94 | gwno == 95
/* 23: South America */
replace smlreg = 23 if gwno == 100 | gwno == 101 | gwno == 110 | gwno == 115 | gwno == 130 | gwno == 135
replace smlreg = 23 if gwno == 140 | gwno == 145 | gwno == 150 | gwno == 155 | gwno == 160 | gwno == 165
replace region = 2 if smlreg == 21 | smlreg == 22 | smlreg == 23
replace smlreg = 22 if smlreg == 21 /* Merge Caribbean and Central America */
/* 3/31: West Europe */
replace smlreg = 31 if gwno == 200 | gwno == 205 | gwno == 210 | gwno == 211 | gwno == 212 | gwno == 220 | gwno == 225 | gwno == 230 | gwno == 235 | gwno == 260
replace smlreg = 31 if gwno == 2 | gwno == 20
replace smlreg = 31 if gwno == 900 | gwno == 920
replace smlreg = 31 if gwno == 305 | gwno == 325 | gwno == 350 | gwno == 352
replace smlreg = 31 if gwno == 265 | gwno == 338
replace smlreg = 31 if gwno == 375 | gwno == 380 | gwno == 385 | gwno == 390 | gwno == 395
replace region = 3 if smlreg == 31
/* 4/41: East Europe */
replace smlreg = 41 if gwno == 355 | gwno == 359 | gwno == 360 | gwno == 365 | gwno == 366 | gwno == 367 | gwno == 368 | gwno == 369 | gwno == 370
replace smlreg = 41 if gwno == 290 | gwno == 310 | gwno == 315 | gwno == 316 | gwno == 317 | gwno == 339 | gwno == 340 | gwno == 341 
replace smlreg = 41 if gwno == 343 | gwno == 344 | gwno == 345 | gwno == 346 | gwno == 347 | gwno == 349
replace region = 4 if smlreg == 41
/* 5: Middle East and North Africa */
/* 51: North Africa */

replace smlreg = 51 if gwno == 600 | gwno == 615 | gwno == 616 | gwno == 620 | gwno == 625 | gwno == 651 
/* 52: Gulf countries */
replace smlreg = 52 if  gwno == 670 | gwno == 678 | gwno == 680 | gwno == 690 | gwno == 692 | gwno == 694 | gwno == 696 | gwno == 698
/* 53: The Levant */
replace smlreg = 53 if gwno == 640 | gwno == 645| gwno == 652 | gwno == 660 | gwno == 663 | gwno == 666
/* 54: Caucausus */
replace smlreg = 54 if gwno == 371 | gwno == 372 | gwno == 373 
replace region = 5 if smlreg >= 51 & smlreg <=54
/* 6: West Africa */
/* 61: Western West Africa */ 
replace smlreg = 61 if gwno == 402 | gwno == 404 | gwno == 420 | gwno == 432 | gwno == 433 
replace smlreg = 61 if gwno == 435 | gwno == 437 | gwno == 438 | gwno == 450 | gwno == 451 
/* 62: Easter West Africa */
replace smlreg = 62 if  gwno == 434 | gwno == 436 | gwno == 439| gwno == 452 | gwno == 461 | gwno == 475
replace region = 6 if smlreg == 61 | smlreg == 62
/* 7: Central and Eastern Africa */
/* 71: Central Africa */
replace smlreg = 71 if gwno == 411 | gwno == 471 | gwno == 481 | gwno == 482 | gwno == 483 | gwno == 484
/* 72: Eastern central Africa */
replace smlreg = 72 if gwno == 490 | gwno == 500 | gwno == 501 | gwno == 510 | gwno == 516 | gwno == 517 | gwno == 626  
/* 73: The Horn */
replace smlreg = 73 if gwno == 522 | gwno == 530 | gwno == 531 | gwno == 511 | gwno == 520 
replace region = 7 if smlreg == 71 | smlreg == 72 | smlreg == 73
/* 8: South Asia */
/* 81: Pakistan, Iran, Afghanistan */
replace smlreg = 81 if gwno == 630 | gwno == 700 | gwno == 770
/* 82: Other -stans */
replace smlreg = 82 if gwno == 701 | gwno == 702 | gwno == 703 | gwno == 704 | gwno == 705
/* 83: South Asia */
replace smlreg = 83 if gwno == 750 | gwno == 760 | gwno == 771 | gwno == 775 | gwno == 780 | gwno == 781 | gwno == 790 
replace region = 8 if smlreg == 81 | smlreg == 82 | smlreg == 83
/* 9: East Asia */
/* 91: North East */
replace smlreg = 91 if gwno == 710 | gwno == 712 | gwno == 713 | gwno == 731 | gwno == 732 | gwno == 740 | gwno == 840  
/* 92: Indochina */
replace smlreg = 92 if gwno == 800 | gwno == 811 | gwno == 812 | gwno == 816 | gwno == 817
/* 93: The islands */
replace smlreg = 93 if gwno == 820 | gwno == 830 | gwno == 835 | gwno == 850 | gwno == 860 | gwno == 910 | gwno == 940 | gwno == 950
replace region = 9 if smlreg >= 91 & smlreg <=93
/*
/* Arab consequences version */
replace region = 10 if (gwno == 615 | gwno == 692 | gwno == 581 | gwno == 522 | gwno == 651 | ///
	gwno == 645 | gwno == 663 | gwno == 690 | gwno == 660 | gwno == 620 | ///
	gwno == 435 | gwno == 600 | gwno == 698 | gwno == 694 | ///
	gwno == 670 | gwno == 520 | gwno == 625 | gwno == 652 | gwno == 616 | ///
	gwno == 696 | gwno == 678 )
	
replace region = 11 if region != 10
drop if gwno == 626

*/

capture drop regmarker
gen regmarker = 0
replace regmarker = 1 if gwno == 560 & region == 1
replace regmarker = 1 if gwno == 100 & region == 2
replace regmarker = 1 if gwno == 200 & region == 3
replace regmarker = 1 if gwno == 290 & region == 4
replace regmarker = 1 if gwno == 600 & region == 5
replace regmarker = 1 if gwno == 475 & region == 6
replace regmarker = 1 if gwno == 500 & region == 7
replace regmarker = 1 if gwno == 750 & region == 8
replace regmarker = 1 if gwno == 710 & region == 9
replace regmarker = 1 if gwno == 651 & region == 10
replace regmarker = 1 if gwno == 2 & region == 11

/* Dummy region variables */
forvalues r = 1(1)11 {
	capture drop reg`r'
	gen reg`r' = 0
	replace reg`r' = 1 if region == `r'
}

/*
keep gwno year reg*
drop if year != 2013
*/
save Regions, replace
