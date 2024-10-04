/*********************************************************/
/**** This do file generates a dummy marking whether *****/
/**** a country in that year is a rotating member on *****/
/**** the UN sec. concil. Permanent members are coded ****/
/**** 1 throughout. 								  ****/
/**** Version 1.0 updated 04.01.2015 by HŒvard MN     ****/  
/*********************************************************/

gen securitycouncilmember = 0
label var securitycouncilmember "Rotating member of UN Sec. Council"

replace securitycouncilmember = 1 if gwno == 615 & year >= 1968 & year <= 1969  
replace securitycouncilmember = 1 if gwno == 615 & year >= 1988 & year <= 1989 
replace securitycouncilmember = 1 if gwno == 615 & year >= 2004 & year <= 2005 

replace securitycouncilmember = 1 if gwno == 540 & year >= 2003 & year <= 2004 
replace securitycouncilmember = 1 if gwno == 540 & year >= 2015 & year <= 2016 

replace securitycouncilmember = 1 if gwno == 160 & year >= 1948 & year <= 1949 
replace securitycouncilmember = 1 if gwno == 160 & year >= 1959 & year <= 1960 
replace securitycouncilmember = 1 if gwno == 160 & year >= 1966 & year <= 1967 
replace securitycouncilmember = 1 if gwno == 160 & year >= 1971 & year <= 1972 
replace securitycouncilmember = 1 if gwno == 160 & year >= 1987 & year <= 1988 
replace securitycouncilmember = 1 if gwno == 160 & year >= 1994 & year <= 1995 
replace securitycouncilmember = 1 if gwno == 160 & year >= 1999 & year <= 2000 
replace securitycouncilmember = 1 if gwno == 160 & year >= 2005 & year <= 2006 
replace securitycouncilmember = 1 if gwno == 160 & year >= 2013 & year <= 2014 

replace securitycouncilmember = 1 if gwno == 900  & year >= 2013 & year <= 2014 
replace securitycouncilmember = 1 if gwno == 900  & year >= 1946 & year <= 1947 
replace securitycouncilmember = 1 if gwno == 900  & year >= 1956 & year <= 1957 
replace securitycouncilmember = 1 if gwno == 900  & year >= 1973 & year <= 1974 
replace securitycouncilmember = 1 if gwno == 900  & year >= 1985 & year <= 1986 

replace securitycouncilmember = 1 if gwno == 305   & year >= 1973 & year <= 1974 
replace securitycouncilmember = 1 if gwno == 305   & year >= 1991 & year <= 1992 
replace securitycouncilmember = 1 if gwno == 305   & year >= 2009 & year <= 2010 

replace securitycouncilmember = 1 if gwno == 373   & year >= 2012 & year <= 2013  

replace securitycouncilmember = 1 if gwno == 692   & year >= 1998 & year <= 1999  

replace securitycouncilmember = 1 if gwno == 771  & year >= 1979 & year <= 1980 
replace securitycouncilmember = 1 if gwno == 771  & year >= 2000 & year <= 2001   

replace securitycouncilmember = 1 if gwno == 370  & year >= 1974 & year <= 1975  

replace securitycouncilmember = 1 if gwno == 211  & year >= 1947 & year <= 1948 
replace securitycouncilmember = 1 if gwno == 211  & year >= 1955 & year <= 1956  
replace securitycouncilmember = 1 if gwno == 211  & year >= 1971 & year <= 1972  
replace securitycouncilmember = 1 if gwno == 211  & year >= 1991 & year <= 1992  
replace securitycouncilmember = 1 if gwno == 211  & year >= 2007 & year <= 2008   

replace securitycouncilmember = 1 if gwno == 434 & year >= 1976 & year <= 1977 
replace securitycouncilmember = 1 if gwno == 434 & year >= 2004 & year <= 2005  

replace securitycouncilmember = 1 if gwno == 145 & year >= 1964 & year <= 1965  
replace securitycouncilmember = 1 if gwno == 145 & year >= 1978 & year <= 1979  

replace securitycouncilmember = 1 if gwno == 346 & year >= 2010 & year <= 2011 

replace securitycouncilmember = 1 if gwno == 571 & year >= 1995 & year <= 1996 

replace securitycouncilmember = 1 if gwno == 140 & year >= 1946 & year <= 1947 
replace securitycouncilmember = 1 if gwno == 140 & year >= 1951 & year <= 1952 
replace securitycouncilmember = 1 if gwno == 140 & year >= 1954 & year <= 1955 
replace securitycouncilmember = 1 if gwno == 140 & year >= 1963 & year <= 1964 
replace securitycouncilmember = 1 if gwno == 140 & year >= 1967 & year <= 1968 
replace securitycouncilmember = 1 if gwno == 140 & year >= 1988 & year <= 1989 
replace securitycouncilmember = 1 if gwno == 140 & year >= 1993 & year <= 1994 
replace securitycouncilmember = 1 if gwno == 140 & year >= 1998 & year <= 1999 
replace securitycouncilmember = 1 if gwno == 140 & year >= 2004 & year <= 2005 
replace securitycouncilmember = 1 if gwno == 140 & year >= 2010 & year <= 2011 

replace securitycouncilmember = 1 if gwno == 355 & year >= 1966 & year <= 1967 
replace securitycouncilmember = 1 if gwno == 355 & year >= 1986 & year <= 1987 
replace securitycouncilmember = 1 if gwno == 355 & year >= 2002 & year <= 2003 

replace securitycouncilmember = 1 if gwno == 439 & year >= 1984 & year <= 1985  
replace securitycouncilmember = 1 if gwno == 439 & year >= 2008 & year <= 2009  

replace securitycouncilmember = 1 if gwno == 516 & year >= 1970 & year <= 1971   

replace securitycouncilmember = 1 if gwno == 402 & year >= 1992 & year <= 1993  

replace securitycouncilmember = 1 if gwno == 471 & year >= 1974 & year <= 1975  
replace securitycouncilmember = 1 if gwno == 471 & year >= 2002 & year <= 2003

replace securitycouncilmember = 1 if gwno == 20 & year >= 1948 & year <= 1949 
replace securitycouncilmember = 1 if gwno == 20 & year >= 1958 & year <= 1959 
replace securitycouncilmember = 1 if gwno == 20 & year >= 1967 & year <= 1968 
replace securitycouncilmember = 1 if gwno == 20 & year >= 1977 & year <= 1978 
replace securitycouncilmember = 1 if gwno == 20 & year >= 1989 & year <= 1990 
replace securitycouncilmember = 1 if gwno == 20 & year >= 1999 & year <= 2000 

replace securitycouncilmember = 1 if gwno == 483 & year >= 2014 & year <= 2015  

replace securitycouncilmember = 1 if gwno == 155 & year >= 1952 & year <= 1953
replace securitycouncilmember = 1 if gwno == 155 & year >= 1961 & year <= 1962  
replace securitycouncilmember = 1 if gwno == 155 & year >= 1996 & year <= 1997  
replace securitycouncilmember = 1 if gwno == 155 & year >= 2003 & year <= 2004  
replace securitycouncilmember = 1 if gwno == 155 & year >= 2014 & year <= 2015    

replace securitycouncilmember = 1 if gwno == 710

replace securitycouncilmember = 1 if gwno == 100 & year >= 1947 & year <= 1948  
replace securitycouncilmember = 1 if gwno == 100 & year >= 1953 & year <= 1954 
replace securitycouncilmember = 1 if gwno == 100 & year >= 1957 & year <= 1958 
replace securitycouncilmember = 1 if gwno == 100 & year >= 1969 & year <= 1970 
replace securitycouncilmember = 1 if gwno == 100 & year >= 1989 & year <= 1990 
replace securitycouncilmember = 1 if gwno == 100 & year >= 2001 & year <= 2002 
replace securitycouncilmember = 1 if gwno == 100 & year >= 2011 & year <= 2012 

replace securitycouncilmember = 1 if gwno == 484 & year >= 1986 & year <= 1987 
replace securitycouncilmember = 1 if gwno == 484 & year >= 2006 & year <= 2007

replace securitycouncilmember = 1 if gwno == 94 & year >= 1975 & year <= 1974
replace securitycouncilmember = 1 if gwno == 94 & year >= 1997 & year <= 1998
replace securitycouncilmember = 1 if gwno == 94 & year >= 2008 & year <= 2009

replace securitycouncilmember = 1 if gwno == 437 & year >= 1964 & year <= 1965
replace securitycouncilmember = 1 if gwno == 437 & year >= 1990 & year <= 1991

replace securitycouncilmember = 1 if gwno == 344 & year >= 2008 & year <= 2009 

replace securitycouncilmember = 1 if gwno == 40 & year >= 1949 & year <= 1950
replace securitycouncilmember = 1 if gwno == 40 & year >= 1956 & year <= 1957  
replace securitycouncilmember = 1 if gwno == 40 & year >= 1990 & year <= 1991    

replace securitycouncilmember = 1 if gwno == 316 & year >= 1994 & year <= 1995

replace securitycouncilmember = 1 if gwno == 315 & year >= 1964 & year <= 1965 
replace securitycouncilmember = 1 if gwno == 315 & year >= 1978 & year <= 1979 

replace securitycouncilmember = 1 if gwno == 490 & year >= 1982 & year <= 1983 
replace securitycouncilmember = 1 if gwno == 490 & year >= 1990 & year <= 1991   

replace securitycouncilmember = 1 if gwno == 390 & year >= 1953 & year <= 1954  
replace securitycouncilmember = 1 if gwno == 390 & year >= 1967 & year <= 1968  
replace securitycouncilmember = 1 if gwno == 390 & year >= 1985 & year <= 1986  
replace securitycouncilmember = 1 if gwno == 390 & year >= 2005 & year <= 2006    

replace securitycouncilmember = 1 if gwno == 522 & year >= 1993 & year <= 1994  

replace securitycouncilmember = 1 if gwno == 130 & year >= 1950 & year <= 1951 
replace securitycouncilmember = 1 if gwno == 130 & year >= 1960 & year <= 1961 
replace securitycouncilmember = 1 if gwno == 130 & year >= 1991 & year <= 1992

replace securitycouncilmember = 1 if gwno == 651 & year == 1946
replace securitycouncilmember = 1 if gwno == 651 & year >= 1949 & year <= 1950 
replace securitycouncilmember = 1 if gwno == 651 & year >= 1984 & year <= 1985 
replace securitycouncilmember = 1 if gwno == 651 & year >= 1996 & year <= 1997 

replace securitycouncilmember = 1 if gwno == 530 & year >= 1967 & year <= 1968  
replace securitycouncilmember = 1 if gwno == 530 & year >= 1989 & year <= 1990  

replace securitycouncilmember = 1 if gwno == 375 & year >= 1969 & year <= 1970 
replace securitycouncilmember = 1 if gwno == 375 & year >= 1989 & year <= 1990 

replace securitycouncilmember = 1 if gwno == 220

replace securitycouncilmember = 1 if gwno == 481 & year >= 1978 & year <= 1979 
replace securitycouncilmember = 1 if gwno == 481 & year >= 1998 & year <= 1999 
replace securitycouncilmember = 1 if gwno == 481 & year >= 2010 & year <= 2011 

replace securitycouncilmember = 1 if gwno == 420 & year >= 1998 & year <= 1999  

replace securitycouncilmember = 1 if gwno == 260 & year >= 1977 & year <= 1978 
replace securitycouncilmember = 1 if gwno == 260 & year >= 1987 & year <= 1988 
replace securitycouncilmember = 1 if gwno == 260 & year >= 1995 & year <= 1996 
replace securitycouncilmember = 1 if gwno == 260 & year >= 2003 & year <= 2004 
replace securitycouncilmember = 1 if gwno == 260 & year >= 2011 & year <= 2012  

replace securitycouncilmember = 1 if gwno == 452 & year >= 1962 & year <= 1963   
replace securitycouncilmember = 1 if gwno == 452 & year >= 1986 & year <= 1987  
replace securitycouncilmember = 1 if gwno == 452 & year >= 2006 & year <= 2007  

replace securitycouncilmember = 1 if gwno == 350 & year >= 1952 & year <= 1953 
replace securitycouncilmember = 1 if gwno == 350 & year >= 2005 & year <= 2006 

replace securitycouncilmember = 1 if gwno == 90 & year >= 2012 & year <= 2013

replace securitycouncilmember = 1 if gwno == 438 & year >= 1972 & year <= 1973 
replace securitycouncilmember = 1 if gwno == 438 & year >= 2002 & year <= 2003 

replace securitycouncilmember = 1 if gwno == 404 & year >= 1996 & year <= 1997 

replace securitycouncilmember = 1 if gwno == 110 & year >= 1975 & year <= 1976 
replace securitycouncilmember = 1 if gwno == 110 & year >= 1982 & year <= 1983 

replace securitycouncilmember = 1 if gwno == 91 & year >= 1995 & year <= 1996

replace securitycouncilmember = 1 if gwno == 310 & year >= 1968 & year <= 1969 
replace securitycouncilmember = 1 if gwno == 310 & year >= 1992 & year <= 1993 

replace securitycouncilmember = 1 if gwno == 750 & year >= 1950 & year <= 1951  
replace securitycouncilmember = 1 if gwno == 750 & year >= 1967 & year <= 1968  
replace securitycouncilmember = 1 if gwno == 750 & year >= 1972 & year <= 1973  
replace securitycouncilmember = 1 if gwno == 750 & year >= 1977 & year <= 1978  
replace securitycouncilmember = 1 if gwno == 750 & year >= 1984 & year <= 1985  
replace securitycouncilmember = 1 if gwno == 750 & year >= 1991 & year <= 1992  
replace securitycouncilmember = 1 if gwno == 750 & year >= 2011 & year <= 2012  

replace securitycouncilmember = 1 if gwno == 850 & year >= 1973 & year <= 1974   
replace securitycouncilmember = 1 if gwno == 850 & year >= 1995 & year <= 1996 
replace securitycouncilmember = 1 if gwno == 850 & year >= 2007 & year <= 2008 

replace securitycouncilmember = 1 if gwno == 630 & year >= 1955 & year <= 1956  

replace securitycouncilmember = 1 if gwno == 645 & year >= 1957 & year <= 1958  
replace securitycouncilmember = 1 if gwno == 645 & year >= 1974 & year <= 1975 

replace securitycouncilmember = 1 if gwno == 205 & year == 1962
replace securitycouncilmember = 1 if gwno == 205 & year >= 1981 & year <= 1982 
replace securitycouncilmember = 1 if gwno == 205 & year >= 2001 & year <= 2002 

replace securitycouncilmember = 1 if gwno == 325 & year >= 1959 & year <= 1960 
replace securitycouncilmember = 1 if gwno == 325 & year >= 1971 & year <= 1972 
replace securitycouncilmember = 1 if gwno == 325 & year >= 1975 & year <= 1976 
replace securitycouncilmember = 1 if gwno == 325 & year >= 1987 & year <= 1988 
replace securitycouncilmember = 1 if gwno == 325 & year >= 1995 & year <= 1996 
replace securitycouncilmember = 1 if gwno == 325 & year >= 2007 & year <= 2008 

replace securitycouncilmember = 1 if gwno == 51 & year >= 1979 & year <= 1980  
replace securitycouncilmember = 1 if gwno == 51 & year >= 2000 & year <= 2001  

replace securitycouncilmember = 1 if gwno == 740 & year >= 1958 & year <= 1959 
replace securitycouncilmember = 1 if gwno == 740 & year >= 1966 & year <= 1967 
replace securitycouncilmember = 1 if gwno == 740 & year >= 1971 & year <= 1972 
replace securitycouncilmember = 1 if gwno == 740 & year >= 1975 & year <= 1976 
replace securitycouncilmember = 1 if gwno == 740 & year >= 1981 & year <= 1982 
replace securitycouncilmember = 1 if gwno == 740 & year >= 1987 & year <= 1988 
replace securitycouncilmember = 1 if gwno == 740 & year >= 1992 & year <= 1993 
replace securitycouncilmember = 1 if gwno == 740 & year >= 1997 & year <= 1998 
replace securitycouncilmember = 1 if gwno == 740 & year >= 2005 & year <= 2006 
replace securitycouncilmember = 1 if gwno == 740 & year >= 2009 & year <= 2010  

replace securitycouncilmember = 1 if gwno == 663 & year >= 1965 & year <= 1966  
replace securitycouncilmember = 1 if gwno == 663 & year >= 1982 & year <= 1983  
replace securitycouncilmember = 1 if gwno == 663 & year >= 2014 & year <= 2015  

replace securitycouncilmember = 1 if gwno == 501 & year >= 1973 & year <= 1974 
replace securitycouncilmember = 1 if gwno == 501 & year >= 1997 & year <= 1998  

replace securitycouncilmember = 1 if gwno == 690 & year >= 1978 & year <= 1979  

replace securitycouncilmember = 1 if gwno == 660 & year >= 1953 & year <= 1954  
replace securitycouncilmember = 1 if gwno == 660 & year >= 2010 & year <= 2011     

replace securitycouncilmember = 1 if gwno == 450 & year == 1961 

replace securitycouncilmember = 1 if gwno == 620 & year >= 1976 & year <= 1977 
replace securitycouncilmember = 1 if gwno == 620 & year >= 2008 & year <= 2009     

replace securitycouncilmember = 1 if gwno == 368 & year >= 2014 & year <= 2015

replace securitycouncilmember = 1 if gwno == 212 & year >= 2013 & year <= 2014

replace securitycouncilmember = 1 if gwno == 580 & year >= 1985 & year <= 1986

replace securitycouncilmember = 1 if gwno == 820 & year == 1965
replace securitycouncilmember = 1 if gwno == 820 & year >= 1989 & year <= 1990
replace securitycouncilmember = 1 if gwno == 820 & year >= 1999 & year <= 2000
replace securitycouncilmember = 1 if gwno == 820 & year >= 2015 & year <= 2016

replace securitycouncilmember = 1 if gwno == 432 & year >= 1966 & year <= 1967 
replace securitycouncilmember = 1 if gwno == 432 & year >= 2000 & year <= 2001 

replace securitycouncilmember = 1 if gwno == 338 & year >= 1983 & year <= 1984  

replace securitycouncilmember = 1 if gwno == 435 & year >= 1974 & year <= 1975   

replace securitycouncilmember = 1 if gwno == 590 & year >= 1977 & year <= 1978
replace securitycouncilmember = 1 if gwno == 590 & year >= 2001 & year <= 2002    

replace securitycouncilmember = 1 if gwno == 70 & year == 1946
replace securitycouncilmember = 1 if gwno == 70 & year >= 1980 & year <= 1981 
replace securitycouncilmember = 1 if gwno == 70 & year >= 2002 & year <= 2003 
replace securitycouncilmember = 1 if gwno == 70 & year >= 2009 & year <= 2010 

replace securitycouncilmember = 1 if gwno == 600 & year >= 1963 & year <= 1964  
replace securitycouncilmember = 1 if gwno == 600 & year >= 1992 & year <= 1993  
replace securitycouncilmember = 1 if gwno == 600 & year >= 2012 & year <= 2013  

replace securitycouncilmember = 1 if gwno == 565 & year >= 1999 & year <= 2000  

replace securitycouncilmember = 1 if gwno == 790 & year >= 1969 & year <= 1970
replace securitycouncilmember = 1 if gwno == 790 & year >= 1988 & year <= 1989  

replace securitycouncilmember = 1 if gwno == 210 & year == 1946
replace securitycouncilmember = 1 if gwno == 210 & year >= 1951 & year <= 1952  
replace securitycouncilmember = 1 if gwno == 210 & year >= 1965 & year <= 1966  
replace securitycouncilmember = 1 if gwno == 210 & year >= 1983 & year <= 1984  
replace securitycouncilmember = 1 if gwno == 210 & year >= 1999 & year <= 2000    

replace securitycouncilmember = 1 if gwno == 920 & year >= 1954 & year <= 1955
replace securitycouncilmember = 1 if gwno == 920 & year == 1966 
replace securitycouncilmember = 1 if gwno == 920 & year >= 1993 & year <= 1994  
replace securitycouncilmember = 1 if gwno == 920 & year >= 2015 & year <= 2016    

replace securitycouncilmember = 1 if gwno == 93 & year >= 1970 & year <= 1971 
replace securitycouncilmember = 1 if gwno == 93 & year >= 1983 & year <= 1984 

replace securitycouncilmember = 1 if gwno == 436 & year >= 1980 & year <= 1981

replace securitycouncilmember = 1 if gwno == 475 & year >= 1966 & year <= 1967
replace securitycouncilmember = 1 if gwno == 475 & year >= 1978 & year <= 1979 
replace securitycouncilmember = 1 if gwno == 475 & year >= 1994 & year <= 1995 
replace securitycouncilmember = 1 if gwno == 475 & year >= 2010 & year <= 2011 
replace securitycouncilmember = 1 if gwno == 475 & year >= 2014 & year <= 2015  

replace securitycouncilmember = 1 if gwno == 385 & year >= 1949 & year <= 1950
replace securitycouncilmember = 1 if gwno == 385 & year >= 1963 & year <= 1964 
replace securitycouncilmember = 1 if gwno == 385 & year >= 1979 & year <= 1980 
replace securitycouncilmember = 1 if gwno == 385 & year >= 2001 & year <= 2002  

replace securitycouncilmember = 1 if gwno == 698 & year >= 1994 & year <= 1995 

replace securitycouncilmember = 1 if gwno == 770 & year >= 1952 & year <= 1953
replace securitycouncilmember = 1 if gwno == 770 & year >= 1968 & year <= 1969 
replace securitycouncilmember = 1 if gwno == 770 & year >= 1976 & year <= 1977 
replace securitycouncilmember = 1 if gwno == 770 & year >= 1983 & year <= 1984 
replace securitycouncilmember = 1 if gwno == 770 & year >= 1993 & year <= 1994 
replace securitycouncilmember = 1 if gwno == 770 & year >= 2003 & year <= 2004
replace securitycouncilmember = 1 if gwno == 770 & year >= 2012 & year <= 2013

replace securitycouncilmember = 1 if gwno == 95 & year >= 1958 & year <= 1959
replace securitycouncilmember = 1 if gwno == 95 & year >= 1972 & year <= 1973
replace securitycouncilmember = 1 if gwno == 95 & year >= 1976 & year <= 1977
replace securitycouncilmember = 1 if gwno == 95 & year >= 1981 & year <= 1982
replace securitycouncilmember = 1 if gwno == 95 & year >= 2007 & year <= 2008

replace securitycouncilmember = 1 if gwno == 150 & year >= 1968 & year <= 1969 

replace securitycouncilmember = 1 if gwno == 135 & year >= 1955 & year <= 1956
replace securitycouncilmember = 1 if gwno == 135 & year >= 1973 & year <= 1974 
replace securitycouncilmember = 1 if gwno == 135 & year >= 1984 & year <= 1985 
replace securitycouncilmember = 1 if gwno == 135 & year >= 2006 & year <= 2007  

replace securitycouncilmember = 1 if gwno == 840 & year == 1957
replace securitycouncilmember = 1 if gwno == 840 & year == 1963
replace securitycouncilmember = 1 if gwno == 840 & year >= 1980 & year <= 1981  
replace securitycouncilmember = 1 if gwno == 840 & year >= 2004 & year <= 2005    

replace securitycouncilmember = 1 if gwno == 290 & year >= 1946 & year <= 1947 
replace securitycouncilmember = 1 if gwno == 290 & year == 1960
replace securitycouncilmember = 1 if gwno == 290 & year >= 1970 & year <= 1971 
replace securitycouncilmember = 1 if gwno == 290 & year >= 1982 & year <= 1983 
replace securitycouncilmember = 1 if gwno == 290 & year >= 1996 & year <= 1997 

replace securitycouncilmember = 1 if gwno == 235 & year >= 1979 & year <= 1980
replace securitycouncilmember = 1 if gwno == 235 & year >= 1997 & year <= 1998 
replace securitycouncilmember = 1 if gwno == 235 & year >= 2011 & year <= 2012 

replace securitycouncilmember = 1 if gwno == 694  & year >= 2006 & year <= 2007

replace securitycouncilmember = 1 if gwno == 732 & year >= 1996 & year <= 1997 
replace securitycouncilmember = 1 if gwno == 732 & year >= 2013 & year <= 2014 

replace securitycouncilmember = 1 if gwno == 360 & year == 1962 
replace securitycouncilmember = 1 if gwno == 360 & year >= 1976 & year <= 1977  
replace securitycouncilmember = 1 if gwno == 360 & year >= 1990 & year <= 1991 
replace securitycouncilmember = 1 if gwno == 360 & year >= 2004 & year <= 2005   

replace securitycouncilmember = 1 if gwno == 365

replace securitycouncilmember = 1 if gwno == 517 & year >= 1994 & year <= 1995
replace securitycouncilmember = 1 if gwno == 517 & year >= 2013 & year <= 2014

replace securitycouncilmember = 1 if gwno == 433 & year >= 1968 & year <= 1969 
replace securitycouncilmember = 1 if gwno == 433 & year >= 1988 & year <= 1989 

replace securitycouncilmember = 1 if gwno == 451  & year >= 1970 & year <= 1971

replace securitycouncilmember = 1 if gwno == 830 & year >= 2001 & year <= 2003

replace securitycouncilmember = 1 if gwno == 317 & year >= 2006 & year <= 2007

replace securitycouncilmember = 1 if gwno == 349 & year >= 1998 & year <= 1999

replace securitycouncilmember = 1 if gwno == 520 & year >= 1971 & year <= 1972 

replace securitycouncilmember = 1 if gwno == 560 & year >= 2007 & year <= 2008
replace securitycouncilmember = 1 if gwno == 560 & year >= 2011 & year <= 2012

replace securitycouncilmember = 1 if gwno == 230 & year >= 1969 & year <= 1970 
replace securitycouncilmember = 1 if gwno == 230 & year >= 1981 & year <= 1982 
replace securitycouncilmember = 1 if gwno == 230 & year >= 1993 & year <= 1994 
replace securitycouncilmember = 1 if gwno == 230 & year >= 2003 & year <= 2004 
replace securitycouncilmember = 1 if gwno == 230 & year >= 2015 & year <= 2016 

replace securitycouncilmember = 1 if gwno == 780 & year >= 1960 & year <= 1961

replace securitycouncilmember = 1 if gwno == 625 & year >= 1972 & year <= 1973

replace securitycouncilmember = 1 if gwno == 380 & year >= 1957 & year <= 1958
replace securitycouncilmember = 1 if gwno == 380 & year >= 1975 & year <= 1976
replace securitycouncilmember = 1 if gwno == 380 & year >= 1997 & year <= 1998

replace securitycouncilmember = 1 if gwno == 652 & year >= 1947 & year <= 1948
replace securitycouncilmember = 1 if gwno == 652 & year >= 1970 & year <= 1971
replace securitycouncilmember = 1 if gwno == 652 & year >= 2002 & year <= 2003

replace securitycouncilmember = 1 if gwno == 800 & year >= 1985 & year <= 1986

replace securitycouncilmember = 1 if gwno == 461 & year >= 1982 & year <= 1983
replace securitycouncilmember = 1 if gwno == 461 & year >= 2012 & year <= 2013

replace securitycouncilmember = 1 if gwno == 52 & year >= 1985 & year <= 1986 

replace securitycouncilmember = 1 if gwno == 616 & year >= 1959 & year <= 1960  
replace securitycouncilmember = 1 if gwno == 616 & year >= 1980 & year <= 1981 
replace securitycouncilmember = 1 if gwno == 616 & year >= 2000 & year <= 2001 

replace securitycouncilmember = 1 if gwno == 640 & year >= 1951 & year <= 1952
replace securitycouncilmember = 1 if gwno == 640 & year >= 1954 & year <= 1955  
replace securitycouncilmember = 1 if gwno == 640 & year == 1961
replace securitycouncilmember = 1 if gwno == 640 & year >= 2009 & year <= 2010    

replace securitycouncilmember = 1 if gwno == 500 & year == 1966
replace securitycouncilmember = 1 if gwno == 500 & year >= 1981 & year <= 1982
replace securitycouncilmember = 1 if gwno == 500 & year >= 2009 & year <= 2010

replace securitycouncilmember = 1 if gwno == 369 & year >= 1948 & year <= 1949
replace securitycouncilmember = 1 if gwno == 369 & year >= 1984 & year <= 1985 
replace securitycouncilmember = 1 if gwno == 369 & year >= 2000 & year <= 2001 

replace securitycouncilmember = 1 if gwno == 696 & year >= 1986 & year <= 1987

*replace securitycouncilmember = 1 if gwno == United Arab Republic 
*1961 Ğ 1962

replace securitycouncilmember = 1 if gwno == 200

replace securitycouncilmember = 1 if gwno == 510 & year >= 1975 & year <= 1976
replace securitycouncilmember = 1 if gwno == 510 & year >= 2005 & year <= 2006

replace securitycouncilmember = 1 if gwno == 2

replace securitycouncilmember = 1 if gwno == 165 & year >= 1965 & year <= 1966 


replace securitycouncilmember = 1 if gwno == 101 & year >= 1962 & year <= 1963
replace securitycouncilmember = 1 if gwno == 101 & year >= 1977 & year <= 1978
replace securitycouncilmember = 1 if gwno == 101 & year >= 1986 & year <= 1987
replace securitycouncilmember = 1 if gwno == 101 & year >= 1992 & year <= 1993
replace securitycouncilmember = 1 if gwno == 101 & year >= 2015 & year <= 2016

replace securitycouncilmember = 1 if gwno == 816 & year >= 2008 & year <= 2009

replace securitycouncilmember = 1 if gwno == 678 & year >= 1990 & year <= 1991 

replace securitycouncilmember = 1 if gwno == 345 & year >= 1950 & year <= 1951
replace securitycouncilmember = 1 if gwno == 345 & year == 1956
replace securitycouncilmember = 1 if gwno == 345 & year >= 1972 & year <= 1973
replace securitycouncilmember = 1 if gwno == 345 & year >= 1988 & year <= 1989

replace securitycouncilmember = 1 if gwno == 551 & year >= 1969 & year <= 1970 
replace securitycouncilmember = 1 if gwno == 551 & year >= 1979 & year <= 1980 
replace securitycouncilmember = 1 if gwno == 551 & year >= 1987 & year <= 1988 

replace securitycouncilmember = 1 if gwno == 552 & year >= 1983 & year <= 1984 
replace securitycouncilmember = 1 if gwno == 552 & year >= 1991 & year <= 1992 


