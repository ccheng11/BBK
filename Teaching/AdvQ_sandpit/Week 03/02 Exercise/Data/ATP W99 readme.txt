PEW RESEARCH CENTER
Wave 99 American Trends Panel 
Dates: Nov. 1-Nov. 7, 2021
Mode: Web
Sample: Full panel
Language: English and Spanish
N=10,260

***************************************************************************************************************************
NOTES

For a small number of respondents with high risk of identification, certain values have been randomly swapped with those of lower risk cases with similar characteristics.

F_RELCOM3 is the variable for the religious commitment index. The "high" religious commitment category includes those who say they attend religious services at least weekly, pray daily, and that religion is very important in their lives. 
The "low" religious commitment category includes those who say they seldom or never attend religious services, seldom or never pray, and that religion is "not too" or "not at all" important in their lives. 
The "medium" religious commitment category includes all other respondents, except for those who declined to answer any of the three questions about religious attendance, prayer frequency, or religion's importance; those declining to answer any of those three questions are not included in any of the religious commitment categories.     

***************************************************************************************************************************
WEIGHTS 


WEIGHT_W99 is the weight for the sample. Data for all Pew Research Center reports are analyzed using this weight.


***************************************************************************************************************************
Releases from this survey:

March 17, 2022 "AI and Human Enhancement: Americans’ Openness Is Tempered by a Range of Concerns"
https://www.pewresearch.org/internet/2022/03/17/ai-and-human-enhancement-americans-openness-is-tempered-by-a-range-of-concerns/ 

March 17, 2022 "5 key themes in Americans’ views about AI and human enhancement" 
https://www.pewresearch.org/fact-tank/2022/03/17/5-key-themes-in-americans-views-about-ai-and-human-enhancement/

May 4, 2022 "Highly religious Americans more skeptical of human enhancements such as brain implants, gene editing"
https://www.pewresearch.org/fact-tank/2022/05/04/highly-religious-americans-more-skeptical-of-human-enhancements-such-as-brain-implants-gene-editing/

July 14, 2022 "How Black Americans view the use of face recognition technology by police"
https://www.pewresearch.org/fact-tank/2022/07/14/how-black-americans-view-the-use-of-face-recognition-technology-by-police/

August 3, 2022 "U.S. women more concerned than men about some AI developments, especially driverless cars"
https://www.pewresearch.org/fact-tank/2022/08/03/u-s-women-more-concerned-than-men-about-some-ai-developments-especially-driverless-cars/

October 24, 2022 "Older Americans more wary than younger adults about prospect of driverless cars on the road"
https://www.pewresearch.org/fact-tank/2022/10/24/older-americans-more-wary-than-younger-adults-about-prospect-of-driverless-cars-on-the-road/


***************************************************************************************************************************
SYNTAX

Syntax to create F_RELCOM3CAT: 

recode F_RELIMP (1=1) (else=0) into highsalience.
recode F_ATTEND (1,2=1) (else=0) into highattend.
recode F_PRAY (1,2=1) (else=0) into highprayer.
count high = highsalience highattend highprayer (1).

recode F_RELIMP (3,4=1) (else=0) into lowsalience.
recode F_ATTEND (5,6=1) (else=0) into lowattend.
recode F_PRAY (6,7=1) (else=0) into lowprayer.
count low = lowsalience lowattend lowprayer (1).

compute F_RELCOM3CAT=2.
if high=3 F_RELCOM3CAT =1.
if low=3 F_RELCOM3CAT=3.
if F_RELIMP=99 or F_ATTEND=99 or F_PRAY=99 F_RELCOM3CAT =99.
val label F_RELCOM3CAT 1 "High" 2 "Medium" 3 "Low" 99 "DK/Ref". 
var label F_RELCOM3CAT 'religious commitment – 3 categories'.
execute.

**delete the additional variables

delete variables highsalience highattend highprayer high lowsalience lowattend lowprayer low.

