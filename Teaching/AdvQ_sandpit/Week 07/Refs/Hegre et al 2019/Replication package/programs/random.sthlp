{smcl}
{* *! version 1.0  26Feb2014}{...}
{findalias asfradohelp}{...}
{vieweralsosee "" "--"}{...}
{vieweralsosee "[R] help" "help help"}{...}
{viewerjumpto "Syntax" "examplehelpfile##syntax"}{...}
{viewerjumpto "Description" "examplehelpfile##description"}{...}
{viewerjumpto "Options" "examplehelpfile##options"}{...}
{viewerjumpto "Remarks" "examplehelpfile##remarks"}{...}
{viewerjumpto "Examples" "examplehelpfile##examples"}{...}
{title:Title}

{phang}
{bf:random} {hline 2} Calculate, draw, and store random effects  

{marker syntax}{...}
{title:Syntax}

{p 8 17 2}
{cmdab:random:}
{varlist}
[{if}]
[{cmd:,} {it:options}]

{synoptset 20 tabbed}{...}
{synopthdr}
{synoptline}
{syntab:Main}
{synopt:{opt varlist}} list of variables (yvar xvar ...) for estimating the random effects model{p_end}
{synopt:{opt random}} random effects variable{p_end}
{synopt:{opt drawyear}} year to draw from {p_end}
{synopt:{opt draws}} number of draws {p_end}
{synopt:{opt saveto}} filename for results dataset{p_end}{synoptline}
{p2colreset}{...}
{p 4 6 2}



{marker description}{...}
{title:Description}

{pstd}
{cmd:random} calculates, draws, and stores random effects from a multilevel  
logit model. The random effects are assumed to be normally distributed with 
mean and standard error equal to the estimated mean and standard error of the 
group specific random effects parameter estimated in the melogit model. Random
effects realizations are drawm from this group specific distribution.
The realized random effects are stored in a separate dataset

{marker options}{...}
{title:Options}

{dlgtab:Main}

{phang}
{opt random} A numeric or string variable that defines the groups the random effects 
should be calculated over. E.g. a country code variable. 

{phang}
{opt drawyear} A year (numeric) from which the estimated random effects should be drawn. Only one
value is allowed. The random effects do not vary across years so any random year will do.

{phang}
{opt draws} The number of draws (numeric) from the random effects distribution to realize

{phang}
{opt savetp} Filename the random effects will be stored to. The stored file includes a group 
identifier. 

{marker examples}{...}
{title:Examples}

{phang}{cmd:. random conflict c1t c2t ltsc0, random(gwno) drawyear(2012) countries(172) draws(50) saveto("randomeffects")}{p_end}
