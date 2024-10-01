## 
## Drew Linzer
## dlinzer@emory.edu
## February 28, 2012
## 
## AntiAmerican-replication.R
## 
## Replication code for:
## Blaydes, Lisa and Drew A. Linzer. 2012. Elite Competition, 
##   Religiosity, and Anti-Americanism in the Islamic World. 
##   American Political Science Review. 106(2): 225-243.
## 

library(foreign)
library(car)
library(R2WinBUGS)
library(MASS)
library(poLCA)
library(ltm)

#setwd("C:/...")
setwd("C:/Users/polar/Downloads/Sandbox/Teaching/AdvQ/Week 04/02 Exercise/Original")

## Bar charts of media content analysis
## number of articles positive, neutral, negative

sen.sol <- c(35,22,8)
sen.sud <- c(15,14,15)
sen <- sen.sol+sen.sud

mor.auj <- c(23,40,40)
mor.mat <- c(23,38,18)
mor <- mor.auj+mor.mat

turk.hurr <- c(7,24,49)
turk.zam <- c(16,18,37)
turk <- turk.hurr+turk.zam

windows(6,3.5)
par(mar=c(2.5,4,1,1),las=1)
barplot(100*cbind(sen/sum(sen),mor/sum(mor),turk/sum(turk)),beside=T,
        ylab="Percent of articles",axes=T,ylim=c(0,70),axis.lty=1,
        col=c("gray90","gray60","gray30"),cex.lab=1.2,cex.names=1.2,
        names.arg=c("Senegal","Morocco","Turkey"),cex.axis=1.2,
        legend.text=c("Positive","Neutral","Negative"),
        args.legend=list(x=10.5,y=72,bty="n",ncol=3))
text(seq(1.5,11.5,1),5+100*c(sen/sum(sen),NA,mor/sum(mor),NA,turk/sum(turk)),
     round(100*c(sen/sum(sen),0,mor/sum(mor),0,turk/sum(turk))))
#savePlot("mediabars",type="pdf")


## Pew GAP 2007 ##
## http://pewglobal.org/database/?indicator=1&survey=8&response=Favorable&mode=table
## http://pewglobal.org/datasets/

gap07 <- read.spss("GAP_2007_Data.sav",to.data.frame=T)
dat <- gap07
dat$cnum <- as.numeric(dat$country)

# Muslim respondents
levels(dat$country)[levels(dat$country)=="Palestinian Territories"] <- "Palestinian ter."
dat$muspray <- 0+(!is.na(dat$Q114))     # 11436 Muslims asked about prayer
dat$Q44GHA[is.na(dat$Q44GHA)] <- "Don't know"
dat$Q44KEN[is.na(dat$Q44KEN)] <- "Don't know"
levels(dat$Q44INDA) <- c(levels(dat$Q44INDA),"Don't know")
dat$Q44INDA[is.na(dat$Q44INDA)] <- "Don't know"
dat$Q44MOR[is.na(dat$Q44MOR)] <- "Don't know"
dat$Q44UGA[is.na(dat$Q44UGA)] <- "Other "
dat$muslim <- 0+(dat$muspray==1 | dat$Q44GHA=="Muslim" | dat$Q44KEN=="Islam" |
                 dat$Q44INDA=="Muslim" | dat$Q44MOR=="Islam" | dat$Q44UGA=="Islam")

## Data set of 12,831 Muslims in 21 countries
mus <- dat[dat$muslim==1,]
cnames <- names(table(mus$country)[table(mus$country)>0])
mus$cnum2 <- recode(mus$cnum,"2=1;10=2;11=3;14=4;16=5;17=6;20=7;22=8;
                              23=9;24=10;25=11;26=12;27=13;29=14;30=15;
                              31=16;32=17;36=18;42=19;43=20;45=21")

## CREATE DERIVED VARIABLES FOR ANALYSIS

## Attitudes towards the United States
mus$us <- as.numeric(mus$Q16A)          # [1] opinions of US (4=worst)
mus$us[mus$us>4] <- NA

mus$amer <- as.numeric(mus$Q16B)        # [2] opinions of Americans (4=worst)
mus$amer[mus$amer>4] <- NA

mus$spread <- as.numeric(mus$Q27)       # [3] spread of American customs (2=bad)
mus$spread[mus$spread>2] <- NA

mus$democ <- as.numeric(mus$Q28)        # [4] American ideas re democracy (2=dislike)
mus$democ[mus$democ>2] <- NA

mus$bus <- as.numeric(mus$Q29)          # [5] American ideas re business (2=dislike)
mus$bus[mus$bus>2] <- NA

mus$movies <- as.numeric(mus$Q30)       # [6] American music, movies, TV (2=dislike)
mus$movies[mus$movies>2] <- NA

mus$admire <- as.numeric(mus$Q31)       # [7] Admire America's tech (2=do not admire)
mus$admire[mus$admire>2] <- NA

mus$us.scale <- rowMeans(cbind((mus$us-1)/3,(mus$amer-1)/3,mus$spread-1,
                         mus$democ-1,mus$bus-1,mus$movies-1,mus$admire-1),na.rm=T)
mus$us.scale[is.na(mus$us.scale)] <- NA

# Anti-Americanism scale has a Cronbach's alpha of 0.78
cronbach.alpha(cbind((mus$us-1)/3,(mus$amer-1)/3,mus$spread-1,
                         mus$democ-1,mus$bus-1,mus$movies-1,mus$admire-1),na.rm=T)

## Attitudes about religion and government
mus$struggle <- 2-as.numeric(mus$Q75)   # 1=think there is a struggle
mus$struggle[mus$struggle<0] <- NA      #   between modernizers/Fundamentalists

mus$id.fund <- as.numeric(mus$Q75B)-1   # 1=identify with fundamentalists, if see struggle
mus$id.fund[mus$id.fund>1] <- NA


## Religiosity
mus$pray <- as.numeric(mus$Q114)
mus$pray[mus$pray>7] <- NA
mus$pray5 <- (mus$pray==7)+0            # 1=pray 5x/day

mus$fast <- as.numeric(mus$Q116)
mus$fast[mus$fast>4] <- NA

mus$relimpt <- as.numeric(mus$Q117)
mus$relimpt[mus$relimpt>4] <- NA
mus$relimpt <- 5-mus$relimpt

table(mus$fast,mus$relimpt,mus$pray5)
table(mus$relimpt)/sum(table(mus$relimpt))
table(mus$fast[mus$relimpt==4])/sum(table(mus$fast[mus$relimpt==4]))
table(mus$pray5[mus$relimpt==4])/sum(table(mus$pray5[mus$relimpt==4]))

mus$pious <- ((mus$pray5==1) & (mus$fast>=3) & (mus$relimpt==4)) + 0
mus$pious[is.na(mus$pray5) | is.na(mus$fast)] <- NA

# Validate dichotomous coding scheme with a latent class model
if (F) {
mus$pray5b <- mus$pray5+1
lc <- poLCA(cbind(pray5b,fast,relimpt)~1,mus,nclass=2,na.rm=F)
plot(lc)
# posterior classification finds expected clusters
table(lc$predclass,mus$pious)
windows()
hist(lc$posterior[,1])
}


## Attitudinal scale: policy and economic evaluations
mus$righttrack <- 2-as.numeric(mus$Q7)      # [1] satisfied with how things going in country
mus$righttrack[mus$righttrack<0] <- NA

mus$econsit <- (4-as.numeric(mus$Q11))/3    # [2] current economic situation in country
mus$econsit[mus$econsit<0] <- NA

mus$kids <- (2-as.numeric(mus$Q13))         # [3] children better off or worse off than people are now?
mus$kids[mus$kids<0] <- NA

mus$govt <- (4-as.numeric(mus$Q23A))/3      # [4] influence of national government
mus$govt[mus$govt<0] <- NA

mus$satisfied <- rowMeans(cbind(mus$righttrack,mus$econsit,mus$kids,mus$govt),na.rm=T)
mus$satisfied[is.na(mus$satisfied)] <- NA

## Media awareness
mus$news <- as.numeric(mus$Q37)-1       # follow int'l news (2=most of the time)
mus$news[mus$news>1] <- NA

# compare to percentages in other countries
table(dat$country,dat$Q37)
sort(table(dat$country,dat$Q37)[,2]/rowSums(table(dat$country,dat$Q37)[,1:2]))
sort(table(mus$country,mus$news)[,2]/rowSums(table(mus$country,mus$news)))


## Demographic variables: age, gender, economic status, education
mus$male <- 2-as.numeric(mus$Q107)  # 1=male, 0=female
mus$age <- as.numeric(mus$Q108)
mus$age[mus$age>97] <- NA


# Socioeconomic status
sesvars <- c("Q123BAN","Q123EGY","Q123ETH","Q123GHA","Q123INDA","Q123INDO","Q123IVO",
             "Q123JOR","Q123KEN","Q123KUW","Q123LEB","Q123MALA","Q123MALI","Q123MOR",
             "Q123NIG","Q123PAK","Q123PAL","Q123SEN","Q123TAN","Q123TUR","Q123UGA")
sesmat <- mus[,sesvars]
mus$ses <- NA
for (i in 1:ncol(sesmat)) {
    seltab <- table(sesmat[,i])
    for (j in 1:length(seltab)) {
        mus$ses[(mus$country==cnames[i]) & (sesmat[,i]==names(seltab)[j])] <- j-1
    }
    mus$ses[grep("Refused",sesmat[,i])] <- NA
    mus$ses[grep("know",sesmat[,i])] <- NA
    mus$ses[mus$country==cnames[i]] <- mus$ses[mus$country==cnames[i]]/max(mus$ses[mus$country==cnames[i]],na.rm=T)
}


# Education level
edvars <- c("Q118BAN","Q118EGY","Q118ETH","Q118GHA","Q118INDA","Q118INDO","Q118IVO",
            "Q118JOR","Q118KEN","Q118KUW","Q118LEB","Q118MALA","Q118MALI","Q118MOR",
            "Q118NIG","Q118PAK","Q118PAL","Q118SEN","Q118TAN","Q118TUR","Q118UGA")
edmat <- mus[,edvars]   # highest level of education
apply(edmat,2,table)
edlevels <- sort(names(table(as.matrix(edmat)))) # 71 outcome categories
# in Pakistan, "Matric" means secondary school

primary <- c(" Incomplete primary school",
                " No formal education",
                "5-9 classes",
                "Complete primary",
                "Complete primary school",
                "Completed elementary",
                "Completed primary school",
                "Did not complete intermediate",
                "Illiterate",
                "Incomplete primary",
                "Incomplete primary school",
                "Less than 5 classes",
                "Literate but no formal schooling",
                "No formal education",
                "No formal education but can read and write",
                "School up to 4 years",
                "School up to 5 to 9 years",
                "Some elementary or less")

secondary <- c("Complete JSS/Middle school",
                "Complete Polytechnic",
                "Complete secondary (preparatory)",
                "Complete secondary (vocational)",
                "Complete secondary school",
                "Complete secondary school technical/vocational type",
                "Complete secondary school: technical/vocational type",
                "Complete Secondary University-preparatory type",
                "Complete secondary: university-preparatory type",
                "Complete SSS/Vocational/Technical/ School",
                "Completed vocational/technical institute",
                "Completed complementary: not sec/vocational ",
                "Completed secondary",
                "Completed secondary school",
                "Did not complete secondary",
                "Incomplete JSS/Middle school",
                "Incomplete Polytechnic",
                "Incomplete secondary (preparatory)",
                "Incomplete secondary (vocational)",
                "Incomplete secondary school",
                "Incomplete secondary school technical/vocational type",
                "Incomplete secondary school: technical/vocational type",
                "Incomplete secondary University-preparatory type",
                "Incomplete secondary: university-preparatory type",
                "Incomplete SSS/Vocational/Technical/ School",
                "Intermediate",
                "Matric",
                "Some secondary",
                "SSC/HSC")

university <- c("Complete College of Education",
                "Completed college",
                "Completed college (Diploma/Certificate)",
                "Completed diploma/ vocational type",
                "Completed university in Middle East/Africa",
                "Completed university in Western Europe/America",
                "Completed university or more",
                "Entered university, did not complete",
                "Graduate",
                "Graduate/Post grad-Gen BA MSc BCom etc",
                "Graduate/Post grad-Prof BE MTech MBA MBBS etc",
                "Incomplete College of Education",
                "Post-graduate",
                "Some college (Diploma/Certifiate)",
                "Some college but not graduated",
                "Some post-secondary (university or technical)",
                "Some university-level education, without a degree",
                "Some university without degree",
                "University-level education, with a degree",
                "University-level education, with degree",
                "University-level education, without degree",
                "University with degree")

other <- c("Don't know","Refused")

length(c(primary,secondary,university,other)) # 71

mus$educ <- NA
for (i in 1:ncol(edmat)) {
    mus$educ[edmat[,i] %in% primary] <- 1
    mus$educ[edmat[,i] %in% secondary] <- 2
    mus$educ[edmat[,i] %in% university] <- 3
}

mus$ed2 <- 0+(mus$educ==2)
mus$ed3 <- 0+(mus$educ==3)


## Set up country-level variables

# Percent of country that is Muslim. Source: Pew study, http://pewforum.org/docs/?DocID=450
# Distance in 1000 miles between country capital and Jerusalem. Source: Google maps
# Imports from the US, in thousands of dollars, 2007. Source: http://tse.export.gov
# Population, total, 2007. Source: World Bank WDI
# GDP per capita (current US$), 2005. Source: World Bank WDI
# Total Economic Assistance (no military), 2007, in millions, historical 2007 $US
#   Source: U.S. Overseas Loans and Grants [Greenbook] http://gbk.eads.usaidallnet.gov/
# Gallup world poll: http://www.gallup.com/poll/142631/Worldwide-Leadership-Approval.aspx

cvars <- read.csv("cvars.csv",row.names=1)
struggle <- (table(mus$cnum,mus$struggle)/rowSums(table(mus$cnum,mus$struggle)))[,2]
struggle[is.na(struggle)] <- NA
cvars$struggle <- struggle
pctrel <- (table(mus$cnum,mus$pious)/rowSums(table(mus$cnum,mus$pious)))[,2]
pctrel[is.na(pctrel)] <- NA
cvars$pctrel <- pctrel
cvars$pctAA <- as.vector((table(mus$cnum,mus$us)/rowSums(table(mus$cnum,mus$us)))[,4])
cvars$meanAA <- sapply(split(mus$us.scale,mus$cnum),mean,na.rm=T)
cvars$log.usimppc <- log(cvars$usimppc)
cvars$log.gdppc <- log(cvars$gdppc)

cvars$gallup <- (100-c(34,6,NA,69,31,46,94,9,82,43,25,23,87,33,73,11,13,76,76,16,84))/100
cor(cvars$gallup,cvars$meanAA,use="complete.obs") #plot(cvars$gallup,cvars$meanAA)

## Tabulate pro-/anti-American using a barplot
anti34 <- rowSums((table(mus$cnum,mus$us)/rowSums(table(mus$cnum,mus$us)))[,3:4])
o <- order(anti34)

windows()
par(mar=c(2.5,8,1,1),las=1)
barplot(t(table(mus$cnum,mus$us)/rowSums(table(mus$cnum,mus$us)))[2:1,o],
        horiz=T,names.arg=cnames[o],xlim=c(-1.1,1.1),xaxt="n",space=0.5,cex.names=1.2)
barplot(-t(table(mus$cnum,mus$us)/rowSums(table(mus$cnum,mus$us)))[3:4,o],
        horiz=T,add=T,xaxt="n",yaxt="n",space=0.5)
axis(1,at=seq(-1,1,0.2),labels=c(seq(100,0,-20),seq(20,100,20)))
abline(v=0)
text(rep(-0.95,21),1.5*c(1:21)-0.65,format(round(100*anti34[o])),pos=2)
text(rep(1.05,21),1.5*c(1:21)-0.65,format(100-round(100*anti34[o])),pos=2)
mtext("Percent anti-American              Percent pro-American",side=3,font=2)
#savePlot("countryAA",type="pdf")

## Scatterplot average AA vs. pct seeing a struggle (asked in 17 countries)
windows(5.5,5.5)
par(mar=c(4.5,4,1,1),las=1)
plot(meanAA~struggle,cvars,xlim=c(0,0.8),ylim=c(0.1,1),col="white",
        cex.axis=1.2,cex.lab=1.2,
        xlab="Proportion seeing a reformer-fundamentalist struggle",
        ylab="Average unfavorability of United States")
text(cvars$struggle,cvars$meanAA,cnames)
#savePlot("AAstruggle",type="pdf")

## Scatterplot percent strongly AA vs. pct seeing a struggle (asked in 17 countries)
windows(5.5,5.5)
par(mar=c(4.5,4,1,1),las=1)
plot(pctAA~struggle,cvars,xlim=c(0,0.8),ylim=c(0,1),col="white",
        cex.axis=1.2,cex.lab=1.2,
        xlab="Proportion seeing a reformer-fundamentalist struggle",
        ylab="Proportion strongly unfavorable of the United States")
text(cvars$struggle,cvars$pctAA,cnames)
#savePlot("AAstruggle2",type="pdf")

## Plot mean AA vs. percent religious
windows(5.5,5.5)
par(mar=c(4.5,4,1,1),las=1)
plot(meanAA~pctrel,cvars,ylim=c(0.1,1),xlim=c(0.2,1),col="white",
        cex.axis=1.2,cex.lab=1.2,
        xlab="Proportion highly religious",
        ylab="Average unfavorability of United States")
text(cvars$pctrel,cvars$meanAA,cnames)
#savePlot("AAreligious",type="pdf")



## 
## Analysis of 2007 survey: HLM using WinBUGS
## 

y <- mus$us.scale               # Anti-Americanism scale: 1=most AA, 0=least AA
pious <- mus$pious              # Individual: 1=highly religious, 0=less religious
news <- mus$news                # Follow int'l news: 1=frequently, 0=only when important
age <- mus$age/100
male <- mus$male
ses <- mus$ses
ed2 <- mus$ed2
ed3 <- mus$ed3
satis <- mus$satisfied
z1 <- cvars$struggle            # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus              # Country: percent Muslim in country
z3 <- cvars$log.gdppc           # Country: log of per capita GDP

saveRDS(mus, "Pew GAP 2007.RData")
saveRDS(cvars, "Country Covariates.RData")

## Model 2, with z1 as percent religious instead of percent seeing a struggle
#z1 <- cvars$pctrel              # Country: percent religious

## Alternative DV, using just the 4-item Anti-Americanism question (appendix)
#y <- (mus$us-1)/3               #  Anti-Americanism: 1=most AA, 0=least AA

## Alternative IV for piety; does respondent support 1=fundamentalists or 0=modernizers
#pious <- mus$id.fund

cnum <- mus$cnum2
N <- nrow(mus)
ncountry <- max(cnum)

# Models 1-4
inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(4,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
                    }

model.file <- "AA-GAP-hlm.txt"
bugs.model <- bugs(data=c("y","pious","news","z1","z2","z3",
                          "cnum","N","ncountry"),
                 inits=inits,
                 parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                      "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                 model.file=model.file,
                 n.chains=3,
                 n.iter=500,
                 debug=TRUE)

# Model estimates
round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)



# Models 5 and 6

ed2init <- NA
ed2init[is.na(ed2)] <- rbinom(sum(is.na(ed2)),1,0.5)
ed3init <- NA
ed3init[is.na(ed3)] <- rbinom(sum(is.na(ed3)),1,0.5)
newsinit <- NA
newsinit[is.na(news)] <- rbinom(sum(is.na(news)),1,0.5)
piousinit <- NA
piousinit[is.na(pious)] <- rbinom(sum(is.na(pious)),1,0.6)
sesinit <- NA
sesinit[is.na(ses)] <- rbeta(sum(is.na(ses)),3,5)

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           b.age=runif(ncountry,0,0.1),
                           b.male=runif(ncountry,-1,1),
                           b.ses=runif(ncountry,-1,1),
                           b.ed2=runif(ncountry,-1,1),
                           b.ed3=runif(ncountry,-1,1),
                           b.satis=runif(ncountry,-1,1),
                           g.country=runif(4,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,9),
                           ed2=ed2init,
                           ed3=ed3init,
                           news=newsinit,
                           pious=piousinit,
                           ses=sesinit,
                           p.pious=0.6,
                           p.news=0.6)
                    }

model.file <- "AA-GAP-hlm2.txt"
bugs.model <- bugs(data=c("y","z1","z2","z3","pious","news",
                          "age","male","ses","ed2","ed3","satis",
                          "cnum","N","ncountry"),
                 inits=inits,
                 parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                      "b.age","b.male","b.ses","b.ed2","b.ed3","b.satis",
                                      "sd.b","sd.y","mu.pious","mu.news",
                                      "mu.age","mu.male","mu.ses","mu.ed2","mu.ed3","mu.satis"),
                 model.file=model.file,
                 n.chains=3,
                 n.iter=500,
                 debug=TRUE)

# Model estimates
round(bugs.model$mean$mu.pious,2)
round(bugs.model$sd$mu.pious,2)

round(bugs.model$mean$mu.news,2)
round(bugs.model$sd$mu.news,2)

round(bugs.model$mean$mu.age,2)
round(bugs.model$sd$mu.age,2)

round(bugs.model$mean$mu.male,2)
round(bugs.model$sd$mu.male,2)

round(bugs.model$mean$mu.ses,2)
round(bugs.model$sd$mu.ses,2)

round(bugs.model$mean$mu.ed2,2)
round(bugs.model$sd$mu.ed2,2)

round(bugs.model$mean$mu.ed3,2)
round(bugs.model$sd$mu.ed3,2)

round(bugs.model$mean$mu.satis,2)
round(bugs.model$sd$mu.satis,2)

cbind(round(bugs.model$mean$g.country,2),
      round(bugs.model$sd$g.country,2))

round(bugs.model$mean$sd.y,2)
round(bugs.model$mean$sd.b,2)

data.frame(cnames,bugs.model$mean$b.pious)[order(bugs.model$mean$b.pious),]
data.frame(cnames,bugs.model$mean$b.news)[order(bugs.model$mean$b.news),]

round(bugs.model$mean$mu.pious+bugs.model$mean$mu.news,2)


# plot results
windows(10,3.3)
par(mfrow=c(1,3),mar=c(4.5,4.5,2.5,1.5),las=1)
xax <- seq(0.1,0.75,0.05) # for struggle
xlab <- "Proportion seeing secular-Islamist struggle"
#xax <- seq(0.35,0.95,0.05)    # for pctrel
#xlab <- "Proportion highly religious"

# 10% Muslim, logGDP=7, vary struggle on x axis
xc <- cbind(1,xax,0.1,7)
plot(xax,xc %*% bugs.model$mean$g.country,ylim=c(0,1),type="l",lty=2,lwd=2,
        xlab=xlab,ylab="Level of Anti-Americanism",
        cex.lab=1.3,cex.main=1.6,xaxt="n",yaxt="n",main="Muslims 10% of population")
lines(xax,(xc %*% bugs.model$mean$g.country)+bugs.model$mean$mu.pious+bugs.model$mean$mu.news,lwd=2)
axis(1,seq(0,1,0.1),seq(0,1,0.1))
axis(2,seq(0,1,0.1),seq(0,1,0.1))

# 65% Muslim, logGDP=7, vary struggle on x axis
xc <- cbind(1,xax,0.65,7)
plot(xax,xc %*% bugs.model$mean$g.country,ylim=c(0,1),type="l",lty=2,lwd=2,
        xlab=xlab,ylab="Level of Anti-Americanism",
        cex.lab=1.3,cex.main=1.6,xaxt="n",yaxt="n",main="Muslims 65% of population")
lines(xax,(xc %*% bugs.model$mean$g.country)+bugs.model$mean$mu.pious+bugs.model$mean$mu.news,lwd=2)
axis(1,seq(0,1,0.1),seq(0,1,0.1))
axis(2,seq(0,1,0.1),seq(0,1,0.1))
text(.33,.74,"Highly religious,\nhigh media-awareness",cex=1.2)
text(.5,.32,"Not highly religious,\nlow media-awareness",cex=1.2)
#text(.7,.74,"Highly religious,\nhigh media-awareness",cex=1.2) # for pctrel
#text(.55,.32,"Not highly religious,\nlow media-awareness",cex=1.2)


# 100% Muslim,logGDP=7, vary struggle on x axis
xc <- cbind(1,xax,1,7)
plot(xax,xc %*% bugs.model$mean$g.country,ylim=c(0,1),type="l",lty=2,lwd=2,
        xlab=xlab,ylab="Level of Anti-Americanism",
        cex.lab=1.3,cex.main=1.6,xaxt="n",yaxt="n",main="Muslims 100% of population")
lines(xax,(xc %*% bugs.model$mean$g.country)+bugs.model$mean$mu.pious+bugs.model$mean$mu.news,lwd=2)
axis(1,seq(0,1,0.1),seq(0,1,0.1))
axis(2,seq(0,1,0.1),seq(0,1,0.1))
# savePlot("yhat-strug",type="pdf")
# savePlot("yhat-pctrel",type="pdf")




## Models 7-11: Robustness checks with other z3's in the upper-level model.

inits <-  function(){ list(b.country=runif(ncountry,0.4,0.6),
                           b.pious=runif(ncountry,0.1,0.2),
                           b.news=runif(ncountry,0,0.1),
                           g.country=runif(4,-0.5,0.5),
                           sd.y=0.2,
                           sd.b=rep(0.05,3),
                           p.pious=0.6,
                           p.news=0.6)
                    }

model.file <- "AA-GAP-hlm.txt"

z1 <- cvars$struggle                # Country: percent seeing a reformer-Islamist struggle
z2 <- cvars$pctmus                  # Country: percent Muslim in country
z3.list <- list()
z3.list[[1]] <- cvars$log.gdppc     # Country: log GDP per capita
z3.list[[2]] <- cvars$log.usimppc   # Country: Imports from US per capita, log
z3.list[[3]] <- cvars$usaid/1000    # Country: US foreign economic aid (billions)
z3.list[[4]] <- cvars$m2j/1000      # Country: miles from country capital to Jerusalem (1000s)
z3.list[[5]] <- cvars$milexp        # Country: Military expenditure
z3.list[[6]] <- cvars$fh.pr         # Country: Political rights
names(z3.list) <- c("log.gdppc","log.usimppc","usaid","m2j","milexp","fh.pr")

bugs.res <- list()
for (i in 1:length(z3.list)) {  
    z3 <- z3.list[[i]]
    bugs.model <- bugs(data=c("y","pious","news","z1","z2","z3","cnum","N","ncountry"),
                       inits=inits,
                       parameters.to.save=c("g.country","b.country","b.pious","b.news",
                                            "mu.country","mu.pious","mu.news","sd.b","sd.y"),
                       model.file=model.file,
                       n.chains=3,
                       n.iter=500,
                       debug=FALSE)

    bugs.res[[i]] <- bugs.model$summary
}

names(bugs.res) <- names(z3.list)

gcountry <- NULL
sdyb <- NULL
mupious <- NULL
munews <- NULL
for (i in 1:length(bugs.res)) {
    gcountry <- cbind(gcountry,round(bugs.res[[i]][1:4,1:2],2))
    sdyb <- cbind(sdyb,round(bugs.res[[i]][c(94,91,92,93),1],2))
    mupious <- cbind(mupious,round(bugs.res[[i]][89,1:2],2))
    munews <- cbind(munews,round(bugs.res[[i]][90,1:2],2))
}

# column 1 is Model 1 from Table 1; columns 2-6 are Table 2.
mupious
munews
gcountry
sdyb





# end of file.
