rm(list=ls())
setwd("~/Dropbox/2018 Summer/PS 50/Lectures/L11 Authoritarianism/Data")
load("Freedom House.RData")

res$year = as.character(res$year)
res$year = as.numeric(res$year)

##### History #####
sel = c(1975, 1980, 1990, 1995, 2000, 2015)
fh = res[grep(paste(sel, collapse="|"), res$year),]

par(mfrow=c(1,2))

### Press Freedom
fh$PR_type = "Partly Free"
fh$PR_type[fh$PR <= 2.5] = "Free"
fh$PR_type[fh$PR >= 5.5] = "Not Free"
fh$PR_type[is.na(fh$PR) == T] = NA
table(fh$PR_type)

fh$PR_type = as.factor(fh$PR_type)
fh$PR_type = factor(fh$PR_type, levels(fh$PR_type)[c(1,3,2)])
levels(fh$PR_type)

counts = table(fh$PR_type, fh$year); counts
barplot(counts, main="Press Freedom in the World", ylim=c(0,250), col=c("darkgreen","gold", "blue"))
legend("topleft", inset=.02, rownames(counts), fill=c("darkgreen","gold", "blue"), horiz=F, cex=0.8)

### Civil liberties
fh$CL_type = "Partly Free"
fh$CL_type[fh$CL <= 2.5] = "Free"
fh$CL_type[fh$CL >= 5.5] = "Not Free"
fh$CL_type[is.na(fh$CL) == T] = NA
table(fh$CL_type)

fh$CL_type = as.factor(fh$CL_type)
fh$CL_type = factor(fh$CL_type, levels(fh$CL_type)[c(1,3,2)])
levels(fh$CL_type)

counts = table(fh$CL_type, fh$year); counts
barplot(counts, main="Civil Liberties in the World", ylim=c(0,250), col=c("darkgreen","gold", "blue"))
legend("topleft", inset=.02, rownames(counts), fill=c("darkgreen","gold", "blue"), horiz=F, cex=0.8)

par(mfrow=c(1,1))

##### US #####
us = res[res$country %in% "United States",]

par(mfrow=c(1,2))
plot(us$year, us$PR, xlab="Year", ylab="Press Freedom", type="l", ylim=c(1,7))
plot(us$year, us$CL, xlab="Year", ylab="Civil Liberties", type="l", ylim=c(1,7))

##### China #####
prc = res[res$country %in% "China",]

par(mfrow=c(1,2))
plot(prc$year, prc$PR, xlab="Year", ylab="Press Freedom", type="l", ylim=c(1,7))
plot(prc$year, prc$CL, xlab="Year", ylab="Civil Liberties", type="l", ylim=c(1,7))
