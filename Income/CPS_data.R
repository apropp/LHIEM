rm(list=ls())
setwd("/Users/apropp/Documents/10Plan/cuban10plan/NoteBooks/Income")
library(Hmisc)
library(expm)
#year.list <- c(1976,1979,1980,1981,1989,1990,1991,2000,2001,2002,2007,2008,2009,2017,2018)
year.list <- c(2019)
yr <- 2019

ipums.load <- TRUE
#ipums.load <- FALSE
if (ipums.load) {
  library(ipumsr)
  ddi <- read_ipums_ddi("cps_00007.xml")
  data <- read_ipums_micro(ddi)
  rm("ddi")
  #save(data,file="../Integration/R_Objects/10Plan_CPSv2.RData")
}

load(here("Data/10Plan_CPS_recent.RData"))
load(here("NoteBooks/Integration/R_Objects/10Plan_CPSv2.RData"))
# if income above 200% FPL - put on ESI if income over $1,000,000
  # Maybe between 2 & 400 FPL exchange, all else on ESI
# Set 1099999 to zero
#Clean data
#Set missing to zero
# Note these cleaning steps
# if under 100% FPL & on nongroup private - put on uninsured
data$INCWAGE[data$INCWAGE == 9999999 | data$INCWAGE == 1099999] <- 0
data$INCBUS[data$INCBUS == 9999999 | data$INCBUS == 1099999] <- 0
data$INCFARM[data$INCFARM == 9999999 | data$INCFARM == 1099999] <- 0
data$INCINT[!is.na(data$INCINT) & data$INCINT > 99999] <- 0
data$INCDIVID[!is.na(data$INCDIVID) & data$INCDIVID == 999999] <- 0
data$INCRENT[!is.na(data$INCRENT) & data$INCRENT > 999990] <- 0
data$INCINT[is.na(data$INCINT)] <- 0
data$INCDIVID[is.na(data$INCDIVID)] <- 0
data$INCRENT[is.na(data$INCRENT)] <- 0

#Load transition data
#tmat <- read.csv(file="Q_transition_matrix.csv",header=FALSE,sep=",")
tmat.input <- as.matrix(read.csv(file="NoteBooks/Income/Q_trans.csv",header=FALSE,sep=","))
tmat.temp <- sqrtm(sqrtm(sqrtm(tmat.input)))

tmat <- matrix(0,nrow = 5, ncol = 6)
tmat[,1] <- 0
for(i in 1:5) {
  tmat[,i+1] <- tmat[,i] + tmat.temp[,i]
}
tmat[,6] <- 1
tmat <- as.data.frame(tmat)
names(tmat) <- c("V1","V2","V3","V4","V5","V6")

data$Immig <- 0
data$Immig[data$CITIZEN==4 | data$CITIZEN==5] <- 1

#Make family unique family identifiers for the tax units 
data$taxid <- 0
#Head of Household
data$taxid[data$RELATE == 101] <- 100*data$SERIAL[data$RELATE == 101] + 1

#HH Spouse
data$taxid[data$RELATE == 201] <- 100*data$SERIAL[data$RELATE == 201] + 1
data$taxid[data$RELATE == 202] <- 100*data$SERIAL[data$RELATE == 202] + 1
data$taxid[data$RELATE == 203] <- 100*data$SERIAL[data$RELATE == 203] + 1

#Minor Children
data$taxid[data$RELATE == 301 & data$AGE < 18] <- 100*data$SERIAL[data$RELATE == 301 & data$AGE < 18] + 1

#Adult Child of HH
data$taxid[data$RELATE == 301 & data$AGE >= 18] <- 100*data$SERIAL[data$RELATE == 301 & data$AGE >= 18] + 
  data$PERNUM[data$RELATE == 301 & data$AGE >= 18]

#Other
data$taxid[data$RELATE > 304] <- 100*data$SERIAL[data$RELATE > 304] + data$PERNUM[data$RELATE > 304]

#Reassignment to account for multiple married couples in a single household
data$taxid[data$RELATE > 300 & data$SPLOC > 2] <- 100*data$SERIAL[data$RELATE > 300 & data$SPLOC > 2] + 
  pmin.int(data$PERNUM[data$RELATE > 300 & data$SPLOC > 2],data$SPLOC[data$RELATE > 300 & data$SPLOC > 2])

### Add children to non-heads of household family subgroups

data$tinc <- data$INCWAGE + data$INCBUS + data$INCFARM + 
  data$INCINT + data$INCDIVID + data$INCRENT

#Building family objects with taxable income, labor income, and weight
#Core variables
fam.names <- c("taxid","w","taxable","labor","wage","size","hw","state","wagetc","bustc","farmtc",
               "white","black","API","AI","multiple","other","combine.other",
               "WM","WF","BM","BF","APIM","APIF","AIM","AIF","MultM","MultF","OthM","OthF","COthM","COthF",
               "LTHS","HS","SCOL","COL","empty",
               "young","prime","older","elderly",
               "male","female",
               "geo.unk","rural","suburban","urban",
               "hispanic")
fam <- as.data.frame(matrix(data = NA,nrow = length(unique(data$taxid[data$YEAR == yr])),ncol = length(fam.names)))
names(fam) <- fam.names
fam$taxid <- sapply(split(data[data$YEAR == yr,"taxid"],data$taxid[data$YEAR == yr]),function(z) unique(z))    # Unique taxid
fam$taxable <- sapply(split(data$tinc[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))     # Taxable income
fam$w <- sapply(split(data$ASECWT[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))         # Family weight
fam$size <- sapply(split(data[data$YEAR == yr,"taxid"],data$taxid[data$YEAR == yr]),function(z) length(z))  # Family size

fam <- fam[order(fam$taxable),]
fam$cw <- 0
fam$cw <- fam$w/sum(fam$w)
fam$cw <- cumsum(fam$cw)

n <- nrow(fam)
fam$q0 <- 1
fam$q0[fam$cw > .2 & fam$cw <= .4] <- 2
fam$q0[fam$cw > .4 & fam$cw <= .6] <- 3
fam$q0[fam$cw > .6 & fam$cw <= .8] <- 4
fam$q0[fam$cw > .8 & fam$cw <= 1] <- 5

q2thresh <- fam$taxable[min(which(fam$q0==2))]
q3thresh <- fam$taxable[min(which(fam$q0==3))]
q4thresh <- fam$taxable[min(which(fam$q0==4))]
q5thresh <- fam$taxable[min(which(fam$q0==5))]
save(q2thresh,q3thresh,q4thresh,q5thresh,file=here("NoteBooks/Integration/R_Objects/income_thresholds.Rda"))

r <- 1.02
set.seed(1234)
fam$taxable0 <- fam$taxable
for (i in 1:16) {
  quint <- paste("q",i,sep ="")
  fam$oquint <- fam[,paste("q",i-1,sep ="")]
  fam$random <- runif(n)
  fam[,quint] <- 0
  for (j in 1:5) {
    fam[fam$random >= tmat[j,"V1"] & fam$random <= tmat[j,"V2"] & fam$oquint == j,quint] <- 1
    fam[fam$random >= tmat[j,"V2"] & fam$random <= tmat[j,"V3"] & fam$oquint == j,quint] <- 2
    fam[fam$random >= tmat[j,"V3"] & fam$random <= tmat[j,"V4"] & fam$oquint == j,quint] <- 3
    fam[fam$random >= tmat[j,"V4"] & fam$random <= tmat[j,"V5"] & fam$oquint == j,quint] <- 4
    fam[fam$random >= tmat[j,"V5"] & fam$random <= tmat[j,"V6"] & fam$oquint == j,quint] <- 5
  }
  txyr <- paste("taxable",i,sep ="")
  ptxyr <- paste("taxable",i-1,sep ="")
  fam[fam[,quint] == fam$oquint,txyr] <- fam[fam[,quint] == fam$oquint,ptxyr]*r
  for (j in 1:5) {
    fam[fam[,quint] == j,txyr] <- sample(fam[fam$oquint == j,ptxyr],size = length(fam[fam[,quint] == j,txyr]),replace=TRUE)*r
  }
  fam[fam[,quint] == fam$oquint,txyr] <- fam[fam[,quint] == fam$oquint,ptxyr]*r
}

FamIncome <- fam[,c("taxid","taxable")]
FamIncome$taxid <- unlist(FamIncome$taxid,use.names=FALSE)
data.f <- data %>%
  left_join(FamIncome, by="taxid")

save(fam,file="NoteBooks/Integration/R_Objects/10Plan_CPS_fam.RData")
save(data.f,file="NoteBooks/Integration/R_Objects/10Plan_CPSv2.RData")



##################
## 26 Year Olds ##
##################

twentysix <- data$tinc[data$AGE==26]
dat.twentysix <- data[data$AGE==26,]
dat.twentysix$taxid <- 1:nrow(dat.twentysix)
# IDEA: make a separate trajectory for 26-year olds in their own taxid

#Building family objects with taxable income, labor income, and weight
#Core variables
fam26.names <- c("taxid","w","taxable","labor","wage","size","hw","state","wagetc","bustc","farmtc",
               "white","black","API","AI","multiple","other","combine.other",
               "WM","WF","BM","BF","APIM","APIF","AIM","AIF","MultM","MultF","OthM","OthF","COthM","COthF",
               "LTHS","HS","SCOL","COL","empty",
               "young","prime","older","elderly",
               "male","female",
               "geo.unk","rural","suburban","urban",
               "hispanic")
fam26 <- as.data.frame(matrix(data = NA,nrow = length(unique(dat.twentysix$taxid[dat.twentysix$YEAR == yr])),ncol = length(fam26.names)))
names(fam26) <- fam26.names
fam26$taxid <- sapply(split(dat.twentysix[dat.twentysix$YEAR == yr,"taxid"],dat.twentysix$taxid[dat.twentysix$YEAR == yr]),function(z) unique(z))    # Unique taxid
fam26$taxable <- sapply(split(dat.twentysix$tinc[dat.twentysix$YEAR == yr],dat.twentysix$taxid[dat.twentysix$YEAR == yr]),function(z) sum(z))     # Taxable income
fam26$w <- sapply(split(dat.twentysix$ASECWT[dat.twentysix$YEAR == yr],dat.twentysix$taxid[dat.twentysix$YEAR == yr]),function(z) sum(z))         # Family weight
fam26$size <- sapply(split(dat.twentysix[dat.twentysix$YEAR == yr,"taxid"],dat.twentysix$taxid[dat.twentysix$YEAR == yr]),function(z) length(z))  # Family size

fam26 <- fam26[order(fam26$taxable),]
fam26$cw <- 0
fam26$cw <- fam26$w/sum(fam26$w)
fam26$cw <- cumsum(fam26$cw)

n <- nrow(fam26)
fam26$q0 <- 1
fam26$q0[fam26$cw > .2 & fam26$cw <= .4] <- 2
fam26$q0[fam26$cw > .4 & fam26$cw <= .6] <- 3
fam26$q0[fam26$cw > .6 & fam26$cw <= .8] <- 4
fam26$q0[fam26$cw > .8 & fam26$cw <= 1] <- 5

r <- 1.02
set.seed(1234)
fam26$taxable0 <- fam26$taxable
for (i in 1:16) {
  quint <- paste("q",i,sep ="")
  fam26$oquint <- fam26[,paste("q",i-1,sep ="")]
  fam26$random <- runif(n)
  fam26[,quint] <- 0
  for (j in 1:5) {
    fam26[fam26$random >= tmat[j,"V1"] & fam26$random <= tmat[j,"V2"] & fam26$oquint == j,quint] <- 1
    fam26[fam26$random >= tmat[j,"V2"] & fam26$random <= tmat[j,"V3"] & fam26$oquint == j,quint] <- 2
    fam26[fam26$random >= tmat[j,"V3"] & fam26$random <= tmat[j,"V4"] & fam26$oquint == j,quint] <- 3
    fam26[fam26$random >= tmat[j,"V4"] & fam26$random <= tmat[j,"V5"] & fam26$oquint == j,quint] <- 4
    fam26[fam26$random >= tmat[j,"V5"] & fam26$random <= tmat[j,"V6"] & fam26$oquint == j,quint] <- 5
  }
  txyr <- paste("taxable",i,sep ="")
  ptxyr <- paste("taxable",i-1,sep ="")
  fam26[fam26[,quint] == fam26$oquint,txyr] <- fam26[fam26[,quint] == fam26$oquint,ptxyr]*r
  for (j in 1:5) {
    fam26[fam26[,quint] == j,txyr] <- sample(fam26[fam26$oquint == j,ptxyr],size = length(fam26[fam26[,quint] == j,txyr]),replace=TRUE)*r
  }
  fam26[fam26[,quint] == fam26$oquint,txyr] <- fam26[fam26[,quint] == fam26$oquint,ptxyr]*r
}

FamIncome <- fam26[,c("taxid","taxable")]
FamIncome$taxid <- unlist(FamIncome$taxid,use.names=FALSE)
dat.f <- dat.twentysix %>%
  left_join(FamIncome, by="taxid")

save(fam26,file="../Integration/R_Objects/10Plan_26_income.RData")


################
## Immigrants ##
################

immigrants <- data.f$tinc[data.f$Immig==1]
dat.immig <- data.f[data.f$Immig==1,]
#dat.immig$taxid <- 1:nrow(dat.immig)
# IDEA: make a separate trajectory for 26-year olds in their own taxid

#Building family objects with taxable income, labor income, and weight
#Core variables
famImmig.names <- c("taxid","w","taxable","labor","wage","size","hw","state","wagetc","bustc","farmtc",
                 "white","black","API","AI","multiple","other","combine.other",
                 "WM","WF","BM","BF","APIM","APIF","AIM","AIF","MultM","MultF","OthM","OthF","COthM","COthF",
                 "LTHS","HS","SCOL","COL","empty",
                 "young","prime","older","elderly",
                 "male","female",
                 "geo.unk","rural","suburban","urban",
                 "hispanic")
famImmig <- as.data.frame(matrix(data = NA,nrow = length(unique(dat.immig$taxid[dat.immig$YEAR == yr])),ncol = length(famImmig.names)))
names(famImmig) <- famImmig.names
famImmig$taxid <- sapply(split(dat.immig[dat.immig$YEAR == yr,"taxid"],dat.immig$taxid[dat.immig$YEAR == yr]),function(z) unique(z))    # Unique taxid
famImmig$taxable <- sapply(split(dat.immig$tinc[dat.immig$YEAR == yr],dat.immig$taxid[dat.immig$YEAR == yr]),function(z) sum(z))     # Taxable income
famImmig$w <- sapply(split(dat.immig$ASECWT[dat.immig$YEAR == yr],dat.immig$taxid[dat.immig$YEAR == yr]),function(z) sum(z))         # Family weight
famImmig$size <- sapply(split(dat.immig[dat.immig$YEAR == yr,"taxid"],dat.immig$taxid[dat.immig$YEAR == yr]),function(z) length(z))  # Family size

famImmig <- famImmig[order(famImmig$taxable),]
famImmig$cw <- 0
famImmig$cw <- famImmig$w/sum(famImmig$w)
famImmig$cw <- cumsum(famImmig$cw)

n <- nrow(famImmig)
famImmig$q0 <- 1
famImmig$q0[famImmig$cw > .2 & famImmig$cw <= .4] <- 2
famImmig$q0[famImmig$cw > .4 & famImmig$cw <= .6] <- 3
famImmig$q0[famImmig$cw > .6 & famImmig$cw <= .8] <- 4
famImmig$q0[famImmig$cw > .8 & famImmig$cw <= 1] <- 5

r <- 1.02
set.seed(1234)
famImmig$taxable0 <- famImmig$taxable
for (i in 1:16) {
  quint <- paste("q",i,sep ="")
  famImmig$oquint <- famImmig[,paste("q",i-1,sep ="")]
  famImmig$random <- runif(n)
  famImmig[,quint] <- 0
  for (j in 1:5) {
    famImmig[famImmig$random >= tmat[j,"V1"] & famImmig$random <= tmat[j,"V2"] & famImmig$oquint == j,quint] <- 1
    famImmig[famImmig$random >= tmat[j,"V2"] & famImmig$random <= tmat[j,"V3"] & famImmig$oquint == j,quint] <- 2
    famImmig[famImmig$random >= tmat[j,"V3"] & famImmig$random <= tmat[j,"V4"] & famImmig$oquint == j,quint] <- 3
    famImmig[famImmig$random >= tmat[j,"V4"] & famImmig$random <= tmat[j,"V5"] & famImmig$oquint == j,quint] <- 4
    famImmig[famImmig$random >= tmat[j,"V5"] & famImmig$random <= tmat[j,"V6"] & famImmig$oquint == j,quint] <- 5
  }
  txyr <- paste("taxable",i,sep ="")
  ptxyr <- paste("taxable",i-1,sep ="")
  famImmig[famImmig[,quint] == famImmig$oquint,txyr] <- famImmig[famImmig[,quint] == famImmig$oquint,ptxyr]*r
  for (j in 1:5) {
    famImmig[famImmig[,quint] == j,txyr] <- sample(famImmig[famImmig$oquint == j,ptxyr],size = length(famImmig[famImmig[,quint] == j,txyr]),replace=TRUE)*r
  }
  famImmig[famImmig[,quint] == famImmig$oquint,txyr] <- famImmig[famImmig[,quint] == famImmig$oquint,ptxyr]*r
}

FamIncome <- famImmig[,c("taxid","taxable")]
FamIncome$taxid <- unlist(FamIncome$taxid,use.names=FALSE)
dat.f <- dat.immig %>%
  left_join(FamIncome, by="taxid")

save(famImmig,file="../Integration/R_Objects/10Plan_immig_income.RData")


