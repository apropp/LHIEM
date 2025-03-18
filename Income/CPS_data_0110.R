rm(list=ls())
set.seed(123)
library(Hmisc)
library(expm)
library(dplyr)
#year.list <- c(1976,1979,1980,1981,1989,1990,1991,2000,2001,2002,2007,2008,2009,2017,2018)
year.list <- c(2019)
yr <- 2019

#ipums.load <- TRUE
ipums.load <- FALSE
if (ipums.load) {
  library(ipumsr)
  ddi <- read_ipums_ddi("cps_00009.xml")
  data <- read_ipums_micro(ddi)
  rm("ddi")
  save(data,file="10Plan_CPSv3.RData")
} else {
  load(here("NoteBooks/Integration/R_Objects/10Plan_CPSv3.RData"))
}
data$Immig <- 0
data$Immig[data$CITIZEN==4 | data$CITIZEN==5] <- 1

#Clean data
#Set missing to zero
data$INCWAGE[data$INCWAGE == 9999999] <- 0
data$INCBUS[data$INCBUS == 9999999] <- 0
data$INCFARM[data$INCFARM == 9999999] <- 0
data$INCINT[!is.na(data$INCINT) & data$INCINT > 99999] <- 0
data$INCDIVID[!is.na(data$INCDIVID) & data$INCDIVID == 999999] <- 0
data$INCRENT[!is.na(data$INCRENT) & data$INCRENT > 999990] <- 0
data$INCINT[is.na(data$INCINT)] <- 0
data$INCDIVID[is.na(data$INCDIVID)] <- 0
data$INCRENT[is.na(data$INCRENT)] <- 0

#Make family unique family identifiers for the tax units 
data$taxid <- 0
#Head of Household
data$taxid[data$RELATE == 101] <- 100*data$SERIAL[data$RELATE == 101] + 1

#HH Spouse
data$taxid[data$RELATE == 201] <- 100*data$SERIAL[data$RELATE == 201] + 1
data$taxid[data$RELATE == 202] <- 100*data$SERIAL[data$RELATE == 202] + 1
data$taxid[data$RELATE == 203] <- 100*data$SERIAL[data$RELATE == 203] + 1
data$married <- 0
data$married[data$RELATE %in% c(201,202,203)] <- 1

#Minor Children
data$taxid[data$RELATE == 301 & data$AGE < 18] <- 100*data$SERIAL[data$RELATE == 301 & data$AGE < 18] + 1

#Adult Child of HH
data$taxid[data$RELATE == 301 & data$AGE >= 18] <- 100*data$SERIAL[data$RELATE == 301 & data$AGE >= 18] + 1

#Other
data$taxid[data$RELATE > 304] <- 100*data$SERIAL[data$RELATE > 304] + 1

#Reassignment to account for multiple married couples in a single household
data$taxid[data$RELATE > 300 & data$SPLOC > 2] <- 100*data$SERIAL[data$RELATE > 300 & data$SPLOC > 2] + 
  pmin.int(data$PERNUM[data$RELATE > 300 & data$SPLOC > 2],data$SPLOC[data$RELATE > 300 & data$SPLOC > 2])

### Add children to non-heads of household family subgroups

data$tinc <- data$INCWAGE + data$INCBUS + data$INCFARM + 
  data$INCINT + data$INCDIVID + data$INCRENT
data$t0 <- data$tinc

data$prime <- 0
data$prime[data$AGE >= 18 & data$AGE <= 65] <- 1

#make groups consistent with the PSID groupings
data <- data[order(data$tinc),]
data$rank <- 0
data$rank <- data$ASECWT/sum(data$ASECWT)
data$rank <- cumsum(data$rank)

data$inc.group <- 1
data$inc.group[data$rank > .2 & data$rank <= .4] <- 2
data$inc.group[data$rank > .4 & data$rank <= .6] <- 3
data$inc.group[data$rank > .6 & data$rank <= .8] <- 4
data$inc.group[data$rank > .8] <- 5

data$age.group <- 0
data$age.group[data$AGE >= 18 & data$AGE < 25] <- 1
data$age.group[data$AGE >= 25 & data$AGE < 35] <- 2
data$age.group[data$AGE >= 35 & data$AGE < 50] <- 3
data$age.group[data$AGE >= 50 & data$AGE < 65] <- 4

data$group <- 0
data$group[data$age.group != 0] <- data$SEX[data$age.group != 0] + 
  2*(data$age.group[data$age.group != 0] - 1) + 8*(data$inc.group[data$age.group != 0] - 1)

ng <- max(data$group)

data$working <- 0
data$working[data$tinc > 10] <- 1
data$w0 <- data$working

thresh <- -.7
load(file="10Plan_PSID.RData")
delta1 <- c(1,sum(delta.mat$w[delta.mat$group == 1 & delta.mat$delta < thresh])/sum(delta.mat$w[delta.mat$group == 1]))
for (g in 2:ng) {
  delta1 <- rbind(delta1,c(g,sum(delta.mat$w[delta.mat$group == g & delta.mat$delta < thresh])/sum(delta.mat$w[delta.mat$group == g])))
}
delta1 <- as.data.frame(delta1)
names(delta1) <- c("g","jloss")

thr1 <- .15
thr2 <- .15
scaler <- 1
delta2 <- sum(delta.mat$w[delta.mat$rank < thr1 & delta.mat$rank2 > thr2])/sum(delta.mat$w[delta.mat$rank < thr1])*scaler
delta3 <- cbind(delta.mat$inc[delta.mat$rank < thr1 & delta.mat$rank2 > thr2],delta.mat$w[delta.mat$rank < thr1 & delta.mat$rank2 > thr2])
delta3 <- as.data.frame(delta3)
names(delta3) <- c("inc","w")

delta5 <- cbind(delta.mat$inc[delta.mat$rank < thr1],delta.mat$w[delta.mat$rank < thr1])
delta5 <- as.data.frame(delta5)
names(delta5) <- c("inc","w")

r <- .021
cap <- .8
n <- nrow(data)
for (t in 1:15) {
  t.var.o <- paste("t",t-1,sep="")
  w.var.o <- paste("w",t-1,sep="")
  t.var <- paste("t",t,sep="")
  w.var <- paste("w",t,sep="")

  #initialize variables
  data[,t.var] <- 0
  data[,w.var] <- 0
  data[,t.var] <- data[,t.var.o]*(1 + r)
  data[,w.var] <- data[,w.var.o]
  
  #set random numbers to determine work status
  data$wrand <- runif(n)
  for (g in 1:ng) {
    
    #building appropriate transition matrices
    delta4 <- cbind(delta.mat$delta[delta.mat$group==g],delta.mat$w[delta.mat$group==g])
    delta4 <- as.data.frame(delta4)
    names(delta4) <- c("delta","w")
    delta4[delta4$delta > thresh,] <- delta4[delta4$delta > thresh,]
#    delta4[delta4$delta > cap,"delta"] <- cap
    delta4$delta <- 1 + delta4$delta + r
    bottomd <- quantile(delta4$delta,probs = .4)
    topd <- quantile(delta4$delta,probs = .6)
    
    #delta1$jloss <- delta1$jloss*0
    #delta2 <- delta2*0
    #delta3$inc <- delta3$inc*0
    #delta4$delta <- delta4$delta*0 + 1
    #delta5$inc <- delta5$inc*0
    #Lose employment
    tlen <- nrow(data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand < delta1[delta1$g == g,"jloss"],])
#    print(sum(data$ASECWT[data$AGE %in% c(25:64) & data[,w.var.o]==1 & data$wrand < delta1[delta1$g == g,"jloss"]])/
#            sum(data$ASECWT[data$AGE %in% c(25:64) & data[,w.var.o]==1]))
    data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand < delta1[delta1$g == g,"jloss"],t.var] <- sample(delta5$inc,tlen,prob=delta5$w,replace = TRUE)*(1+r)^g
    data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand < delta1[delta1$g == g,"jloss"],w.var] <- 0
    
    #Gain employment
    data[data$AGE %in% c(18:64) & data[,w.var.o]==0 & data$wrand < delta2,w.var] <- 1
    tlen <- nrow(data[data$AGE %in% c(18:64) & data[,w.var.o]==0 & data$wrand < delta2,])
    data[data$AGE %in% c(18:64) & data[,w.var.o]==0 & 
           data$wrand < delta2,t.var] <- sample(delta3$inc,tlen,prob=delta3$w,replace = TRUE)*(1+r)^g
    
    #Update general population
    data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand >= delta1[delta1$g == g,"jloss"],w.var] <- 1
    tlen <- nrow(data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand >= delta1[delta1$g == g,"jloss"],])
#    data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand >= delta1[delta1$g == g,"jloss"],t.var] <- sample(delta4$delta,tlen,prob=delta4$w,replace = TRUE)*
#      data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand >= delta1[delta1$g == g,"jloss"],t.var.o]
    data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand >= delta1[delta1$g == g,"jloss"],t.var] <- runif(tlen,min = bottomd,max = topd)*
      data[data$AGE %in% c(18:64) & data[,w.var.o]==1 & data$wrand >= delta1[delta1$g == g,"jloss"],t.var.o]
  }
}

summary(data$t0)
summary(data$t1)
summary(data$t2)
summary(data$t5)
summary(data$t10)
summary(data$t15)

summary(data$w0)
summary(data$w5)
summary(data$w10)
summary(data$w15)

# Building family objects with taxable income, labor income, and weight
# Core variables
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
fam$taxid <- sapply(split(data[data$YEAR == yr,"taxid"],data$taxid[data$YEAR == yr]),function(z) unique(z))
fam$taxable <- sapply(split(data$tinc[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
fam$w <- sapply(split(data$ASECWT[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
fam$size <- sapply(split(data$taxid[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) length(z))
fam$prime <- sapply(split(data$prime[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) max(z))
fam$workers <- sapply(split(data$working[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
fam$fw <- fam$w/fam$size
fam$married <- sapply(split(data$married[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) max(z))

fam$t0 <- fam$taxable
for(t in 1:15) {
  t.var <- paste("t",t,sep="")
  fam[,t.var] <- 0
  fam[,t.var] <- sapply(split(data[data$YEAR == yr,t.var],data$taxid[data$YEAR == yr]),function(z) sum(z))
}

### This concludes the program

fam <- fam[order(fam$taxable),]
fam$cw <- 0
fam$cw <- fam$w/sum(fam$w)
fam$cw <- cumsum(fam$cw)


# Add TAXID and TAXABLE INCOME
FamIncome <- fam[,c("taxid","taxable")]
FamIncome$taxid <- unlist(FamIncome$taxid,use.names=FALSE)
data <- data %>%
  left_join(FamIncome, by="taxid")

min.ages <- data %>%
  group_by(taxid) %>%
  summarise(min.age=min(AGE))
elderly.fams <- min.ages$taxid[min.ages$min.age>64]
fam.under65 <- fam[! fam$taxid %in% elderly.fams,]
data.under65 <- data[data$AGE<65,]

save(fam,file=here("NoteBooks/Integration/R_Objects/10Plan_CPS_fam_0110.RData"))
save(data,file=here("NoteBooks/Integration/R_Objects/10Plan_CPS_data_0110.RData"))
save(fam.under65,file=here("NoteBooks/Integration/R_Objects/10Plan_CPS_fam_u65_0110.RData"))
save(data.under65, file=here("NoteBooks/Integration/R_Objects/10Plan_CPS_data_u65_0110.RData"))
