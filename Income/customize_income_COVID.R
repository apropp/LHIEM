rm(list=ls())
yr <- 2019
#load(here("NoteBooks/Integration/R_Objects/10Plan_CPS_fam_u65_COVID.RData"))
load(here("NoteBooks/Integration/R_Objects/10Plan_CPS_data_u65_COVID.RData"))

data <-  data.under65

##################
## 26 Year Olds ##
##################
inc.twentysix <- data$tinc[data$AGE==26]
dat.twentysix <- data[data$AGE==26,]
dat.twentysix$taxid <- 1:nrow(dat.twentysix)
fam26.names <- c("taxid","w","taxable","labor","wage","size","hw","state","wagetc","bustc","farmtc",
               "white","black","API","AI","multiple","other","combine.other",
               "WM","WF","BM","BF","APIM","APIF","AIM","AIF","MultM","MultF","OthM","OthF","COthM","COthF",
               "LTHS","HS","SCOL","COL","empty",
               "young","prime","older","elderly",
               "male","female",
               "geo.unk","rural","suburban","urban",
               "hispanic")
fam26 <- as.data.frame(matrix(data = NA,nrow = length(unique(data$taxid[data$YEAR == yr])),ncol = length(fam26.names)))
names(fam26) <- fam26.names
fam26$taxid <- sapply(split(data[data$YEAR == yr,"taxid"],data$taxid[data$YEAR == yr]),function(z) unique(z))
fam26$taxable <- sapply(split(data$tinc[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
fam26$w <- sapply(split(data$ASECWT[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
fam26$size <- sapply(split(data$taxid[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) length(z))
fam26$prime <- sapply(split(data$prime[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) max(z))
fam26$workers <- sapply(split(data$working[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
fam26$fw <- fam26$w/fam26$size
fam26$married <- sapply(split(data$married[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) max(z))
fam26$t0 <- fam26$taxable

for(t in 1:15) {
  t.var <- paste("t",t,sep="")
  fam26[,t.var] <- 0
  fam26[,t.var] <- sapply(split(data[data$YEAR == yr,t.var],data$taxid[data$YEAR == yr]),function(z) sum(z))
}

fam26 <- fam26[order(fam26$taxable),]
fam26$cw <- 0
fam26$cw <- fam26$w/sum(fam26$w)
fam26$cw <- cumsum(fam26$cw)

# Add TAXID and TAXABLE INCOME
Fam26Income <- fam26[,c("taxid","taxable")]
Fam26Income$taxid <- unlist(Fam26Income$taxid,use.names=FALSE)
data <- data %>%
  left_join(Fam26Income, by="taxid")

save(fam26,file=here("NoteBooks/Integration/R_Objects/10Plan_26_income_COVID.RData"))


################
## Immigrants ##
################
inc.immig <- data$tinc[data$Immig==1]
dat.immig <- data[data$Immig==1,]
dat.immig$taxid <- 1:nrow(dat.immig)

famImmig.names <- c("taxid","w","taxable","labor","wage","size","hw","state","wagetc","bustc","farmtc",
                 "white","black","API","AI","multiple","other","combine.other",
                 "WM","WF","BM","BF","APIM","APIF","AIM","AIF","MultM","MultF","OthM","OthF","COthM","COthF",
                 "LTHS","HS","SCOL","COL","empty",
                 "young","prime","older","elderly",
                 "male","female",
                 "geo.unk","rural","suburban","urban",
                 "hispanic")
famImmig <- as.data.frame(matrix(data = NA,nrow = length(unique(data$taxid[data$YEAR == yr])),ncol = length(famImmig.names)))
names(famImmig) <- famImmig.names
famImmig$taxid <- sapply(split(data[data$YEAR == yr,"taxid"],data$taxid[data$YEAR == yr]),function(z) unique(z))
famImmig$taxable <- sapply(split(data$tinc[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
famImmig$w <- sapply(split(data$ASECWT[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
famImmig$size <- sapply(split(data$taxid[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) length(z))
famImmig$prime <- sapply(split(data$prime[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) max(z))
famImmig$workers <- sapply(split(data$working[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) sum(z))
famImmig$fw <- famImmig$w/famImmig$size
famImmig$married <- sapply(split(data$married[data$YEAR == yr],data$taxid[data$YEAR == yr]),function(z) max(z))
famImmig$t0 <- famImmig$taxable

for(t in 1:15) {
  t.var <- paste("t",t,sep="")
  famImmig[,t.var] <- 0
  famImmig[,t.var] <- sapply(split(data[data$YEAR == yr,t.var],data$taxid[data$YEAR == yr]),function(z) sum(z))
}

famImmig <- famImmig[order(famImmig$taxable),]
famImmig$cw <- 0
famImmig$cw <- famImmig$w/sum(famImmig$w)
famImmig$cw <- cumsum(famImmig$cw)

# Add TAXID and TAXABLE INCOME
FamImmigIncome <- famImmig[,c("taxid","taxable")]
FamImmigIncome$taxid <- unlist(FamImmigIncome$taxid,use.names=FALSE)
data <- data %>%
  left_join(FamImmigIncome, by="taxid")

save(famImmig,file=here("NoteBooks/Integration/R_Objects/10Plan_immig_income_COVID.RData"))
