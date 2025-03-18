#### A script written by Adrienne Propp in Oct 2019
#### This script cleans our main data file (from the CPS)
#### and contains functions that will help us calculate income.
#### We get variable codes from https://cps.ipums.org/cps/index.shtml

# Load CPS data
# path <- getwd()
# load(file.path(path,"R_Objects/CART.Rda"))
# load(file.path(path,"R_Objects/10Plan_CPS_fam.RData"))
load(here("NoteBooks/Integration/R_Objects/transitions.Rda"))
#load(here("NoteBooks/Integration/R_Objects/10Plan_CPS_fam.RData"))
load(here("NoteBooks/Integration/R_Objects/10Plan_CPS_fam_0110.RData")) # New version
load(here("NoteBooks/Integration/R_Objects/10Plan_26_income_0110.RData"))
load(here("NoteBooks/Integration/R_Objects/income_thresholds.Rda"))
load(here("NoteBooks/Integration/R_Objects/10Plan_immig_income_0110.RData"))

# These are the federal poverty guidelines by size of household
FPL.by.size <- c(12490, 16910, 21330, 25750, 30170, 34590, 39010, 43430) # https://aspe.hhs.gov/2019-poverty-guidelines add $4420 for each additional member after 8
# Grows at rate of 2% per year - page 31 of https://www.cbo.gov/system/files/2019-08/55551-CBO-outlook-update_0.pdf 
fg <- 1.02

#######################
### Calculate % FPL ###
#######################
# This function calculates % FPL based on household income and size
calc.FPL <- function(income, fam.size, year){
  bin <- FPL.by.size[min(fam.size,8)]*fg^(year)
  bin <- bin + max(0, fam.size-8)*4420
  fpl <- (income/bin)*100
  return(fpl)
}

# This function basically does the same thing as calc.FPL (above) but takes a full population as its input
calc.FPL.for.pop <- function(pop, year){
  Units <- pop %>%
    dplyr::group_by(TID) %>%
    dplyr::summarise(FamIncome=mean(FamIncome), N=n())
  bin <- pmin(unlist(Units[,"N"],use.names=FALSE),8)
  bin <- sapply(bin, function(x) FPL.by.size[x]*fg^(year))
  bin <- bin + pmax(unlist(Units[,"N"],use.names=FALSE)-8,0)*4420
  Units[,"FPL.new"] <- (Units[,"FamIncome"]/bin)*100
  joined <- left_join(pop,Units,by="TID")
  return(joined$FPL.new)
}


####################################################################
### UNCOMMENT EVERYTHING BELOW IF YOU NEED TO REMAKE CPS DATASET ###
### This allows me to generate the variables I need from the     ###
### dataset that results from Carter's code.                     ###
####################################################################
# load(here("NoteBooks/Integration/R_Objects/10Plan_CPS_data_u65_0110.RData")) # New version
# 
# load(here("NoteBooks/Integration/R_Objects/CART.Rda"))
# cps <- data.under65
# cps <- na.omit(cps)
# 
# ######################################
# ### Set up our predictor variables ###
# ######################################
# 
# # Age
# #cps$AgeGroup[cps$AGE>=65]=5; cps$AgeGroup[cps$AGE<65 & cps$AGE>=50]=4; cps$AgeGroup[cps$AGE<50 & cps$AGE>=35]=3; cps$AgeGroup[cps$AGE<35 & cps$AGE>=19]=2; cps$AgeGroup[cps$AGE<19]=1
# cps$AgeGroup[cps$AGE<65 & cps$AGE>=50]=4; cps$AgeGroup[cps$AGE<50 & cps$AGE>=35]=3; cps$AgeGroup[cps$AGE<35 & cps$AGE>=19]=2; cps$AgeGroup[cps$AGE<19]=1
# cps$AgeGroup <- factor(cps$AgeGroup, labels=c("<19","19-34","35-49","50-64"))
# 
# # Income
# #cps$Income <- cps$INCTOT
# cps$FamIncome <- cps$taxable
# cps$WageProp <- cps$INCWAGE/cps$FamIncome
# cps$WageProp[is.na(cps$WageProp)] <- 0
# 
# # FPL
# cps$TID <- cps$taxid
# cps$FPL <- calc.FPL.for.pop(cps,year=1)
# 
# # Race
# cps$Race <- 4 # Other
# cps$Race[cps$RACE==100 | cps$RACE==802 | cps$RACE==803 | cps$RACE==804] <- 2 # White
# cps$Race[cps$RACE==200 | cps$RACE==801 | cps$RACE==805 | cps$RACE==806 | cps$RACE==807 | cps$RACE==810 | cps$RACE==811 | cps$RACE==814 | cps$RACE==816] <- 3 # Black
# cps$Race[cps$HISPAN!= 0 & cps$HISPAN!= 901 & cps$HISPAN!= 902] <- 1 # Hispanic
# #000 - not hispanic
# #901 & 902 not sure
# cps$Race <- factor(cps$Race, labels=c("hispanic","white","black", "other"))
# 
# # Sex
# cps$Sex <- factor(cps$SEX, labels=c("male","female"))
# 
# # Insurance Category
# cps$InsCat2 <- 6
# cps$InsCat2[cps$ANYCOVLY==1] <- 4 # Uninsured
# cps$InsCat2[cps$GRPCOVLY==2] <- 5 #ESI
# cps$InsCat2[cps$MRKSCOVLY==2 | cps$MRKCOVLY==2 | cps$NMCOVLY==2] <- 1 # Nongroup - this + remaining uninsured is our target population!
# cps$InsCat2[cps$PUBPART==2 | cps$TRCCOVLY==2 | cps$CHAMPVALY==2 | cps$INHCOVLY==2 | cps$PUBCOVLY==2 | cps$HICHAMP==2 | cps$HIMCARE==2] <- 2 # Public or other group programs
# cps$InsCat2[cps$HIMCAID==2] <- 3 # Medicaid
# 
# # Insurance Category corrections
# #cps$InsCat2[cps$InsCat2==1 & cps$FPL<=100] <- 4 # Correct anyone at or below 100% FPL on exchange to be uninsured
# #cps$InsCat2[cps$InsCat2==3 & cps$FPL>200 & cps$FPL<400] <- 1 # Correct anyone on Medicaid between 200-400 FPL to be on the exchange
# #cps$InsCat2[cps$InsCat2==3 & cps$FPL>=400] <- 1 # Correct anyone on Medicaid at or over 400 FPL to be on group private (ESI)
# 
# # Insurance Category corrections for unassigned minors
# newborn.fams <- cps$taxid[which(cps$InsCat2==6)]
# #table(cps$InsCat2[cps$taxid %in% newborn.fams])
# taxids.w.dep.on.ESI <- (cps$taxid[(cps$taxid %in% newborn.fams) & cps$GRPDEPLY==2])
# uniques <- cps$taxid[ave(cps$taxid, cps$taxid, FUN = length) == 1]
# cps$InsCat2[cps$InsCat2==6 & (cps$taxid %in% newborn.fams) & (cps$taxid %in% taxids.w.dep.on.ESI)] <- 5 # If family indicates dependents on ESI, assign ESI
# cps$InsCat2[cps$InsCat2==6 & cps$taxid %in% uniques] <- 3 # If no other family members, assign medicaid
# remaining.newborn.fams <- cps$taxid[cps$InsCat2==6]
# m <- subset(cps,cps$taxid %in% remaining.newborn.fams); mm <- m %>% dplyr::group_by(taxid) %>% dplyr::summarise(N=n(),Income=mean(FamIncome),FamCat=first(InsCat2))#; mm$FPL=calc.FPL(mm$Income,mm$N,year=1)
# cps <- cps %>% dplyr::left_join(mm[,c("taxid","FamCat")], by="taxid")
# cps$InsCat2[cps$InsCat2==6 & cps$FPL<200] <- 3
# cps$InsCat2 <- ifelse(cps$InsCat2==6, cps$FamCat, cps$InsCat2)
# cps$InsCat2[cps$InsCat2==6] <- 5 # Set the rest also to NG Private After evaluating the FPL of these families, only 7/123 are above 150% FPL, and the other 7 are still below 300%
# cps$InsCat <- factor(cps$InsCat2, labels=c("NonGroup Private","Other Public","Medicaid","Uninsured","Other Private"))
# 
# save(cps, file=here("NoteBooks/Integration/R_Objects/cps.Rda"))
###################
### END SECTION ###
###################

# Medical Spending - Sample from MEPS spending based on buckets for AgeGroup & InsCat
load(here("NoteBooks/Integration/R_Objects/cps.Rda"))

cps$MedSpend <- 0
cps$MedSpend[cps$AgeGroup=="<19" & cps$InsCat=="Other Public"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="<19" & dat$InsCat=="Other Public"], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="Other Public",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="Other Public"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="<19" & cps$InsCat=="Medicaid"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="<19" & dat$InsCat=="Medicaid"], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="Medicaid",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="Medicaid"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="<19" & cps$InsCat=="Other Private"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="<19" & dat$InsCat=="Other Private"], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="Other Private",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="Other Private"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="<19" & cps$InsCat=="Uninsured"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="<19" & (dat$InsCat=="Uninsured")], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="Uninsured",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="Uninsured"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="<19" & cps$InsCat=="NonGroup Private"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="<19" & (dat$InsCat=="NonGroup Private")], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="NonGroup Private"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="19-34" & cps$InsCat=="Other Public"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="19-34" & dat$InsCat=="Other Public"], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="Other Public",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="Other Public"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="19-34" & cps$InsCat=="Medicaid"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="19-34" & dat$InsCat=="Medicaid"], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="Medicaid",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="Medicaid"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="19-34" & cps$InsCat=="Other Private"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="19-34" & dat$InsCat=="Other Private"], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="Other Private",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="Other Private"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="19-34" & cps$InsCat=="Uninsured"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="19-34" & (dat$InsCat=="Uninsured")], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="Uninsured",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="Uninsured"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="19-34" & cps$InsCat=="NonGroup Private"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="19-34" & (dat$InsCat=="NonGroup Private" )], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="NonGroup Private"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="35-49" & cps$InsCat=="Other Public"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="35-49" & dat$InsCat=="Other Public"], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="Other Public",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="Other Public"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="35-49" & cps$InsCat=="Medicaid"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="35-49" & dat$InsCat=="Medicaid"], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="Medicaid",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="Medicaid"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="35-49" & cps$InsCat=="Other Private"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="35-49" & dat$InsCat=="Other Private"], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="Other Private",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="Other Private"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="35-49" & cps$InsCat=="Uninsured"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="35-49" & (dat$InsCat=="Uninsured")], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="Uninsured",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="Uninsured"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="35-49" & cps$InsCat=="NonGroup Private"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="35-49" & (dat$InsCat=="NonGroup Private")], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="NonGroup Private"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="50-64" & cps$InsCat=="Other Public"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="50-64" & dat$InsCat=="Other Public"], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="Other Public",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="Other Public"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="50-64" & cps$InsCat=="Medicaid"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="50-64" & dat$InsCat=="Medicaid"], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="Medicaid",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="Medicaid"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="50-64" & cps$InsCat=="Other Private"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="50-64" & dat$InsCat=="Other Private"], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="Other Private",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="Other Private"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="50-64" & cps$InsCat=="Uninsured"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="50-64" & (dat$InsCat=="Uninsured")], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="Uninsured",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="Uninsured"], replace=TRUE)
cps$MedSpend[cps$AgeGroup=="50-64" & cps$InsCat=="NonGroup Private"] <- sample(dat$TOTEXPY1[dat$AgeGroup=="50-64" & (dat$InsCat=="NonGroup Private")], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="NonGroup Private"], replace=TRUE)
#cps$MedSpend[cps$AgeGroup=="65+"] <- sample(dat$TOTEXPY1, size=nrow(cps[cps$AgeGroup=="65+",]), replace=TRUE)

cps$Deduct <- 0
cps$Deduct[cps$AgeGroup=="<19" & cps$InsCat=="NonGroup Private"] <- sample(dat$Deduct.x[dat$AgeGroup=="<19" & dat$InsCat=="NonGroup Private"], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="NonGroup Private"], replace=TRUE)
cps$Deduct[cps$AgeGroup=="19-34" & cps$InsCat=="NonGroup Private"] <- sample(dat$Deduct.x[dat$AgeGroup=="19-34" & dat$InsCat=="NonGroup Private"], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="NonGroup Private"], replace=TRUE)
cps$Deduct[cps$AgeGroup=="35-49" & cps$InsCat=="NonGroup Private"] <- sample(dat$Deduct.x[dat$AgeGroup=="35-49" & dat$InsCat=="NonGroup Private"], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="NonGroup Private"], replace=TRUE)
cps$Deduct[cps$AgeGroup=="50-64" & cps$InsCat=="NonGroup Private"] <- sample(dat$Deduct.x[dat$AgeGroup=="50-64" & dat$InsCat=="NonGroup Private"], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="NonGroup Private"], replace=TRUE)

if (policy=="Baseline"){
  cps$SLF <- 0
  cps$SLF[cps$AgeGroup=="<19" & cps$InsCat=="Other Public"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="<19" & dat$InsCat=="Other Public"], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="Other Public",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="Other Public"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="<19" & cps$InsCat=="Medicaid"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="<19" & dat$InsCat=="Medicaid"], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="Medicaid",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="Medicaid"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="<19" & cps$InsCat=="Other Private"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="<19" & dat$InsCat=="Other Private"], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="Other Private",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="Other Private"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="<19" & cps$InsCat=="Uninsured"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="<19" & (dat$InsCat=="Uninsured")], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="Uninsured",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="Uninsured"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="<19" & cps$InsCat=="NonGroup Private"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="<19" & (dat$InsCat=="NonGroup Private")], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="NonGroup Private"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="19-34" & cps$InsCat=="Other Public"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="19-34" & dat$InsCat=="Other Public"], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="Other Public",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="Other Public"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="19-34" & cps$InsCat=="Medicaid"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="19-34" & dat$InsCat=="Medicaid"], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="Medicaid",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="Medicaid"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="19-34" & cps$InsCat=="Other Private"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="19-34" & dat$InsCat=="Other Private"], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="Other Private",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="Other Private"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="19-34" & cps$InsCat=="Uninsured"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="19-34" & (dat$InsCat=="Uninsured")], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="Uninsured",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="Uninsured"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="19-34" & cps$InsCat=="NonGroup Private"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="19-34" & (dat$InsCat=="NonGroup Private" )], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="NonGroup Private"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="35-49" & cps$InsCat=="Other Public"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="35-49" & dat$InsCat=="Other Public"], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="Other Public",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="Other Public"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="35-49" & cps$InsCat=="Medicaid"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="35-49" & dat$InsCat=="Medicaid"], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="Medicaid",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="Medicaid"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="35-49" & cps$InsCat=="Other Private"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="35-49" & dat$InsCat=="Other Private"], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="Other Private",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="Other Private"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="35-49" & cps$InsCat=="Uninsured"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="35-49" & (dat$InsCat=="Uninsured")], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="Uninsured",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="Uninsured"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="35-49" & cps$InsCat=="NonGroup Private"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="35-49" & (dat$InsCat=="NonGroup Private")], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="NonGroup Private"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="50-64" & cps$InsCat=="Other Public"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="50-64" & dat$InsCat=="Other Public"], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="Other Public",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="Other Public"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="50-64" & cps$InsCat=="Medicaid"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="50-64" & dat$InsCat=="Medicaid"], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="Medicaid",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="Medicaid"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="50-64" & cps$InsCat=="Other Private"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="50-64" & dat$InsCat=="Other Private"], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="Other Private",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="Other Private"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="50-64" & cps$InsCat=="Uninsured"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="50-64" & (dat$InsCat=="Uninsured")], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="Uninsured",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="Uninsured"], replace=TRUE)
  cps$SLF[cps$AgeGroup=="50-64" & cps$InsCat=="NonGroup Private"] <- sample(dat$TOTSLFY1[dat$AgeGroup=="50-64" & (dat$InsCat=="NonGroup Private")], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="NonGroup Private"], replace=TRUE)
  #cps$SLF[cps$AgeGroup=="65+"] <- sample(dat$TOTSLFY1, size=nrow(cps[cps$AgeGroup=="65+",]), replace=TRUE)
}

if (policy=="HYBRID"){
  cps$InsCat[cps$FPL<400] <- "Medicaid"
} else if (policy=="HYBRID2"){
  cps$InsCat[cps$FPL<250] <- "Medicaid"
}

if (other.test=="ADJUST.INSCAT"){
  # Insurance Category corrections - sensitivity test
  cps$InsCat2[cps$InsCat2==1 & cps$FPL<=100] <- 4 # Correct anyone at or below 100% FPL on exchange to be uninsured
  cps$InsCat2[cps$InsCat2==3 & cps$FPL>200 & cps$FPL<400] <- 1 # Correct anyone on Medicaid between 200-400 FPL to be on the exchange
  cps$InsCat2[cps$InsCat2==3 & cps$FPL>=400] <- 1 # Correct anyone on Medicaid at or over 400 FPL to be on group private (ESI)
}

cps$InsCat <- factor(cps$InsCat2, labels=c("NonGroup Private","Other Public","Medicaid","Uninsured","Other Private"))

####################################################################

################################################
### Adjust Population & Income Distributions ###
################################################
reset.twentysix <- function(twentysix.pop, year){
  years.left <- m.years - year + 1  # The number of years of income we get will be this number + 1
  sampled <- sample(seq(1:nrow(fam26)),nrow(twentysix.pop),replace=TRUE,prob=fam26$w)  # Sample from pool of 26 year olds
  trajectory.sample <- fam26[sampled,] # Extract the info for these sampled trajectories
  incomes <- trajectory.sample[,which(colnames(fam26)=="t0"):which(colnames(fam26)==paste("t",(years.left-1),sep=''))] # Take incomes for the years we need (the years left in the model)
  trajectory.sample[,"taxid"] <- twentysix.pop$TID # Add the taxid for the 26 year olds in our population
  trajectory.sample[,which(colnames(fam26)=="t0"):which(colnames(fam26)=="t15")] <- 0 # Reset
  trajectory.sample[,which(colnames(fam26)==paste("t",(year-1),sep='')):which(colnames(fam26)=="t15")] <- ifelse(years.left==1,incomes,incomes[,1:which(colnames(incomes)==paste("t",(years.left-1),sep=''))]) # Add the incomes for the appropriate years
  fam <- rbind(fam, trajectory.sample) # Add these trajectories to our "fam" dataframe
  assign('fam',fam,envir=.GlobalEnv) # This updates the fam dataframe in the global environment
}

remove.wage.income <- function(pop, year){
  will.lose.wages <- which(pop$Survive==0 | pop$Age==64)              # Indexes for anyone who has passed away or about to retire
  fams.to.reduce.income <- pop[will.lose.wages,"TID"]                 # Taxids of families of these individuals
  #pop[pop$TID %in% fams.to.reduce.income,"IncomeAdjustRequired"] <- 1   # Indicate that family members will require income adjustment
  adjustments <- pop[will.lose.wages,c("TID","WageProp","FamIncome")]
  a2 <- adjustments %>%
    dplyr::group_by(TID) %>%
    dplyr::summarise(WageProp=sum(WageProp),FamIncome=mean(FamIncome))
  a2$WageProp <- pmin(1,a2$WageProp)
  a2$NewFamIncome <- (1-a2$WageProp)*a2$FamIncome
  pop <- pop %>%
    dplyr::left_join(a2[,c("TID","NewFamIncome")], by=c("TID"))
  pop["FamIncome"] <- pop["NewFamIncome"]
  pop <- within(pop,rm("NewFamIncome"))
  
  years.left <- m.years - year  # The number of years of income we get will be this number + 1
  taxable.left <- paste("t",(year-1):(m.years-1),sep='')#; q.left <- paste("q",year:(m.years-1),sep='') # Names of the taxable income & quintile variables
  fam$TID <- unlist(fam$taxid,use.names=FALSE)
  fam <- fam %>%
    dplyr::left_join(a2[,c("TID","WageProp")], by=c("TID"))
  fam[is.na(fam$WageProp),"WageProp"] <- 0
  fam[,taxable.left] <- fam[,taxable.left]*(1-fam[,"WageProp"]) # Adjust income
  
  # Now update income quintiles
  # for (y in 1:years.left){
  #   fam[fam$TID %in% fams.to.reduce.income, q.left[y]] <- 1
  #   fam[fam$taxable.left >= q2thresh & fam$taxable.left <= q3thresh & fam$TID %in% fams.to.reduce.income, q.left[y]] <- 2
  #   fam[fam$taxable.left >= q3thresh & fam$taxable.left <= q4thresh & fam$TID %in% fams.to.reduce.income, q.left[y]] <- 3
  #   fam[fam$taxable.left >= q4thresh & fam$taxable.left <= q5thresh & fam$TID %in% fams.to.reduce.income, q.left[y]] <- 4
  #   fam[fam$taxable.left >= q5thresh, q.left[y]] <- 5
  # }
  fam <- within(fam,rm(TID))
  fam <- within(fam,rm(WageProp))
  assign('fam',fam,envir=.GlobalEnv) # This updates the fam dataframe in the global environment
  return(pop)
}

##########################
### Income Calculation ###
##########################
# This is the original income transition function, reading from the distributions developed by Carter in CPS_data.R
new.income <- function(pop, year){
  fam$TID <- unlist(fam$taxid,use.names=FALSE) # Tax IDs - rename so we can merge fam & pop
  year.taxable <- paste("t",year,sep='') # We need this to select the right year's income
  population <- pop %>%
    dplyr::group_by(TID) %>%
    dplyr::left_join(fam, by=c("TID")) # This piece merges our population with the "fam" dataset which has the income trajectories
  population <- as.data.frame(population)
  pop["FamIncome"] <- population[,year.taxable] # Choose the income for this year
  return(pop)
}


# This version is basically the same as the above but allows for Insurance Category transitions if income changes significantly
new.income2 <- function(pop, year){
  fam$TID <- unlist(fam$taxid,use.names=FALSE) # Tax IDs - rename so we can merge fam & pop
  year.taxable <- paste("t",(year-1),sep='') # We need this to select the right year's income
  #year.q <- paste("q",year,sep=''); year.qlast <- paste("q",(year-1),sep='')
  population <- pop %>%
    dplyr::group_by(TID) %>%
    dplyr::left_join(fam, by=c("TID")) # This piece merges our population with the "fam" dataset which has the income trajectories
  population <- as.data.frame(population)
  #transition.rates.by.quintile <- matrix(data=0,nrow=5,ncol=5); colnames(transition.rates.by.quintile) <- levels(population$InsCat)
  #transition.rates.by.quintile <- table(population$q1[population$q1>0],population$InsCat[population$q1>0]); 
  #for(row in 1:nrow(transition.rates.by.quintile)){transition.rates.by.quintile[row,] <- round(transition.rates.by.quintile[row,]/rowSums(transition.rates.by.quintile)[row],digits=2)}
  transition.rates.by.quintile <- tr
  population$PovCat[population$FPL<=100] <- 1
  population$PovCat[population$FPL>100 & population$FPL<=150] <- 2
  population$PovCat[population$FPL>150 & population$FPL<=250] <- 3
  population$PovCat[population$FPL>250 & population$FPL<=400] <- 4
  population$PovCat[population$FPL>400 & population$FPL<=600] <- 5
  population$PovCat[population$FPL>600 & population$FPL<=800] <- 6
  population$PovCat[population$FPL>800] <- 7
  #pop["InsCat"] <- ifelse(population[,year.q]==population[,year.qlast], population[,"InsCat"], sapply(population[,"PovCat"], function(x) which(levels(population[,"InsCat"])==sample(colnames(transition.rates.by.quintile),1,prob=transition.rates.by.quintile[x,]))))
  diff.income <- ifelse(population[,year.taxable]>1.1*population[,"FamIncome"] | population[,year.taxable]<0.9*population[,"FamIncome"], 1, 0) # Check for significant change in income
  pop["InsCat"] <- ifelse(diff.income==1, population[,"InsCat"], sapply(population[,"PovCat"], function(x) which(levels(population[,"InsCat"])==sample(colnames(transition.rates.by.quintile),1,prob=transition.rates.by.quintile[x,]))))
  pop["FamIncome"] <- population[,year.taxable] # Choose the income for this year
  if (other.test=="ADJUST.INSCAT"){
    # Insurance Category corrections - sensitivity test
    cps$InsCat2[cps$InsCat2==1 & cps$FPL<=100] <- 4 # Correct anyone at or below 100% FPL on exchange to be uninsured
    cps$InsCat2[cps$InsCat2==3 & cps$FPL>200 & cps$FPL<400] <- 1 # Correct anyone on Medicaid between 200-400 FPL to be on the exchange
    cps$InsCat2[cps$InsCat2==3 & cps$FPL>=400] <- 1 # Correct anyone on Medicaid at or over 400 FPL to be on group private (ESI)
  }
  if (policy=="HYBRID"){
    pop$InsCat[pop$FPL<400] <- 3
  } else if (policy=="HYBRID2"){
    pop$InsCat[pop$FPL<250] <- 3
  }
  pop$InsCat <- factor(pop$InsCat, labels=c("NonGroup Private","Other Public","Medicaid","Uninsured","Other Private"))
  return(pop)
}


###################
### Immigration ###
###################
# Approx 29% of immigrants are eligible.  Approximately 42% of immigrant families have at least one eligible member.  
# Since only approx 70% of the individuals in those families that make up the 42% are actually eligible, if we sample
# to get 42% "elig" immigrant families and 58% "inelig", we should actually get approx the right breakdown.
immigs <- cps[cps$Immig==1,]
prop.elig.immig <- sum(cps[(cps$InsCat=="NonGroup Private" | cps$InsCat=="Uninsured") & cps$AGE<65 & cps$Immig==1,"ASECWT"])/sum(cps[cps$Immig==1,"ASECWT"])
elig.immig.fams <- unique(unlist(cps[(cps$InsCat=="NonGroup Private" | cps$InsCat=="Uninsured") & cps$AGE<65 & cps$Immig==1,"taxid"]),use.names=FALSE)
prop.elig.immig.fams <- sum(cps[cps$taxid %in% elig.immig.fams,"ASECWT"])/sum(cps[cps$Immig==1,"ASECWT"])
elig.immig.fam.set <- cps[cps$taxid %in% elig.immig.fams,] %>%
  dplyr::group_by(taxid) %>%
  dplyr::summarise(weight=sum(ASECWT))
inelig.immig.fam.set <- cps[cps$taxid %in% immigs$taxid & !cps$taxid %in% elig.immig.fams,] %>%
  dplyr::group_by(taxid) %>%
  dplyr::summarise(weight=sum(ASECWT))
mean.elig.fam.wt <- mean(elig.immig.fam.set$weight)
mean.inelig.fam.wt <- mean(inelig.immig.fam.set$weight)
#N.immigs <- 1127167

add.immigrants2 <- function(pop,immig.pop,max.tid,max.id,year){
  N.immigs <- immigs.per.year[year]
  years.left <- m.years - year + 1
  N.elig.fams <- round(prop.elig.immig.fams*N.immigs/mean.elig.fam.wt)
  N.inelig.fams <- round((1-prop.elig.immig.fams)*N.immigs/mean.inelig.fam.wt)
  add.elig <- sample(elig.immig.fam.set$taxid,N.elig.fams,prob=elig.immig.fam.set$weight,replace=FALSE) # Choose IDs
  add.inelig <- sample(inelig.immig.fam.set$taxid,N.inelig.fams,prob=inelig.immig.fam.set$weight,replace=FALSE) # Choose IDs
  added <- c(add.elig,add.inelig)
  trajectory.sample <- famImmig[famImmig[,"taxid"] %in% added,] # This copies the income trajectory for the family
  trajectory.sample <- cbind(trajectory.sample,new=((max.tid+1):(max.tid+length(added)))) # add a column for the new ids
  tid.conversion <- as.data.frame(trajectory.sample[,c("taxid","new")])
  colnames(tid.conversion) <- c("TID","new")
  incomes <- trajectory.sample[,which(colnames(famImmig)=="t0"):which(colnames(famImmig)==paste("t",(years.left-1),sep=''))] # Take incomes for the years we need (the years left in the model)
  trajectory.sample[,which(colnames(famImmig)==paste("t",(year-1),sep='')):which(colnames(famImmig)=="t15")] <- ifelse(years.left==1,incomes,incomes[,1:which(colnames(incomes)==paste("t",(years.left-1),sep=''))]) # Add the incomes for the appropriate years
  new.rows <- immig.pop[immig.pop[,"TID"] %in% added,]
  new.rows <- merge(new.rows,tid.conversion,by="TID")
  new.rows["TID"] <- new.rows["new"]
  new.rows["id"] <- max.id + c(1:nrow(new.rows))
  new.rows["Preg"] <- "not pregnant"
  new.rows <- dplyr::select(new.rows, -c("new"))
  trajectory.sample[,"taxid"] <- trajectory.sample[,"new"]
  trajectory.sample <- dplyr::select(trajectory.sample, -c("new"))
  fam <- rbind(fam, trajectory.sample) # Add these trajectories to our "fam" dataframe
  assign('fam',fam,envir=.GlobalEnv) # This updates the fam dataframe in the global environment
  pop <- rbind(pop,new.rows)
  return(pop)
}

# add.immigrants <- function(pop,maxid,year){
#   years.left <- m.years - year + 1
#   desired.wt <- 1127167
#   elig.wt <- 0; inelig.wt <- 0
#   while(elig.wt<(prop.elig.immig*desired.wt)){
#     added <- sample(eligible.immigs$TID,1,prob=eligible.immigs$WT) # Choose IDs
#     elig.wt <- elig.wt + sum(eligible.immigs[eligible.immigs$TID==added,"WT"]) # Update weight
#     trajectory.sample <- famImmig[famImmig$taxid==added,]
#     trajectory.sample[,"taxid"] <- maxid+1; maxid <- maxid+1 # Generate a new taxid for this family
#     incomes <- trajectory.sample[,which(colnames(famImmig)=="q0"):which(colnames(famImmig)==paste("taxable",years.left,sep=''))] # Take incomes for the years we need (the years left in the model)
#     trajectory.sample[,which(colnames(famImmig)==paste("q",year,sep='')):which(colnames(famImmig)=="taxable16")] <- incomes[,5:which(colnames(incomes)==paste("taxable",years.left,sep=''))] # Add the incomes for the appropriate years
#     fam <- rbind(fam, trajectory.sample) # Add these trajectories to our "fam" dataframe
#     assign('fam',fam,envir=.GlobalEnv) # This updates the fam dataframe in the global environment
#     new.rows <- eligible.immigs[eligible.immigs$TID==added,]
#     new.rows["TID"] <- maxid
#     pop <- rbind(pop,new.rows)
#   }
#   while(inelig.wt<((1-prop.elig.immig)*desired.wt)){
#     added <- sample(ineligible.immigs$TID,1,prob=ineligible.immigs$WT) # Choose IDs
#     inelig.wt <- inelig.wt + sum(ineligible.immigs[ineligible.immigs$TID==added,"WT"]) # Update weight
#     trajectory.sample <- famImmig[famImmig$taxid==added,]
#     trajectory.sample[,"taxid"] <- maxid+1; maxid <- maxid+1 # Generate a new taxid for this family
#     incomes <- trajectory.sample[,which(colnames(famImmig)=="q0"):which(colnames(famImmig)==paste("taxable",years.left,sep=''))] # Take incomes for the years we need (the years left in the model)
#     trajectory.sample[,which(colnames(famImmig)==paste("q",year,sep='')):which(colnames(famImmig)=="taxable16")] <- incomes[,5:which(colnames(incomes)==paste("taxable",years.left,sep=''))] # Add the incomes for the appropriate years
#     fam <- rbind(fam, trajectory.sample) # Add these trajectories to our "fam" dataframe
#     assign('fam',fam,envir=.GlobalEnv) # This updates the fam dataframe in the global environment
#     new.rows <- ineligible.immigs[ineligible.immigs$TID==added,]
#     new.rows["TID"] <- maxid
#     pop <- rbind(pop,new.rows)
#   }
#   return(pop)
# }

################
### Old Code ###
################

#   pop["FamIncome"] <- population[,year.taxable] # Choose the income for this year
#   pop["InsCat"] <- ifelse(population[,year.q]==population[,year.qlast], population[,"InsCat"], sapply(population[,"InsCat"], function(x) which(levels(population[,"InsCat"])==sample(colnames(transitions),1,prob=transitions[x,]))))
#   # pop["InsCat"] <- ifelse(pop["FPL"]>138,
#   #                         ifelse(pop["Age"]>26,
#   #                                ifelse(pop["Preg"]=="not pregnant",
#   #                                       sapply(population[,"InsCat"], function(x) which(levels(population[,"InsCat"])==sample(colnames(transitions)[c(1,2,4,5)],1,prob=transitions["Medicaid",c(1,2,4,5)])),
#   #                                       population[,"InsCat"])),
#   #                                population[,"InsCat"]),
#   #                         population[,"InsCat"])
#   pop$InsCat <- factor(pop$InsCat, labels=c("NonGroup Private","Other Public","Medicaid","Uninsured","Other Private"))
#   return(pop)
# }



# This function determines new income quintile - will incorporate with Carter's income transitions
# new.quint <- function(pop){
#   old <- pop["FamIncome"]
#   q <- quantile(old, probs=seq(0,1,.2))
#   curr <- cut(ecdf(old)(old), breaks=seq(0,1,.2),labels=c(1,2,3,4,5))
#   t <- apply(old, 1, function(x) sample(5,1,prob=qt[cut(ecdf(x)(x), breaks=seq(0,1,.2),labels=c(1,2,3,4,5)),]))
# }
# 

