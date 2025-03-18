#### A script written by Adrienne Propp in Oct 2019
#### Input parameters for 10Plan

######################
## Model Parameters ##
######################
# If we run model for 15 years and debt is forgiven after 15 years, then we won't capture costs associated with forgiven debt
#set.seed(123)           # Provides reproducible results
m.years       <- 16     # Years to run model
d.years       <- 15     # Years before forgiving debt
n             <- 5000   # Agents in model for test run
#interest.rate <- 1.03   # Interest rate for loans - now this is accounted for in 10Plan_RunFile.R
medical.inflation.rate <- 1.051 # Medical inflation rate from the literature
#medical.inflation.rate.reduced <- 1.044 # Reduced rate for sensitivity analysis
#corrected.inflation <- medical.inflation.rate # Medical inflation rate from the literature
#if(which.prices=="MCare_reduced"){corrected.inflation <- medical.inflation.rate.reduced} # Reduced rate for sensitivity analysis
if(which.prices=="MCare_reduced"){medical.inflation.rate <- 1.044} # Reduced rate for sensitivity analysis
borrowing.param <- 1.15
reduced.copay <- 10
uninsured.increase.demand <- 0.2
uninsured.increase.demand.higher <- 0.35

###############################
## Initialize Variable Lists ##
###############################
cols.fam <- c("TID","INCOME","FPL","ELIG","OOPCAP","MEDSPEND_THISYEAR","COPAY_THISYEAR","BORROW_THISYEAR","BORROWED_YTD","PAYMENT_THISYEAR","TOTAL_LOAN_BAL","FORGIVEN")
loans <- paste("Loan.",1:(m.years+1),sep=""); cols.fam <- c(cols.fam, loans)
forgiveness <- c("Forgiven.26","Forgiven.65","Forgiven.death","Forgiven.expire") # Forgiveness of each type for this year only
totals <- c("Loans.totalthisyear","Repayment.thisyear","Forgiven.thisyear") # Total CURRENT loan balance, total repaid to THIS YEAR, total forgiven THIS YEAR
overalltotals <- c("Ever.Borrowed","Tot.Interest","Ever.Forgiven","Ever.Repaid","To.Parents","From.Children") # These are cumulative, so totals TO DATE

################################
## Population Size Parameters ##
################################
evol <- read.xlsx(here("Data/popevol.xlsx")) # Table 1 from https://census.gov/data/tables/2017/demo/popproj/2017-summary-tables.html 
births.per.year <- as.numeric(evol[10:25,"X5"])*1000
deaths.per.year <- as.numeric(evol[10:25,"X6"])*1000*0.2651851
# We are only looking at those under 65 years, and according to the CDC only 26.51851% of deaths are for this group
immigs.per.year <- as.numeric(evol[10:25,"X8"])*1000

load(here("NoteBooks/Integration/R_Objects/dat.Rda"))
dat$PovCat <- 0
dat$PovCat[dat$POVLEVY2<=100] <- 1
dat$PovCat[dat$POVLEVY2>100 & dat$POVLEVY2<=150] <- 2
dat$PovCat[dat$POVLEVY2>150 & dat$POVLEVY2<=250] <- 3
dat$PovCat[dat$POVLEVY2>250 & dat$POVLEVY2<=400] <- 4
dat$PovCat[dat$POVLEVY2>400 & dat$POVLEVY2<=600] <- 5
dat$PovCat[dat$POVLEVY2>600 & dat$POVLEVY2<=800] <- 6
dat$PovCat[dat$POVLEVY2>800] <- 7
#transition.rates.by.quintile <- matrix(data=0,nrow=5,ncol=5); colnames(transition.rates.by.quintile) <- levels(population$InsCat)
#transition.rates.by.quintile[1,] <- c(.06,.03,.59,.17,.15) #https://www.kff.org/other/state-indicator/nonelderly-up-to-100-fpl/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
tr <- round(questionr::wtd.table(dat$PovCat,dat$InsCat,weights=dat$LONGWT),2)
for(row in 1:nrow(tr)){tr[row,] <- round(tr[row,]/rowSums(tr)[row],digits=2)}
trTOT <- round(questionr::wtd.table(dat$InsCat,weights=dat$LONGWT),2)
trTOT <- round(trTOT/sum(trTOT),digits=2)
###################################
## Set up Behavioral Adjustments ##
###################################
utilization.adjustment.ranges.uninsured <- matrix(0, nrow=4, ncol=2,dimnames=list(c("Outpatient","Inpatient","ER","All Other"),c("Conservative","Aggressive")))
utilization.adjustment.ranges.uninsured[,1] <- c(1.056, 1.016, 0.768, 1.2)
utilization.adjustment.ranges.uninsured[,2] <- c(1.44, 1.232, 1.328, 1.2)
utilization.adjustment.ranges.insured <- matrix(0, nrow=5, ncol=2,dimnames=list(c("Deductible Not Met","Deductible Met - Outpatient","Deductible Met - Inpatient","Deductible Met - ER","Deductible Met - All Other"),c("Conservative","Aggressive")))
utilization.adjustment.ranges.insured[,1] <- c(1.2, .86, .83, .79, .95)
utilization.adjustment.ranges.insured[,2] <- c(1.2, .8, .8, .66, .75)

##################################
## Tabulate Results of Interest ##
##################################
tabulate.results <- function(results){
  target <- results[results$Elig==1,]
  hybrid.target <- results[results$Elig==1 | results$InsCat=="Medicaid",]
  medicaid.pop <- results[results$InsCat=="Medicaid",]
  all.fams <- results %>%
    dplyr::group_by(TID) %>%
    dplyr::summarise(FamIncome=mean(FamIncome),FPL=mean(FPL),MedSpend=sum(MedSpend*Elig),Visits=sum(Visits*Elig),OOP=sum(OOP*Elig),Cap=mean(Cap),Borrow=sum(Borrow*Elig),Loans=sum(Loans.totalthisyear),Payments=sum(Repayment.thisyear),Forgiven=sum(Forgiven.thisyear),Elig=sum(Elig),WT=mean(WTH))
  target.fams <- all.fams[all.fams$Elig>=1,]
  
  
  tot.spending.10P <- sum(target["MedSpend"]*target["WT"])
  tot.spending.under.400FPL <- sum(target[results$FPL<=400,"MedSpend"]*target[results$FPL<=400,"WT"])
  tot.spending.hybrid.target <- sum(hybrid.target["MedSpend"]*hybrid.target["WT"])
  tot.spending.medicaid <- sum(medicaid.pop["MedSpend"]*medicaid.pop["WT"])
  tot.pop <- sum(results["WT"])
  tot.pop.10p <- sum(target["WT"])
  tot.pop.10p.unins <- sum(target[target$InsCat=="Uninsured","WT"])
  tot.pop.10p.ngpriv <- sum(target[target$InsCat=="NonGroup Private","WT"])
  tot.pop.under.400FPL <- sum(target[results$FPL<=400,"WT"])
  total.borrowed.thisyear <- sum(target["Borrow"]*target["WT"])
  total.borrowed.thisyear.unins <- sum(target[target$InsCat=="Uninsured","Borrow"]*target[target$InsCat=="Uninsured","WT"])
  total.borrowed.thisyear.ngpriv <- sum(target[target$InsCat=="NonGroup Private","Borrow"]*target[target$InsCat=="NonGroup Private","WT"])
  total.interest.thisyear <- sum(target["Yearly.Interest"]*target["WT"])
  total.interest.thisyear.unins <- sum(target[target$InsCat=="Uninsured","Yearly.Interest"]*target[target$InsCat=="Uninsured","WT"])
  total.interest.thisyear.ngpriv <- sum(target[target$InsCat=="NonGroup Private","Yearly.Interest"]*target[target$InsCat=="NonGroup Private","WT"])
  total.premiums.thisyear <- sum(target["Repayment.thisyear"]*target["WT"])
  total.premiums.thisyear.unins <- sum(target[target$InsCat=="Uninsured","Repayment.thisyear"]*target[target$InsCat=="Uninsured","WT"])
  total.premiums.thisyear.ngpriv <- sum(target[target$InsCat=="NonGroup Private","Repayment.thisyear"]*target[target$InsCat=="NonGroup Private","WT"])
  total.premiums.thisyear.all <- sum(results["Repayment.thisyear"]*results["WT"])
  total.premiums.thisyear.all.unins <- sum(results[target$InsCat=="Uninsured","Repayment.thisyear"]*results[target$InsCat=="Uninsured","WT"])
  total.premiums.thisyear.all.ngpriv <- sum(results[target$InsCat=="NonGroup Private","Repayment.thisyear"]*results[target$InsCat=="NonGroup Private","WT"])
  total.forgiven.thisyear <- sum(target["Forgiven.thisyear"]*target["WT"])
  total.forgiven.thisyear.all <- sum(results["Forgiven.thisyear"]*results["WT"])
  #net.cashflow <- total.interest.thisyear + total.premiums.thisyear - total.forgiven.thisyear
  total.copays <- sum(target["OOP"]*target["WT"])
  mean.copay.all <- sum(results["OOP"]*results["WT"])/sum(results["WT"])
  mean.copay.10P <- sum(target["OOP"]*target["WT"])/sum(target["WT"])
  mean.copay.all.fam <- sum(all.fams["OOP"]*all.fams["WT"])/sum(all.fams["WT"])
  mean.copay.10P.fam <- sum(target.fams["OOP"]*target.fams["WT"])/sum(target.fams["WT"])
  median.copay.all <- weighted.median(results[,"OOP"],results[,"WT"])
  median.copay.10P <- weighted.median(target[,"OOP"],target[,"WT"])
  median.copay.all.fam <- weighted.median(unlist(all.fams[,"OOP"],use.names=FALSE),unlist(all.fams[,"WT"],use.names=FALSE))
  median.copay.10P.fam <- weighted.median(unlist(target.fams[,"OOP"],use.names=FALSE),unlist(target.fams[,"WT"],use.names=FALSE))
  mean.spending.10P <- tot.spending.10P/tot.pop.10p
  median.spending.10P <- weighted.median(target[,"MedSpend"],target[,"WT"])
  mean.fam.spending.10P <- sum(target.fams["MedSpend"]*target.fams["WT"])/sum(target.fams["WT"])
  median.fam.spending.10P <- weighted.median(unlist(target.fams[,"MedSpend"],use.names=FALSE),unlist(target.fams[,"WT"],use.names=FALSE))
  total.borrowed.ever <- sum(target["Ever.Borrowed"]*target["WT"])
  total.interest.ever <- sum(target["Tot.Interest"]*target["WT"])
  total.premiums.ever <- sum(target["Ever.Repaid"]*target["WT"])
  total.forgiven.ever <- sum(target["Ever.Forgiven"]*target["WT"])
  min.fam.spending.10P <- min(target.fams["MedSpend"])
  q25.fam.spending.10P <- wtd.quantile(unlist(target.fams["MedSpend"]),0.25,weight=unlist(target.fams["WT"]))[[1]]
  q75.fam.spending.10P <- wtd.quantile(unlist(target.fams["MedSpend"]),0.75,weight=unlist(target.fams["WT"]))[[1]]
  max.fam.spending.10P <- max(target.fams["MedSpend"])
  
  total.visits.unins <- sum(target[target$InsCat=="Uninsured","Visits"]*target[target$InsCat=="Uninsured","WT"])
  total.visits.ngpriv <-sum(target[target$InsCat=="NonGroup Private","Visits"]*target[target$InsCat=="NonGroup Private","WT"])
  mean.visits.unins <- sum(target[target$InsCat=="Uninsured","Visits"]*target[target$InsCat=="Uninsured","WT"])/sum(target["WT"])
  mean.visits.ngpriv <- sum(target[target$InsCat=="NonGroup Private","Visits"]*target[target$InsCat=="NonGroup Private","WT"])/sum(target["WT"])
  
  
  mean.spending.10P.uninsured <- sum(target[target$InsCat=="Uninsured","MedSpend"]*target[target$InsCat=="Uninsured","WT"])/sum(target[target$InsCat=="Uninsured","WT"])
  median.spending.10P.uninsured <- weighted.median(target[target$InsCat=="Uninsured","MedSpend"],target[target$InsCat=="Uninsured","WT"])
  mean.spending.10P.ngpriv <- sum(target[target$InsCat=="NonGroup Private","MedSpend"]*target[target$InsCat=="NonGroup Private","WT"])/sum(target[target$InsCat=="NonGroup Private","WT"])
  median.spending.10P.ngpriv <- weighted.median(target[target$InsCat=="NonGroup Private","MedSpend"],target[target$InsCat=="NonGroup Private","WT"])
  mean.copay.10P.uninsured <- sum(target[target$InsCat=="Uninsured","OOP"]*target[target$InsCat=="Uninsured","WT"])/sum(target[target$InsCat=="Uninsured","WT"])
  median.copay.10P.uninsured <- weighted.median(target[target$InsCat=="Uninsured","OOP"],target[target$InsCat=="Uninsured","WT"])
  mean.copay.10P.ngpriv <- sum(target[target$InsCat=="NonGroup Private","OOP"]*target[target$InsCat=="NonGroup Private","WT"])/sum(target[target$InsCat=="NonGroup Private","WT"])
  median.copay.10P.ngpriv <- weighted.median(target[target$InsCat=="NonGroup Private","OOP"],target[target$InsCat=="NonGroup Private","WT"])
  mean.repayments.10P.uninsured <- sum(target[target$InsCat=="Uninsured","Repayment.thisyear"]*target[target$InsCat=="Uninsured","WT"])/sum(target[target$InsCat=="Uninsured","WT"])
  median.repayments.10P.uninsured <- weighted.median(target[target$InsCat=="Uninsured","Repayment.thisyear"],target[target$InsCat=="Uninsured","WT"])
  mean.repayments.10P.ngpriv <- sum(target[target$InsCat=="NonGroup Private","Repayment.thisyear"]*target[target$InsCat=="NonGroup Private","WT"])/sum(target[target$InsCat=="NonGroup Private","WT"])
  median.repayments.10P.ngpriv <- weighted.median(target[target$InsCat=="NonGroup Private","Repayment.thisyear"],target[target$InsCat=="NonGroup Private","WT"])
  
  
  mean.spending.10P.goodhealth <- sum(target[target$HS=="good","MedSpend"]*target[target$HS=="good","WT"])/sum(target[target$HS=="good","WT"])
  median.spending.10P.goodhealth <- weighted.median(target[target$HS=="good","MedSpend"],target[target$HS=="good","WT"])
  mean.spending.10P.badhealth <- sum(target[target$HS=="bad","MedSpend"]*target[target$HS=="bad","WT"])/sum(target[target$HS=="bad","WT"])
  median.spending.10P.badhealth <- weighted.median(target[target$HS=="bad","MedSpend"],target[target$HS=="bad","WT"])
  mean.spending.10P.acute <- sum(target[target$Morbidity=="acute","MedSpend"]*target[target$Morbidity=="acute","WT"])/sum(target[target$Morbidity=="acute","WT"])
  median.spending.10P.acute <- weighted.median(target[target$Morbidity=="acute","MedSpend"],target[target$Morbidity=="acute","WT"])
  mean.spending.10P.chronic <- sum(target[target$Morbidity=="chronic","MedSpend"]*target[target$Morbidity=="chronic","WT"])/sum(target[target$Morbidity=="chronic","WT"])
  median.spending.10P.chronic <- weighted.median(target[target$Morbidity=="chronic","MedSpend"],target[target$Morbidity=="chronic","WT"])
  mean.copay.10P.goodhealth <- sum(target[target$HS=="good","OOP"]*target[target$HS=="good","WT"])/sum(target[target$HS=="good","WT"])
  median.copay.10P.goodhealth <- weighted.median(target[target$HS=="good","OOP"],target[target$HS=="good","WT"])
  mean.copay.10P.badhealth <- sum(target[target$HS=="bad","OOP"]*target[target$HS=="bad","WT"])/sum(target[target$HS=="bad","WT"])
  median.copay.10P.badhealth <- weighted.median(target[target$HS=="bad","OOP"],target[target$HS=="bad","WT"])
  mean.copay.10P.acute <- sum(target[target$Morbidity=="acute","OOP"]*target[target$Morbidity=="acute","WT"])/sum(target[target$Morbidity=="acute","WT"])
  median.copay.10P.acute <- weighted.median(target[target$Morbidity=="acute","OOP"],target[target$Morbidity=="acute","WT"])
  mean.copay.10P.chronic <- sum(target[target$Morbidity=="chronic","OOP"]*target[target$Morbidity=="chronic","WT"])/sum(target[target$Morbidity=="chronic","WT"])
  median.copay.10P.chronic <- weighted.median(target[target$Morbidity=="chronic","OOP"],target[target$Morbidity=="chronic","WT"])
  mean.repayments.10P.goodhealth <- sum(target[target$HS=="good","Repayment.thisyear"]*target[target$HS=="good","WT"])/sum(target[target$HS=="good","WT"])
  median.repayments.10P.goodhealth <- weighted.median(target[target$HS=="good","Repayment.thisyear"],target[target$HS=="good","WT"])
  mean.repayments.10P.badhealth <- sum(target[target$HS=="bad","Repayment.thisyear"]*target[target$HS=="bad","WT"])/sum(target[target$HS=="bad","WT"])
  median.repayments.10P.badhealth <- weighted.median(target[target$HS=="bad","Repayment.thisyear"],target[target$HS=="bad","WT"])
  mean.repayments.10P.acute <- sum(target[target$Morbidity=="acute","Repayment.thisyear"]*target[target$Morbidity=="acute","WT"])/sum(target[target$Morbidity=="acute","WT"])
  median.repayments.10P.acute <- weighted.median(target[target$Morbidity=="acute","Repayment.thisyear"],target[target$Morbidity=="acute","WT"])
  mean.repayments.10P.chronic <- sum(target[target$Morbidity=="chronic","Repayment.thisyear"]*target[target$Morbidity=="chronic","WT"])/sum(target[target$Morbidity=="chronic","WT"])
  median.repayments.10P.chronic <- weighted.median(target[target$Morbidity=="chronic","Repayment.thisyear"],target[target$Morbidity=="chronic","WT"])
  
  
  min.spending.10P.goodhealth <- min(target[target$HS=="good","MedSpend"]*target[target$HS=="good","WT"])
  q25.spending.10P.goodhealth <- wtd.quantile(target[target$HS=="good","MedSpend"],0.25,weight=target[target$HS=="good","WT"])[[1]]
  q75.spending.10P.goodhealth <- wtd.quantile(target[target$HS=="good","MedSpend"],0.75,weight=target[target$HS=="good","WT"])[[1]]
  q90.spending.10P.goodhealth <- wtd.quantile(target[target$HS=="good","MedSpend"],0.90,weight=target[target$HS=="good","WT"])[[1]]
  q95.spending.10P.goodhealth <- wtd.quantile(target[target$HS=="good","MedSpend"],0.95,weight=target[target$HS=="good","WT"])[[1]]
  q99.spending.10P.goodhealth <- wtd.quantile(target[target$HS=="good","MedSpend"],0.99,weight=target[target$HS=="good","WT"])[[1]]
  max.spending.10P.goodhealth <- max(target[target$HS=="good","MedSpend"]*target[target$HS=="good","WT"])
  min.spending.10P.badhealth <- min(target[target$HS=="bad","MedSpend"]*target[target$HS=="bad","WT"])
  q25.spending.10P.badhealth <- wtd.quantile(target[target$HS=="bad","MedSpend"],0.25,weight=target[target$HS=="bad","WT"])[[1]]
  q75.spending.10P.badhealth <- wtd.quantile(target[target$HS=="bad","MedSpend"],0.75,weight=target[target$HS=="bad","WT"])[[1]]
  q90.spending.10P.badhealth <- wtd.quantile(target[target$HS=="bad","MedSpend"],0.90,weight=target[target$HS=="bad","WT"])[[1]]
  q95.spending.10P.badhealth <- wtd.quantile(target[target$HS=="bad","MedSpend"],0.95,weight=target[target$HS=="bad","WT"])[[1]]
  q99.spending.10P.badhealth <- wtd.quantile(target[target$HS=="bad","MedSpend"],0.99,weight=target[target$HS=="bad","WT"])[[1]]
  max.spending.10P.badhealth <- max(target[target$HS=="bad","MedSpend"]*target[target$HS=="bad","WT"])
  
  
  
  prop.10p <- sum(target["WT"])/sum(results["WT"])
  prop.Medicaid <- sum(results[results$InsCat=="Medicaid","WT"])/sum(results["WT"])
  prop.OthPublic <- sum(results[results$InsCat=="Other Public","WT"])/sum(results["WT"])
  prop.OthPrivate <- sum(results[results$InsCat=="Other Private","WT"])/sum(results["WT"])
  prop.NonGrp <- sum(results[results$InsCat=="NonGroup Private","WT"])/sum(results["WT"])
  prop.UnIns <- sum(results[results$InsCat=="Uninsured","WT"])/sum(results["WT"])
  prop.under400 <- sum(results[results$FPL<=400,"WT"])/sum(results["WT"])
  
  prop.target.indiv.with.loans <- sum(target[target$Loans.totalthisyear>0,"WT"])/sum(target["WT"])
  prop.target.fams.with.loans <- sum(unlist(target.fams[target.fams$Loans>0,"WT"]))/sum(target.fams["WT"])
  prop.target.fams.paying.cap <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0,"WT"]))/sum(target.fams[,"WT"])
  
  min.copay.10P <- min(target[,"OOP"]*target[,"WT"])
  q25.copay.10P <- wtd.quantile(target[,"OOP"],0.25,weight=target[,"WT"])[[1]]
  q50.copay.10P <- wtd.quantile(target[,"OOP"],0.50,weight=target[,"WT"])[[1]]
  q75.copay.10P <- wtd.quantile(target[,"OOP"],0.75,weight=target[,"WT"])[[1]]
  q90.copay.10P <- wtd.quantile(target[,"OOP"],0.90,weight=target[,"WT"])[[1]]
  q95.copay.10P <- wtd.quantile(target[,"OOP"],0.95,weight=target[,"WT"])[[1]]
  q99.copay.10P <- wtd.quantile(target[,"OOP"],0.99,weight=target[,"WT"])[[1]]
  max.copay.10P <- max(target[,"OOP"]*target[,"WT"])
  
  prop.female <- sum(results[results$Sex=="female","WT"])/sum(results[,"WT"])
  prop.male <- sum(results[results$Sex=="male","WT"])/sum(results[,"WT"])
  prop.white <- sum(results[results$Race=="white","WT"])/sum(results[,"WT"])
  prop.black <- sum(results[results$Race=="black","WT"])/sum(results[,"WT"])
  prop.hispanic <- sum(results[results$Race=="hispanic","WT"])/sum(results[,"WT"])
  prop.otherrace <- sum(results[results$Race=="other","WT"])/sum(results[,"WT"])
  prop.under19 <- sum(results[results$AgeGroup=="<19","WT"])/sum(results[,"WT"])
  prop.19to34 <- sum(results[results$AgeGroup=="19-34","WT"])/sum(results[,"WT"])
  prop.35to49 <- sum(results[results$AgeGroup=="35-49","WT"])/sum(results[,"WT"])
  prop.50to64 <- sum(results[results$AgeGroup=="50-64","WT"])/sum(results[,"WT"])
  #prop.65plus <- sum(results[results$AgeGroup=="65+","WT"])/sum(results[,"WT"])
  prop.under100FPL <- sum(results[results$FPL<=100,"WT"])/sum(results[,"WT"])
  prop.100to150FPL <- sum(results[results$FPL>100 & results$FPL<=150,"WT"])/sum(results[,"WT"])
  prop.150to250FPL <- sum(results[results$FPL>150 & results$FPL<=250,"WT"])/sum(results[,"WT"])
  prop.250to400FPL <- sum(results[results$FPL>250 & results$FPL<=400,"WT"])/sum(results[,"WT"])
  prop.400to600FPL <- sum(results[results$FPL>400 & results$FPL<=600,"WT"])/sum(results[,"WT"])
  prop.600to800FPL <- sum(results[results$FPL>600 & results$FPL<=800,"WT"])/sum(results[,"WT"])
  prop.over800FPL <- sum(results[results$FPL>800,"WT"])/sum(results["WT"])
  
  prop.target.female <- sum(target[target$Sex=="female","WT"])/sum(target[,"WT"])
  prop.target.male <- sum(target[target$Sex=="male","WT"])/sum(target[,"WT"])
  prop.target.white <- sum(target[target$Race=="white","WT"])/sum(target[,"WT"])
  prop.target.black <- sum(target[target$Race=="black","WT"])/sum(target[,"WT"])
  prop.target.hispanic <- sum(target[target$Race=="hispanic","WT"])/sum(target[,"WT"])
  prop.target.otherrace <- sum(target[target$Race=="other","WT"])/sum(target[,"WT"])
  prop.target.under19 <- sum(target[target$AgeGroup=="<19","WT"])/sum(target[,"WT"])
  prop.target.19to34 <- sum(target[target$AgeGroup=="19-34","WT"])/sum(target[,"WT"])
  prop.target.35to49 <- sum(target[target$AgeGroup=="35-49","WT"])/sum(target[,"WT"])
  prop.target.50to64 <- sum(target[target$AgeGroup=="50-64","WT"])/sum(target[,"WT"])
  #prop.target.65plus <- sum(target[target$AgeGroup=="65+","WT"])/sum(target[,"WT"])
  prop.target.under100FPL <- sum(target[target$FPL<=100,"WT"])/sum(target[,"WT"])
  prop.target.100to150FPL <- sum(target[target$FPL>100 & target$FPL<=150,"WT"])/sum(target[,"WT"])
  prop.target.150to250FPL <- sum(target[target$FPL>150 & target$FPL<=250,"WT"])/sum(target[,"WT"])
  prop.target.250to400FPL <- sum(target[target$FPL>250 & target$FPL<=400,"WT"])/sum(target[,"WT"])
  prop.target.400to600FPL <- sum(target[target$FPL>400 & target$FPL<=600,"WT"])/sum(target[,"WT"])
  prop.target.600to800FPL <- sum(target[target$FPL>600 & target$FPL<=800,"WT"])/sum(target[,"WT"])
  prop.target.over800FPL <- sum(target[target$FPL>800,"WT"])/sum(target["WT"])
  
  mean.fam.income <- sum(all.fams[,"FamIncome"]*all.fams[,"WT"])/sum(all.fams[,"WT"])
  
  #if(empty(target.fams[target.fams$Payments>0,])){effective.repayment.rate.under100=NA;effective.repayment.rate.100to150=NA;effective.repayment.rate.150to250=NA;effective.repayment.rate.250to400=NA;effective.repayment.rate.400to600=NA;effective.repayment.rate.600to800=NA;effective.repayment.rate.over800=NA} else{
  
  if(policy!="HYBRID"){
    if(policy!="HYBRID2"){
      mean.effective.repayment.rate.under100 <- sum(target.fams[target.fams$FPL<=100,"Payments"]*target.fams[target.fams$FPL<=100,"WT"]/target.fams[target.fams$FPL<=100,"FamIncome"])/sum(target.fams[target.fams$FPL<=100,"WT"])
      mean.effective.repayment.rate.100to150 <- sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]*target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"])/sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])
      mean.effective.repayment.rate.150to250 <- sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]*target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"])/sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])
    } else{mean.effective.repayment.rate.under100=0;mean.effective.repayment.rate.100to150=0;mean.effective.repayment.rate.150to250=0}
    mean.effective.repayment.rate.250to400 <- sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]*target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"])/sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])
  } else {mean.effective.repayment.rate.under100=0;mean.effective.repayment.rate.100to150=0;mean.effective.repayment.rate.150to250=0;mean.effective.repayment.rate.250to400=0}
  mean.effective.repayment.rate.400to600 <- sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]*target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"])/sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])
  mean.effective.repayment.rate.600to800 <- sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]*target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"])/sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])
  mean.effective.repayment.rate.over800 <- sum(target.fams[target.fams$FPL>800,"Payments"]*target.fams[target.fams$FPL>800,"WT"]/target.fams[target.fams$FPL>800,"FamIncome"])/sum(target.fams[target.fams$FPL>800,"WT"])
  
  if(policy!="HYBRID"){
    if(policy!="HYBRID2"){
      mean.copay.as.prop.of.income.under100 <- sum(target.fams[target.fams$FPL<=100,"OOP"]*target.fams[target.fams$FPL<=100,"WT"]/target.fams[target.fams$FPL<=100,"FamIncome"])/sum(target.fams[target.fams$FPL<=100,"WT"])
      mean.copay.as.prop.of.income.100to150 <- sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"OOP"]*target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"])/sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])
      mean.copay.as.prop.of.income.150to250 <- sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"OOP"]*target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"])/sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])
    } else{mean.copay.as.prop.of.income.under100=0;mean.copay.as.prop.of.income.100to150=0;mean.copay.as.prop.of.income.150to250=0}  
    mean.copay.as.prop.of.income.250to400 <- sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"OOP"]*target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"])/sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])
  } else {mean.copay.as.prop.of.income.under100=0;mean.copay.as.prop.of.income.100to150=0;mean.copay.as.prop.of.income.150to250=0;mean.copay.as.prop.of.income.250to400=0}
  mean.copay.as.prop.of.income.400to600 <- sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"OOP"]*target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"])/sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])
  mean.copay.as.prop.of.income.600to800 <- sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"OOP"]*target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"])/sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])
  mean.copay.as.prop.of.income.over800 <- sum(target.fams[target.fams$FPL>800,"OOP"]*target.fams[target.fams$FPL>800,"WT"]/target.fams[target.fams$FPL>800,"FamIncome"])/sum(target.fams[target.fams$FPL>800,"WT"])
  
  if(policy!="HYBRID"){
    if(policy!="HYBRID2"){
      min.effective.repayment.rate.under100 <- min(target.fams[target.fams$FPL<=100,"Payments"]/target.fams[target.fams$FPL<=100,"FamIncome"])
      min.effective.repayment.rate.100to150 <- min(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"])
      min.effective.repayment.rate.150to250 <- min(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"])
      max.effective.repayment.rate.under100 <- max(target.fams[target.fams$FPL<=100,"Payments"]/target.fams[target.fams$FPL<=100,"FamIncome"])
      max.effective.repayment.rate.100to150 <- max(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"])
      max.effective.repayment.rate.150to250 <- max(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"])
      q25.effective.repayment.rate.under100 <- wtd.quantile(unlist(target.fams[target.fams$FPL<=100,"Payments"]/target.fams[target.fams$FPL<=100,"FamIncome"]),0.25,weight=unlist(target.fams[target.fams$FPL<=100,"WT"]))[[1]]
      q25.effective.repayment.rate.100to150 <- wtd.quantile(unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"]),0.25,weight=unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))[[1]]
      q25.effective.repayment.rate.150to250 <- wtd.quantile(unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"]),0.25,weight=unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))[[1]]
      q50.effective.repayment.rate.under100 <- wtd.quantile(unlist(target.fams[target.fams$FPL<=100,"Payments"]/target.fams[target.fams$FPL<=100,"FamIncome"]),0.50,weight=unlist(target.fams[target.fams$FPL<=100,"WT"]))[[1]]
      q50.effective.repayment.rate.100to150 <- wtd.quantile(unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"]),0.50,weight=unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))[[1]]
      q50.effective.repayment.rate.150to250 <- wtd.quantile(unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"]),0.50,weight=unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))[[1]]
      q75.effective.repayment.rate.under100 <- wtd.quantile(unlist(target.fams[target.fams$FPL<=100,"Payments"]/target.fams[target.fams$FPL<=100,"FamIncome"]),0.75,weight=unlist(target.fams[target.fams$FPL<=100,"WT"]))[[1]]
      q75.effective.repayment.rate.100to150 <- wtd.quantile(unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"]),0.75,weight=unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))[[1]]
      q75.effective.repayment.rate.150to250 <- wtd.quantile(unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"]),0.75,weight=unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))[[1]]
      q90.effective.repayment.rate.under100 <- wtd.quantile(unlist(target.fams[target.fams$FPL<=100,"Payments"]/target.fams[target.fams$FPL<=100,"FamIncome"]),0.90,weight=unlist(target.fams[target.fams$FPL<=100,"WT"]))[[1]]
      q90.effective.repayment.rate.100to150 <- wtd.quantile(unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"]),0.90,weight=unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))[[1]]
      q90.effective.repayment.rate.150to250 <- wtd.quantile(unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"]),0.90,weight=unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))[[1]]
      q95.effective.repayment.rate.under100 <- wtd.quantile(unlist(target.fams[target.fams$FPL<=100,"Payments"]/target.fams[target.fams$FPL<=100,"FamIncome"]),0.95,weight=unlist(target.fams[target.fams$FPL<=100,"WT"]))[[1]]
      q95.effective.repayment.rate.100to150 <- wtd.quantile(unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"]),0.95,weight=unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))[[1]]
      q95.effective.repayment.rate.150to250 <- wtd.quantile(unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"]),0.95,weight=unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))[[1]]
      q99.effective.repayment.rate.under100 <- wtd.quantile(unlist(target.fams[target.fams$FPL<=100,"Payments"]/target.fams[target.fams$FPL<=100,"FamIncome"]),0.99,weight=unlist(target.fams[target.fams$FPL<=100,"WT"]))[[1]]
      q99.effective.repayment.rate.100to150 <- wtd.quantile(unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"]),0.99,weight=unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))[[1]]
      q99.effective.repayment.rate.150to250 <- wtd.quantile(unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"]),0.99,weight=unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))[[1]]
      
    } else {
      min.effective.repayment.rate.under100 <- 0; min.effective.repayment.rate.100to150 <- 0; min.effective.repayment.rate.150to250 <- 0;
      max.effective.repayment.rate.under100 <- 0; max.effective.repayment.rate.100to150 <- 0; max.effective.repayment.rate.150to250 <- 0;
      q25.effective.repayment.rate.under100 <- 0; q25.effective.repayment.rate.100to150 <- 0; q25.effective.repayment.rate.150to250 <- 0;
      q50.effective.repayment.rate.under100 <- 0; q50.effective.repayment.rate.100to150 <- 0; q50.effective.repayment.rate.150to250 <- 0; 
      q75.effective.repayment.rate.under100 <- 0; q75.effective.repayment.rate.100to150 <- 0; q75.effective.repayment.rate.150to250 <- 0; 
      q90.effective.repayment.rate.under100 <- 0; q90.effective.repayment.rate.100to150 <- 0; q90.effective.repayment.rate.150to250 <- 0; 
      q95.effective.repayment.rate.under100 <- 0; q95.effective.repayment.rate.100to150 <- 0; q95.effective.repayment.rate.150to250 <- 0; 
      q99.effective.repayment.rate.under100 <- 0; q99.effective.repayment.rate.100to150 <- 0; q99.effective.repayment.rate.150to250 <- 0; 
    }
    min.effective.repayment.rate.250to400 <- min(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"])
    q25.effective.repayment.rate.250to400 <- wtd.quantile(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.25,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
    q50.effective.repayment.rate.250to400 <- wtd.quantile(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.50,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
    q75.effective.repayment.rate.250to400 <- wtd.quantile(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.75,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
    q90.effective.repayment.rate.250to400 <- wtd.quantile(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.90,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
    q95.effective.repayment.rate.250to400 <- wtd.quantile(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.95,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
    q99.effective.repayment.rate.250to400 <- wtd.quantile(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.99,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
    max.effective.repayment.rate.250to400 <- max(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"])
  } else{
    min.effective.repayment.rate.under100 <- 0; min.effective.repayment.rate.100to150 <- 0; min.effective.repayment.rate.150to250 <- 0; min.effective.repayment.rate.250to400 <- 0;
    max.effective.repayment.rate.under100 <- 0; max.effective.repayment.rate.100to150 <- 0; max.effective.repayment.rate.150to250 <- 0; max.effective.repayment.rate.250to400 <- 0;
    q25.effective.repayment.rate.under100 <- 0; q25.effective.repayment.rate.100to150 <- 0; q25.effective.repayment.rate.150to250 <- 0; q25.effective.repayment.rate.250to400 <- 0;
    q50.effective.repayment.rate.under100 <- 0; q50.effective.repayment.rate.100to150 <- 0; q50.effective.repayment.rate.150to250 <- 0; q50.effective.repayment.rate.250to400 <- 0;
    q75.effective.repayment.rate.under100 <- 0; q75.effective.repayment.rate.100to150 <- 0; q75.effective.repayment.rate.150to250 <- 0; q75.effective.repayment.rate.250to400 <- 0;
    q90.effective.repayment.rate.under100 <- 0; q90.effective.repayment.rate.100to150 <- 0; q90.effective.repayment.rate.150to250 <- 0; q90.effective.repayment.rate.250to400 <- 0;
    q95.effective.repayment.rate.under100 <- 0; q95.effective.repayment.rate.100to150 <- 0; q95.effective.repayment.rate.150to250 <- 0; q95.effective.repayment.rate.250to400 <- 0;
    q99.effective.repayment.rate.under100 <- 0; q99.effective.repayment.rate.100to150 <- 0; q99.effective.repayment.rate.150to250 <- 0; q99.effective.repayment.rate.250to400 <- 0;
  }
  min.effective.repayment.rate.400to600 <- min(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"])
  q25.effective.repayment.rate.400to600 <- wtd.quantile(unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"]),0.25,weight=unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))[[1]]
  q50.effective.repayment.rate.400to600 <- wtd.quantile(unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"]),0.50,weight=unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))[[1]]
  q75.effective.repayment.rate.400to600 <- wtd.quantile(unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"]),0.75,weight=unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))[[1]]
  q90.effective.repayment.rate.400to600 <- wtd.quantile(unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"]),0.90,weight=unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))[[1]]
  q95.effective.repayment.rate.400to600 <- wtd.quantile(unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"]),0.95,weight=unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))[[1]]
  q99.effective.repayment.rate.400to600 <- wtd.quantile(unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"]),0.99,weight=unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))[[1]]
  max.effective.repayment.rate.400to600 <- max(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"])
  min.effective.repayment.rate.600to800 <- min(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"])
  q25.effective.repayment.rate.600to800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"]),0.25,weight=unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))[[1]]
  q50.effective.repayment.rate.600to800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"]),0.50,weight=unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))[[1]]
  q75.effective.repayment.rate.600to800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"]),0.75,weight=unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))[[1]]
  q90.effective.repayment.rate.600to800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"]),0.90,weight=unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))[[1]]
  q95.effective.repayment.rate.600to800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"]),0.95,weight=unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))[[1]]
  q99.effective.repayment.rate.600to800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"]),0.99,weight=unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))[[1]]
  max.effective.repayment.rate.600to800 <- max(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"])
  min.effective.repayment.rate.over800 <- min(target.fams[target.fams$FPL>800,"Payments"]/target.fams[target.fams$FPL>800,"FamIncome"])
  q25.effective.repayment.rate.over800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>800,"Payments"]/target.fams[target.fams$FPL>800,"FamIncome"]),0.25,weight=unlist(target.fams[target.fams$FPL>800,"WT"]))[[1]]
  q50.effective.repayment.rate.over800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>800,"Payments"]/target.fams[target.fams$FPL>800,"FamIncome"]),0.50,weight=unlist(target.fams[target.fams$FPL>800,"WT"]))[[1]]
  q75.effective.repayment.rate.over800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>800,"Payments"]/target.fams[target.fams$FPL>800,"FamIncome"]),0.75,weight=unlist(target.fams[target.fams$FPL>800,"WT"]))[[1]]
  q90.effective.repayment.rate.over800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>800,"Payments"]/target.fams[target.fams$FPL>800,"FamIncome"]),0.90,weight=unlist(target.fams[target.fams$FPL>800,"WT"]))[[1]]
  q95.effective.repayment.rate.over800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>800,"Payments"]/target.fams[target.fams$FPL>800,"FamIncome"]),0.95,weight=unlist(target.fams[target.fams$FPL>800,"WT"]))[[1]]
  q99.effective.repayment.rate.over800 <- wtd.quantile(unlist(target.fams[target.fams$FPL>800,"Payments"]/target.fams[target.fams$FPL>800,"FamIncome"]),0.99,weight=unlist(target.fams[target.fams$FPL>800,"WT"]))[[1]]
  max.effective.repayment.rate.over800 <- max(target.fams[target.fams$FPL>800,"Payments"]/target.fams[target.fams$FPL>800,"FamIncome"])
  
  
  
  # Consider changing the below to total amount ever borrowed
  mean.amount.borrowed <- sum(target[,"Borrow"]*target[,"WT"])/sum(target[,"WT"])
  mean.amount.borrowed.nonzero <- sum(target[target$Borrow>0,"Borrow"]*target[target$Borrow>0,"WT"])/sum(target[target$Borrow>0,"WT"])
  sd.borrowed <- 0
  sd.borrowed.nonzero <- 0
  mean.amount.paid <- sum(results[,"Repayment.thisyear"]*results[,"WT"])/sum(results[,"WT"])
  mean.amount.paid.nonzero <- sum(results[results$Repayment.thisyear>0,"Repayment.thisyear"]*results[results$Repayment.thisyear>0,"WT"])/sum(results[results$Repayment.thisyear>0,"WT"])
  sd.paid <- 0
  sd.paid.nonzero <- 0
  mean.copay <- sum(target[,"OOP"]*target[,"WT"])/sum(target[,"WT"])
  mean.copay.nonzero <- sum(target[target$OOP>0,"OOP"]*target[target$OOP>0,"WT"])/sum(target[target$OOP>0,"WT"])
  sd.copay <- 0
  sd.copay.nonzero <- 0
  
  Loans.Outstanding <- sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  Ever.Borrowed <- sum(results[,"Ever.Borrowed"]*results[,"WT"])
  Ever.Forgiven <- sum(results[,"Ever.Forgiven"]*results[,"WT"])
  Tot.Interest <- sum(results[,"Tot.Interest"]*results[,"WT"])
  Ever.Repaid <- sum(results[,"Ever.Repaid"]*results[,"WT"])
  To.Parents <- sum(results[,"To.Parents"]*results[,"WT"])
  From.Children <- sum(results[,"From.Children"]*results[,"WT"])
  
  
  tot.forgiven.expired <- sum(results[,"Forgiven.expire"]*results[,"WT"])
  tot.forgiven.age65 <- sum(results[,"Forgiven.65"]*results[,"WT"])
  tot.forgiven.death <- sum(results[,"Forgiven.death"]*results[,"WT"])
  tot.forgiven.age26 <-  sum(results[,"Forgiven.26"]*results[,"WT"])
  mean.forgiven.expired <- sum(results[,"Forgiven.expire"]*results[,"WT"])/sum(results[,"WT"])
  mean.forgiven.age65 <- sum(results[,"Forgiven.65"]*results[,"WT"])/sum(results[,"WT"])
  mean.forgiven.death <- sum(results[,"Forgiven.death"]*results[,"WT"])/sum(results[,"WT"])
  mean.forgiven.age26 <-  sum(results[,"Forgiven.26"]*results[,"WT"])/sum(results[,"WT"])
  
  prop.copays.female <- sum(target[target$Sex=="female","OOP"]*target[target$Sex=="female","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.male <- sum(target[target$Sex=="male","OOP"]*target[target$Sex=="male","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.white <- sum(target[target$Race=="white","OOP"]*target[target$Race=="white","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.black <- sum(target[target$Race=="black","OOP"]*target[target$Race=="black","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.hispanic <- sum(target[target$Race=="hispanic","OOP"]*target[target$Race=="hispanic","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.otherrace <- sum(target[target$Race=="other","OOP"]*target[target$Race=="other","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.under19 <- sum(target[target$AgeGroup=="<19","OOP"]*target[target$AgeGroup=="<19","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.19to34 <- sum(target[target$AgeGroup=="19-34","OOP"]*target[target$AgeGroup=="19-34","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.35to49 <- sum(target[target$AgeGroup=="35-49","OOP"]*target[target$AgeGroup=="35-49","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.50to64 <- sum(target[target$AgeGroup=="50-64","OOP"]*target[target$AgeGroup=="50-64","WT"])/sum(target[,"OOP"]*target[,"WT"])
  #prop.copays.65plus <- sum(target[target$AgeGroup=="65+","OOP"]*target[target$AgeGroup=="65+","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.under100 <- sum(target[target$FPL<=100,"OOP"]*target[target$FPL<=100,"WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.100to150 <- sum(target[target$FPL>100 & target$FPL<=150,"OOP"]*target[target$FPL>100 & target$FPL<=150,"WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.150to250 <- sum(target[target$FPL>150 & target$FPL<=250,"OOP"]*target[target$FPL>150 & target$FPL<=250,"WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.250to400 <- sum(target[target$FPL>250 & target$FPL<=400,"OOP"]*target[target$FPL>250 & target$FPL<=400,"WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.400to600 <- sum(target[target$FPL>400 & target$FPL<=600,"OOP"]*target[target$FPL>400 & target$FPL<=600,"WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.600to800 <- sum(target[target$FPL>600 & target$FPL<=800,"OOP"]*target[target$FPL>600 & target$FPL<=800,"WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.over800 <- sum(target[target$FPL>800,"OOP"]*target[target$FPL>800,"WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.under400 <- sum(target[target$FPL<=400,"OOP"]*target[target$FPL<=400,"WT"])/sum(target[,"OOP"]*target[,"WT"])
  
  prop.copays.under100.ghealth <- sum(target[target$FPL<=100 & target$HS=="good","OOP"]*target[target$FPL<=100 & target$HS=="good","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.100to150.ghealth <- sum(target[target$FPL>100 & target$FPL<=150 & target$HS=="good","OOP"]*target[target$FPL>100 & target$FPL<=150 & target$HS=="good","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.150to250.ghealth <- sum(target[target$FPL>150 & target$FPL<=250 & target$HS=="good","OOP"]*target[target$FPL>150 & target$FPL<=250 & target$HS=="good","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.250to400.ghealth <- sum(target[target$FPL>250 & target$FPL<=400 & target$HS=="good","OOP"]*target[target$FPL>250 & target$FPL<=400 & target$HS=="good","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.400to600.ghealth <- sum(target[target$FPL>400 & target$FPL<=600 & target$HS=="good","OOP"]*target[target$FPL>400 & target$FPL<=600 & target$HS=="good","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.600to800.ghealth <- sum(target[target$FPL>600 & target$FPL<=800 & target$HS=="good","OOP"]*target[target$FPL>600 & target$FPL<=800 & target$HS=="good","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.over800.ghealth <- sum(target[target$FPL>800 & target$HS=="good","OOP"]*target[target$FPL>800 & target$HS=="good","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.under400.ghealth <- sum(target[target$FPL<=400 & target$HS=="good","OOP"]*target[target$FPL<=400 & target$HS=="good","WT"])/sum(target[,"OOP"]*target[,"WT"])
  
  prop.copays.under100.bhealth <- sum(target[target$FPL<=100 & target$HS=="bad","OOP"]*target[target$FPL<=100 & target$HS=="bad","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.100to150.bhealth <- sum(target[target$FPL>100 & target$FPL<=150 & target$HS=="bad","OOP"]*target[target$FPL>100 & target$FPL<=150 & target$HS=="bad","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.150to250.bhealth <- sum(target[target$FPL>150 & target$FPL<=250 & target$HS=="bad","OOP"]*target[target$FPL>150 & target$FPL<=250 & target$HS=="bad","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.250to400.bhealth <- sum(target[target$FPL>250 & target$FPL<=400 & target$HS=="bad","OOP"]*target[target$FPL>250 & target$FPL<=400 & target$HS=="bad","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.400to600.bhealth <- sum(target[target$FPL>400 & target$FPL<=600 & target$HS=="bad","OOP"]*target[target$FPL>400 & target$FPL<=600 & target$HS=="bad","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.600to800.bhealth <- sum(target[target$FPL>600 & target$FPL<=800 & target$HS=="bad","OOP"]*target[target$FPL>600 & target$FPL<=800 & target$HS=="bad","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.over800.bhealth <- sum(target[target$FPL>800 & target$HS=="bad","OOP"]*target[target$FPL>800 & target$HS=="bad","WT"])/sum(target[,"OOP"]*target[,"WT"])
  prop.copays.under400.bhealth <- sum(target[target$FPL<=400 & target$HS=="bad","OOP"]*target[target$FPL<=400 & target$HS=="bad","WT"])/sum(target[,"OOP"]*target[,"WT"])
  
  
  
  prop.debt.female <- sum(results[results$Sex=="female","Loans.totalthisyear"]*results[results$Sex=="female","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.male <- sum(results[results$Sex=="male","Loans.totalthisyear"]*results[results$Sex=="male","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.white <- sum(results[results$Race=="white","Loans.totalthisyear"]*results[results$Race=="white","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.black <- sum(results[results$Race=="black","Loans.totalthisyear"]*results[results$Race=="black","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.hispanic <- sum(results[results$Race=="hispanic","Loans.totalthisyear"]*results[results$Race=="hispanic","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.otherrace <- sum(results[results$Race=="other","Loans.totalthisyear"]*results[results$Race=="other","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.under19 <- sum(results[results$AgeGroup=="<19","Loans.totalthisyear"]*results[results$AgeGroup=="<19","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.19to34 <- sum(results[results$AgeGroup=="19-34","Loans.totalthisyear"]*results[results$AgeGroup=="19-34","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.35to49 <- sum(results[results$AgeGroup=="35-49","Loans.totalthisyear"]*results[results$AgeGroup=="35-49","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.50to64 <- sum(results[results$AgeGroup=="50-64","Loans.totalthisyear"]*results[results$AgeGroup=="50-64","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  #prop.debt.65plus <- sum(results[results$AgeGroup=="65+","Loans.totalthisyear"]*results[results$AgeGroup=="65+","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.under100 <- sum(results[results$FPL<=100,"Loans.totalthisyear"]*results[results$FPL<=100,"WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.100to150 <- sum(results[results$FPL>100 & results$FPL<=150,"Loans.totalthisyear"]*results[results$FPL>100 & results$FPL<=150,"WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.150to250 <- sum(results[results$FPL>150 & results$FPL<=250,"Loans.totalthisyear"]*results[results$FPL>150 & results$FPL<=250,"WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.250to400 <- sum(results[results$FPL>250 & results$FPL<=400,"Loans.totalthisyear"]*results[results$FPL>250 & results$FPL<=400,"WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.400to600 <- sum(results[results$FPL>400 & results$FPL<=600,"Loans.totalthisyear"]*results[results$FPL>400 & results$FPL<=600,"WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.600to800 <- sum(results[results$FPL>600 & results$FPL<=800,"Loans.totalthisyear"]*results[results$FPL>600 & results$FPL<=800,"WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.over800 <- sum(results[results$FPL>800,"Loans.totalthisyear"]*results[results$FPL>800,"WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.under400 <- sum(results[results$FPL<=400,"Loans.totalthisyear"]*results[results$FPL<=400,"WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  
  prop.debt.under100.ghealth <- sum(results[results$FPL<=100 & results$HS=="good","Loans.totalthisyear"]*results[results$FPL<=100 & results$HS=="good","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.100to150.ghealth <- sum(results[results$FPL>100 & results$FPL<=150 & results$HS=="good","Loans.totalthisyear"]*results[results$FPL>100 & results$FPL<=150 & results$HS=="good","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.150to250.ghealth <- sum(results[results$FPL>150 & results$FPL<=250 & results$HS=="good","Loans.totalthisyear"]*results[results$FPL>150 & results$FPL<=250 & results$HS=="good","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.250to400.ghealth <- sum(results[results$FPL>250 & results$FPL<=400 & results$HS=="good","Loans.totalthisyear"]*results[results$FPL>250 & results$FPL<=400 & results$HS=="good","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.400to600.ghealth <- sum(results[results$FPL>400 & results$FPL<=600 & results$HS=="good","Loans.totalthisyear"]*results[results$FPL>400 & results$FPL<=600 & results$HS=="good","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.600to800.ghealth <- sum(results[results$FPL>600 & results$FPL<=800 & results$HS=="good","Loans.totalthisyear"]*results[results$FPL>600 & results$FPL<=800 & results$HS=="good","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.over800.ghealth <- sum(results[results$FPL>800 & results$HS=="good","Loans.totalthisyear"]*results[results$FPL>800 & results$HS=="good","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.under400.ghealth <- sum(results[results$FPL<=400 & results$HS=="good","Loans.totalthisyear"]*results[results$FPL<=400 & results$HS=="good","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  
  prop.debt.under100.bhealth <- sum(results[results$FPL<=100 & results$HS=="bad","Loans.totalthisyear"]*results[results$FPL<=100 & results$HS=="bad","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.100to150.bhealth <- sum(results[results$FPL>100 & results$FPL<=150 & results$HS=="bad","Loans.totalthisyear"]*results[results$FPL>100 & results$FPL<=150 & results$HS=="bad","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.150to250.bhealth <- sum(results[results$FPL>150 & results$FPL<=250 & results$HS=="bad","Loans.totalthisyear"]*results[results$FPL>150 & results$FPL<=250 & results$HS=="bad","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.250to400.bhealth <- sum(results[results$FPL>250 & results$FPL<=400 & results$HS=="bad","Loans.totalthisyear"]*results[results$FPL>250 & results$FPL<=400 & results$HS=="bad","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.400to600.bhealth <- sum(results[results$FPL>400 & results$FPL<=600 & results$HS=="bad","Loans.totalthisyear"]*results[results$FPL>400 & results$FPL<=600 & results$HS=="bad","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.600to800.bhealth <- sum(results[results$FPL>600 & results$FPL<=800 & results$HS=="bad","Loans.totalthisyear"]*results[results$FPL>600 & results$FPL<=800 & results$HS=="bad","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.over800.bhealth <- sum(results[results$FPL>800 & results$HS=="bad","Loans.totalthisyear"]*results[results$FPL>800 & results$HS=="bad","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  prop.debt.under400.bhealth <- sum(results[results$FPL<=400 & results$HS=="bad","Loans.totalthisyear"]*results[results$FPL<=400 & results$HS=="bad","WT"])/sum(results[,"Loans.totalthisyear"]*results[,"WT"])
  
  if(policy!="HYBRID"){
    if(policy!="HYBRID2"){
      prop.target.fams.paying.cap.under100 <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0 & target.fams$FPL<=100,"WT"]))/sum(target.fams[target.fams$FPL<=100,"WT"])
      prop.target.fams.paying.cap.100to150 <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0 & target.fams$FPL<=150 & target.fams$FPL>100,"WT"]))/sum(target.fams[target.fams$FPL<=150 & target.fams$FPL>100,"WT"])
      prop.target.fams.paying.cap.150to250 <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0 & target.fams$FPL<=250 & target.fams$FPL>150,"WT"]))/sum(target.fams[target.fams$FPL<=250 & target.fams$FPL>150,"WT"])
    } else{prop.target.fams.paying.cap.under100=0;prop.target.fams.paying.cap.100to150=0;prop.target.fams.paying.cap.150to250=0}
    prop.target.fams.paying.cap.250to400 <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0 & target.fams$FPL<=400 & target.fams$FPL>250,"WT"]))/sum(target.fams[target.fams$FPL<=400 & target.fams$FPL>250,"WT"])
  } else {prop.target.fams.paying.cap.under100=0;prop.target.fams.paying.cap.100to150=0;prop.target.fams.paying.cap.150to250=0;prop.target.fams.paying.cap.250to400=0}
  prop.target.fams.paying.cap.400to600 <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0 & target.fams$FPL<=600 & target.fams$FPL>400,"WT"]))/sum(target.fams[target.fams$FPL<=600 & target.fams$FPL>400,"WT"])
  prop.target.fams.paying.cap.600to800 <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0 & target.fams$FPL<=800 & target.fams$FPL>600,"WT"]))/sum(target.fams[target.fams$FPL<=800 & target.fams$FPL>600,"WT"])
  prop.target.fams.paying.cap.over800 <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0 & target.fams$FPL>800,"WT"]))/sum(target.fams[target.fams$FPL>800,"WT"])
  
  if(policy!="HYBRID"){
    if(policy!="HYBRID2"){
      mean.total.payment.under100 <- sum((target.fams[target.fams$FPL<=100,"Payments"]+target.fams[target.fams$FPL<=100,"OOP"])*target.fams[target.fams$FPL<=100,"WT"]/target.fams[target.fams$FPL<=100,"FamIncome"])/sum(target.fams[target.fams$FPL<=100,"WT"])
      mean.total.payment.100to150 <- sum((target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]+target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"OOP"])*target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"])/sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])
      mean.total.payment.150to250 <- sum((target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]+target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"OOP"])*target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"])/sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])
      mean.N.on.10P.under100 <- sum(target.fams[target.fams$FPL<=100,"Elig"]*target.fams[target.fams$FPL<=100,"WT"])/sum(target.fams[target.fams$FPL<=100,"WT"])
      mean.N.on.10P.100to150 <- sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Elig"]*target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])/sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])
      mean.N.on.10P.150to250 <- sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Elig"]*target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])/sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])
      mean.N.visits.under100 <- sum(target.fams[target.fams$FPL<=100,"Visits"]*target.fams[target.fams$FPL<=100,"WT"])/sum(target.fams[target.fams$FPL<=100,"WT"])
      mean.N.visits.100to150 <- sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Visits"]*target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])/sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])
      mean.N.visits.150to250 <- sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Visits"]*target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])/sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])
      mean.repayment.under100 <- sum(target.fams[target.fams$FPL<=100,"Payments"]*target.fams[target.fams$FPL<=100,"WT"])/sum(target.fams[target.fams$FPL<=100,"WT"])
      mean.repayment.100to150 <- sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]*target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])/sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])
      mean.repayment.150to250 <- sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]*target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])/sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])
      mean.copay.under100 <- sum(target.fams[target.fams$FPL<=100,"OOP"]*target.fams[target.fams$FPL<=100,"WT"])/sum(target.fams[target.fams$FPL<=100,"WT"])
      mean.copay.100to150 <- sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"OOP"]*target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])/sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])
      mean.copay.150to250 <- sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"OOP"]*target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])/sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])
      median.N.visits.under100 <- weighted.median(unlist(target.fams[target.fams$FPL<=100,"Visits"]),unlist(target.fams[target.fams$FPL<=100,"WT"]))
      median.N.visits.100to150 <- weighted.median(unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Visits"]),unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))
      median.N.visits.150to250 <- weighted.median(unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Visits"]),unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))
      median.copay.under100 <- weighted.median(unlist(target.fams[target.fams$FPL<=100,"OOP"]),unlist(target.fams[target.fams$FPL<=100,"WT"]))
      median.copay.100to150 <- weighted.median(unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"OOP"]),unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))
      median.copay.150to250 <- weighted.median(unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"OOP"]),unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))

    } else{
      mean.total.payment.under100=0;mean.total.payment.100to150=0;mean.total.payment.150to250=0
      mean.N.on.10P.under100=0; mean.N.on.10P.100to150=0; mean.N.on.10P.150to250=0
      mean.N.visits.under100=0; mean.N.visits.100to150=0; mean.N.visits.150to250=0
      median.N.visits.under100=0; median.N.visits.100to150=0; median.N.visits.150to250=0
      mean.repayment.under100=0; mean.repayment.100to150=0; mean.repayment.150to250=0
      mean.copay.under100=0; mean.copay.100to150=0; mean.copay.150to250=0
      median.copay.under100=0; median.copay.100to150=0; median.copay.150to250=0
    }
    mean.total.payment.250to400 <- sum((target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]+target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"OOP"])*target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"])/sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])
    mean.N.on.10P.250to400 <- sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Elig"]*target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])/sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])
    mean.N.visits.250to400 <- sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Visits"]*target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])/sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])
    median.N.visits.250to400 <- weighted.median(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Visits"]),unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))
    mean.repayment.250to400 <- sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]*target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])/sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])
    mean.copay.250to400 <- sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"OOP"]*target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])/sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])
    median.copay.250to400 <- weighted.median(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"OOP"]),unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))
  } else {
    mean.repayment.under100=0; mean.copay.under100=0; mean.N.visits.under100=0; median.copay.under100=0; median.N.visits.under100=0; mean.N.on.10P.under100=0; 
    mean.repayment.150to250=0; mean.copay.150to250=0; mean.N.visits.150to250=0; median.copay.150to250=0; median.N.visits.150to250=0; mean.N.on.10P.150to250=0; 
    mean.repayment.100to150=0; mean.copay.100to150=0; mean.N.visits.100to150=0; median.copay.100to150=0; median.N.visits.100to150=0; mean.N.on.10P.100to150=0; 
    mean.repayment.250to400=0; mean.copay.250to400=0; mean.N.visits.250to400=0; median.copay.250to400=0; median.N.visits.250to400=0; mean.N.on.10P.250to400=0; 
    mean.total.payment.under100=0;mean.total.payment.100to150=0;mean.total.payment.150to250=0;mean.total.payment.250to400=0
    }
  mean.total.payment.400to600 <- sum((target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]+target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"OOP"])*target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"])/sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])
  mean.total.payment.600to800 <- sum((target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]+target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"OOP"])*target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"])/sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])
  mean.total.payment.over800 <- sum((target.fams[target.fams$FPL>800,"Payments"]+target.fams[target.fams$FPL>800,"OOP"])*target.fams[target.fams$FPL>800,"WT"]/target.fams[target.fams$FPL>800,"FamIncome"])/sum(target.fams[target.fams$FPL>800,"WT"])
  mean.N.on.10P.400to600 <- sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Elig"]*target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])/sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])
  mean.N.on.10P.600to800 <- sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Elig"]*target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])/sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])
  mean.N.on.10P.over800 <- sum(target.fams[target.fams$FPL>800,"Elig"]*target.fams[target.fams$FPL>800,"WT"])/sum(target.fams[target.fams$FPL>800,"WT"])
  mean.N.visits.400to600 <- sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Visits"]*target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])/sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])
  mean.N.visits.600to800 <- sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Visits"]*target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])/sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])
  mean.N.visits.over800 <- sum(target.fams[target.fams$FPL>800,"Visits"]*target.fams[target.fams$FPL>800,"WT"])/sum(target.fams[target.fams$FPL>800,"WT"])
  median.N.visits.400to600 <- weighted.median(unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Visits"]),unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))
  median.N.visits.600to800 <- weighted.median(unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Visits"]),unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))
  median.N.visits.over800 <- weighted.median(unlist(target.fams[target.fams$FPL>800,"Visits"]),unlist(target.fams[target.fams$FPL>800,"WT"]))
  mean.repayment.400to600 <- sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]*target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])/sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])
  mean.repayment.600to800 <- sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]*target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])/sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])
  mean.repayment.over800 <- sum(target.fams[target.fams$FPL>800,"Payments"]*target.fams[target.fams$FPL>800,"WT"])/sum(target.fams[target.fams$FPL>800,"WT"])
  mean.copay.400to600 <- sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"OOP"]*target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])/sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])
  mean.copay.600to800 <- sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"OOP"]*target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])/sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])
  mean.copay.over800 <- sum(target.fams[target.fams$FPL>800,"OOP"]*target.fams[target.fams$FPL>800,"WT"])/sum(target.fams[target.fams$FPL>800,"WT"])
  median.copay.400to600 <- weighted.median(unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"OOP"]),unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))
  median.copay.600to800 <- weighted.median(unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"OOP"]),unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))
  median.copay.over800 <- weighted.median(unlist(target.fams[target.fams$FPL>800,"OOP"]),unlist(target.fams[target.fams$FPL>800,"WT"]))
  
  if(policy!="HYBRID"){
    if(policy!="HYBRID2"){
      median.total.payment.under100 <- wtd.quantile(unlist((target.fams[target.fams$FPL<=100,"Payments"]+target.fams[target.fams$FPL<=100,"OOP"])/target.fams[target.fams$FPL<=100,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL<=100,"WT"]))[[1]]
      median.total.payment.100to150 <- wtd.quantile(unlist((target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]+target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"OOP"])/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))[[1]]
      median.total.payment.150to250 <- wtd.quantile(unlist((target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]+target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"OOP"])/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))[[1]]
    } else{median.total.payment.under100=0;median.total.payment.100to150=0;median.total.payment.150to250=0}
    median.total.payment.250to400 <- wtd.quantile(unlist((target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]+target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"OOP"])/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
  } else {median.total.payment.under100=0;median.total.payment.100to150=0;median.total.payment.150to250=0;median.total.payment.250to400=0}
  median.total.payment.400to600 <- wtd.quantile(unlist((target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]+target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"OOP"])/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))[[1]]
  median.total.payment.600to800 <- wtd.quantile(unlist((target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]+target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"OOP"])/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))[[1]]
  median.total.payment.over800 <- wtd.quantile(unlist((target.fams[target.fams$FPL>800,"Payments"]+target.fams[target.fams$FPL>800,"OOP"])/target.fams[target.fams$FPL>800,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>800,"WT"]))[[1]]
  
  table <- t(data.frame(tot.spending.10P,tot.spending.under.400FPL,tot.spending.hybrid.target,tot.spending.medicaid,tot.pop,
                        tot.pop.10p,tot.pop.10p.unins,tot.pop.10p.ngpriv,tot.pop.under.400FPL,
                        total.borrowed.thisyear,total.borrowed.thisyear.unins,total.borrowed.thisyear.ngpriv,
                        total.interest.thisyear,total.interest.thisyear.unins,total.interest.thisyear.ngpriv,
                        total.premiums.thisyear,total.premiums.thisyear.unins,total.premiums.thisyear.ngpriv,
                        total.forgiven.thisyear,total.copays,
                        total.borrowed.ever,total.interest.ever,total.premiums.ever,
                        prop.10p,prop.Medicaid,prop.OthPublic,prop.OthPrivate,prop.NonGrp,prop.UnIns,prop.under400,prop.target.indiv.with.loans,
                        prop.target.fams.with.loans,prop.target.fams.paying.cap,
                        prop.female,prop.male,prop.white,prop.black,prop.hispanic,prop.otherrace,
                        prop.under19,prop.19to34,prop.35to49,prop.50to64,prop.under100FPL,prop.100to150FPL,prop.150to250FPL,
                        prop.250to400FPL,prop.400to600FPL,prop.600to800FPL,prop.over800FPL,
                        prop.target.female,prop.target.male,prop.target.white,prop.target.black,prop.target.hispanic,prop.target.otherrace,
                        prop.target.under19,prop.target.19to34,prop.target.35to49,prop.target.50to64,prop.target.under100FPL,prop.target.100to150FPL,prop.target.150to250FPL,
                        prop.target.250to400FPL,prop.target.400to600FPL,prop.target.600to800FPL,prop.target.over800FPL,mean.fam.income,
                        mean.effective.repayment.rate.under100,mean.effective.repayment.rate.100to150,mean.effective.repayment.rate.150to250,mean.effective.repayment.rate.250to400,
                        mean.effective.repayment.rate.400to600,mean.effective.repayment.rate.600to800,mean.effective.repayment.rate.over800,
                        mean.copay.as.prop.of.income.under100,mean.copay.as.prop.of.income.100to150,mean.copay.as.prop.of.income.150to250,
                        mean.copay.as.prop.of.income.250to400,mean.copay.as.prop.of.income.400to600,mean.copay.as.prop.of.income.600to800,
                        mean.copay.as.prop.of.income.over800,
                        mean.amount.borrowed,mean.amount.borrowed.nonzero,sd.borrowed,sd.borrowed.nonzero,mean.amount.paid,
                        mean.amount.paid.nonzero,sd.paid,sd.paid.nonzero,mean.copay,mean.copay.nonzero,sd.copay,sd.copay.nonzero,
                        tot.forgiven.expired,tot.forgiven.age65,tot.forgiven.death,tot.forgiven.age26,
                        mean.forgiven.expired,mean.forgiven.age65,mean.forgiven.death,mean.forgiven.age26,
                        prop.copays.female,prop.copays.male,prop.copays.white,
                        prop.copays.black,prop.copays.hispanic,prop.copays.otherrace,prop.copays.under19,prop.copays.19to34,
                        prop.copays.35to49,prop.copays.50to64,prop.copays.under100,prop.copays.100to150,
                        prop.copays.150to250,prop.copays.250to400,prop.copays.400to600,prop.copays.600to800,prop.copays.over800,
                        prop.copays.under400,prop.copays.under100.ghealth,prop.copays.100to150.ghealth,
                        prop.copays.150to250.ghealth,prop.copays.250to400.ghealth,prop.copays.400to600.ghealth,prop.copays.600to800.ghealth,prop.copays.over800.ghealth,
                        prop.copays.under400.ghealth,prop.copays.under100.bhealth,prop.copays.100to150.bhealth,
                        prop.copays.150to250.bhealth,prop.copays.250to400.bhealth,prop.copays.400to600.bhealth,prop.copays.600to800.bhealth,prop.copays.over800.bhealth,
                        prop.copays.under400.bhealth, min.copay.10P,q25.copay.10P, q50.copay.10P, q75.copay.10P, q90.copay.10P, q95.copay.10P, q99.copay.10P, max.copay.10P,
                        prop.debt.female,prop.debt.male,prop.debt.white,prop.debt.black,prop.debt.hispanic,
                        prop.debt.otherrace,prop.debt.under19,prop.debt.19to34,prop.debt.35to49,prop.debt.50to64,
                        prop.debt.under100,prop.debt.100to150,prop.debt.150to250,prop.debt.250to400,prop.debt.400to600,prop.debt.600to800,
                        prop.debt.over800,prop.debt.under400,
                        prop.debt.under100.ghealth,prop.debt.100to150.ghealth,prop.debt.150to250.ghealth,prop.debt.250to400.ghealth,
                        prop.debt.400to600.ghealth,prop.debt.600to800.ghealth,prop.debt.over800.ghealth,prop.debt.under400.ghealth,
                        prop.debt.under100.bhealth,prop.debt.100to150.bhealth,prop.debt.150to250.bhealth,prop.debt.250to400.bhealth,
                        prop.debt.400to600.bhealth,prop.debt.600to800.bhealth,prop.debt.over800.bhealth,prop.debt.under400.bhealth,
                        Loans.Outstanding,Ever.Borrowed,Ever.Forgiven,Tot.Interest,Ever.Repaid,
                        To.Parents,From.Children,mean.spending.10P,median.spending.10P,mean.fam.spending.10P,median.fam.spending.10P,
                        min.fam.spending.10P,q25.fam.spending.10P,q75.fam.spending.10P,max.fam.spending.10P,
                        mean.spending.10P.uninsured, median.spending.10P.uninsured, mean.spending.10P.ngpriv, median.spending.10P.ngpriv,
                        mean.spending.10P.goodhealth, median.spending.10P.goodhealth, mean.spending.10P.badhealth, median.spending.10P.badhealth,
                        mean.spending.10P.acute, median.spending.10P.acute, mean.spending.10P.chronic, median.spending.10P.chronic,
                        mean.copay.all, mean.copay.10P, mean.copay.all.fam, mean.copay.10P.fam,
                        median.copay.all, median.copay.10P, median.copay.all.fam, median.copay.10P.fam,
                        mean.copay.10P.uninsured, median.copay.10P.uninsured, mean.copay.10P.ngpriv, median.copay.10P.ngpriv,
                        mean.repayments.10P.uninsured, median.repayments.10P.uninsured, mean.repayments.10P.ngpriv, median.repayments.10P.ngpriv,
                        mean.copay.10P.goodhealth, median.copay.10P.goodhealth, mean.copay.10P.badhealth, median.copay.10P.badhealth,
                        mean.copay.10P.acute, median.copay.10P.acute, mean.copay.10P.chronic, median.copay.10P.chronic,
                        mean.repayments.10P.goodhealth, median.repayments.10P.goodhealth, mean.repayments.10P.badhealth, median.repayments.10P.badhealth,
                        mean.repayments.10P.acute, median.repayments.10P.acute, mean.repayments.10P.chronic, median.repayments.10P.chronic,
                        min.effective.repayment.rate.under100,min.effective.repayment.rate.100to150,min.effective.repayment.rate.150to250,min.effective.repayment.rate.250to400,
                        min.effective.repayment.rate.400to600,min.effective.repayment.rate.600to800,min.effective.repayment.rate.over800,
                        q25.effective.repayment.rate.under100,q25.effective.repayment.rate.100to150,q25.effective.repayment.rate.150to250,q25.effective.repayment.rate.250to400,
                        q25.effective.repayment.rate.400to600,q25.effective.repayment.rate.600to800,q25.effective.repayment.rate.over800,
                        q50.effective.repayment.rate.under100,q50.effective.repayment.rate.100to150,q50.effective.repayment.rate.150to250,q50.effective.repayment.rate.250to400,
                        q50.effective.repayment.rate.400to600,q50.effective.repayment.rate.600to800,q50.effective.repayment.rate.over800,
                        q75.effective.repayment.rate.under100,q75.effective.repayment.rate.100to150,q75.effective.repayment.rate.150to250,q75.effective.repayment.rate.250to400,
                        q75.effective.repayment.rate.400to600,q75.effective.repayment.rate.600to800,q75.effective.repayment.rate.over800,
                        q90.effective.repayment.rate.under100,q90.effective.repayment.rate.100to150,q90.effective.repayment.rate.150to250,q90.effective.repayment.rate.250to400,
                        q90.effective.repayment.rate.400to600,q90.effective.repayment.rate.600to800,q90.effective.repayment.rate.over800,
                        q95.effective.repayment.rate.under100,q95.effective.repayment.rate.100to150,q95.effective.repayment.rate.150to250,q95.effective.repayment.rate.250to400,
                        q95.effective.repayment.rate.400to600,q95.effective.repayment.rate.600to800,q95.effective.repayment.rate.over800,
                        q99.effective.repayment.rate.under100,q99.effective.repayment.rate.100to150,q99.effective.repayment.rate.150to250,q99.effective.repayment.rate.250to400,
                        q99.effective.repayment.rate.400to600,q99.effective.repayment.rate.600to800,q99.effective.repayment.rate.over800,
                        max.effective.repayment.rate.under100,max.effective.repayment.rate.100to150,max.effective.repayment.rate.150to250,max.effective.repayment.rate.250to400,
                        max.effective.repayment.rate.400to600,max.effective.repayment.rate.600to800,max.effective.repayment.rate.over800,
                        prop.target.fams.paying.cap.under100, prop.target.fams.paying.cap.100to150,
                        prop.target.fams.paying.cap.150to250,prop.target.fams.paying.cap.250to400,prop.target.fams.paying.cap.400to600,
                        prop.target.fams.paying.cap.600to800,prop.target.fams.paying.cap.over800,
                        mean.total.payment.under100,mean.total.payment.100to150,mean.total.payment.150to250,
                        mean.total.payment.250to400,mean.total.payment.400to600,mean.total.payment.600to800,mean.total.payment.over800,
                        median.total.payment.under100,median.total.payment.100to150,median.total.payment.150to250,
                        median.total.payment.250to400,median.total.payment.400to600,median.total.payment.600to800,median.total.payment.over800,
                        total.premiums.thisyear.all,total.premiums.thisyear.all.unins,total.premiums.thisyear.all.ngpriv,total.forgiven.thisyear.all,
                        min.spending.10P.goodhealth, q25.spending.10P.goodhealth, q75.spending.10P.goodhealth, q90.spending.10P.goodhealth,
                        q95.spending.10P.goodhealth, q99.spending.10P.goodhealth, max.spending.10P.goodhealth,
                        min.spending.10P.badhealth, q25.spending.10P.badhealth, q75.spending.10P.badhealth, q90.spending.10P.badhealth,
                        q95.spending.10P.badhealth, q99.spending.10P.badhealth, max.spending.10P.badhealth,
                        mean.N.on.10P.under100,mean.N.on.10P.100to150,mean.N.on.10P.150to250,mean.N.on.10P.250to400,
                        mean.N.on.10P.400to600,mean.N.on.10P.600to800,mean.N.on.10P.over800,
                        mean.N.visits.under100,mean.N.visits.100to150,mean.N.visits.150to250,mean.N.visits.250to400,
                        mean.N.visits.400to600,mean.N.visits.600to800,mean.N.visits.over800,
                        mean.repayment.under100,mean.repayment.100to150,mean.repayment.150to250,mean.repayment.250to400,
                        mean.repayment.400to600,mean.repayment.600to800,mean.repayment.over800,
                        mean.copay.under100,mean.copay.100to150,mean.copay.150to250,mean.copay.250to400,
                        mean.copay.400to600,mean.copay.600to800,mean.copay.over800,
                        median.N.visits.under100,median.N.visits.100to150,median.N.visits.150to250,median.N.visits.250to400,
                        median.N.visits.400to600,median.N.visits.600to800,median.N.visits.over800,
                        median.copay.under100,median.copay.100to150,median.copay.150to250,median.copay.250to400,
                        median.copay.400to600,median.copay.600to800,median.copay.over800,
                        total.visits.unins,total.visits.ngpriv,mean.visits.unins,mean.visits.ngpriv))
  return(table)
}

