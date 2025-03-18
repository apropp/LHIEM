remove(list = ls())
library(tidyverse)
library(purrr)
library(plyr)
library(openxlsx)
library(here)
require(compiler)
library(spatstat)
library(questionr)
library(Hmisc)
enableJIT(3) # This seems to help with runtime
source(here::here("NoteBooks/Integration/10Plan_Integrated_loanextracted.R"))
source(here::here("Notebooks/Integration/COVID_adjustments.R"))
inputs <- read.xlsx(here::here("NoteBooks/Integration/InputOutputFile.xlsx"))#, sheet=1))
OUT <- createWorkbook()
addWorksheet(OUT, "Inputs")
writeData(OUT, sheet = "Inputs", x = inputs)
level <- 1 # Which COVID modification
# TODO: put this in a for loop to cycle through all the modifications we want to test

#for (run in 1:nrow(inputs)){
for (run in c(2)){
  skip_to_next <- FALSE
  tryCatch({
  # Take input parameters from appropriate columns in excel sheet
  cap.scale <- inputs[run,"cap.scale"]; behavioral <- inputs[run,"behavioral"]; which.prices <- inputs[run,"which.prices"]
  twentysix.option <- inputs[run,"twentysix.option"]; interest.rate <- inputs[run,"interest.rate"]; policy <- inputs[run,"policy"]
  med.inflation <- inputs[run,"med.inflation"]; healthstat.dist <- inputs[run,"healthstat.dist"]
  elderly.reaction <- inputs[run,"elderly"]; mortality.test <- inputs[run,"mortality.test"]; unins.inc <- inputs[run,"unins.inc"]
  other.test <- inputs[run,"other.test"]
  
  file.name <- paste("Dyn_model_run_",run,".",Sys.Date(),".rds",sep="") # can use Sys.time() if needed. 
  CART.name <- paste("NoteBooks/Integration/R_Objects/CART_", which.prices, ".Rda",sep='')
  load(here(CART.name))
  print(paste("Run ",run,":",sep=''))
  
  # Only run what is necessary based on the specifications
  prepare.pop(cps) # Run preparation script
  cps <- COVID.modifications(cps,level) # Adjust for COVID runs
  if (policy=="Baseline"){
    #prepare.pop(cps) # Run preparation script
    print("Running Baseline Case...")
    years <- run.dynamics(cps) # Run dynamics
    # Don't apply the plan here because we're looking at status quo
  } else if (policy=="HYBRID" | policy=="HYBRID2" | policy=="10Plan"){
    #prepare.pop(cps) # Run preparation script
    print("Running Simulation...")
    years <- run.dynamics(cps) # Run dynamics
    if (run==2){
      saveRDS(years, here("NoteBooks/Outputs/10PlanBase.rds"))
    }
    print("Applying Policy...")
    years <- apply.plan(years) # Apply 10Plan or variation thereof
  } else if (policy=="rerun"){
    #prepare.pop(cps) # This is really just to re-load the functions using new parameters
    years <- readRDS(here("NoteBooks/Outputs/10PlanBase.rds"))
    print("Loading standard run & re-applying Policy...")
    years <- apply.plan(years) # Apply 10Plan or variation thereof
  }
  }, error=function(e){cat(paste("ERROR in run :",run,sep=''),conditionMessage(e), "\n"); skip_to_next <- TRUE})
  if(skip_to_next) { next }  
  saveRDS(years, here("NoteBooks/Outputs/",file.name))
  
  # Calculate all results for each year
  for (i in 2:(m.years+1)){
    results <- years[[i]]
    results.vector <- tabulate.results(results)
    if(exists("mytable")){mytable <- cbind(mytable,results.vector)}
    else{mytable=data.frame(results.vector)}
  }
  sheetname <- paste("Run ",run,sep='')
  addWorksheet(OUT, sheetname)
  writeData(OUT, sheet = sheetname, x = mytable, rowNames=TRUE)
  rm(mytable)

}
saveWorkbook(OUT, "FinalResults.xlsx", overwrite=TRUE)
rm(OUT)


# This can be used if we have already run the files & just need to tabulate the results
# It is the same procedure as is included in the loop above
OUT <- createWorkbook()
addWorksheet(OUT, "Inputs")
writeData(OUT, sheet = "Inputs", x = inputs)
for (run in c(2,nrow(inputs))){
  print(run)
  cap.scale <- inputs[run,"cap.scale"]; behavioral <- inputs[run,"behavioral"]; which.prices <- inputs[run,"which.prices"]
  twentysix.option <- inputs[run,"twentysix.option"]; interest.rate <- inputs[run,"interest.rate"]; policy <- inputs[run,"policy"]
  med.inflation <- inputs[run,"med.inflation"]; healthstat.dist <- inputs[run,"healthstat.dist"]
  elderly.reaction <- inputs[run,"elderly"]; mortality.test <- inputs[run,"mortality.test"]; unins.inc <- inputs[run,"unins.inc"]
  other.test <- inputs[run,"other.test"]
  prepare.pop(cps)
  
  #dir.create(here::here("NoteBooks/Tables_Plots/Figures",run))
  file.name <- paste("Test_",run,".rds",sep='')
  years<-readRDS(here("NoteBooks/Outputs",file.name))
  
  for (i in 2:(m.years+1)){
    results <- years[[i]]
    results.vector <- tabulate.results(results)
    if(exists("mytable")){mytable <- cbind(mytable,results.vector)}
    else{mytable=data.frame(results.vector)}
    # Run output functions
    # Export results to correct excel sheet
    # Look up how to edit without overwriting
  }
  #run.sheet <- createSheet(wb, sheetName = paste("Run ",run,sep=''))
  sheetname <- paste("Run ",run,sep='')
  addWorksheet(OUT, sheetname)
  writeData(OUT, sheet = sheetname, x = mytable, rowNames=TRUE)
  rm(mytable)
  #save(years,file=paste("Run_",run,".Rda",sep=''))
}
saveWorkbook(OUT, "Results_Test.xlsx", overwrite=TRUE)



tabulate.results.typo <- function(results){
  target <- results[results$Elig==1,]
  hybrid.target <- results[results$Elig==1 | results$InsCat=="Medicaid",]
  medicaid.pop <- results[results$InsCat=="Medicaid",]
  all.fams <- results %>%
    dplyr::group_by(TID) %>%
    dplyr::summarise(FamIncome=mean(FamIncome),FPL=mean(FPL),MedSpend=sum(MedSpend),Visits=sum(Visits),OOP=sum(OOP),Cap=mean(Cap),Borrow=sum(Borrow),Loans=sum(Loans.totalthisyear),Payments=sum(Repayment.thisyear),Forgiven=sum(Forgiven.thisyear),Elig=sum(Elig),WT=mean(WTH))
  target.fams <- all.fams[all.fams$Elig>=1,]
  
  prop.target.fams.paying.cap <- sum(unlist(target.fams[target.fams$Payments==target.fams$Cap & target.fams$Cap>0,"WT"]))/sum(target.fams[,"WT"])
  
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
    
    q50.effective.repayment.rate.250to400 <- wtd.quantile(unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.50,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
    
  if(policy!="HYBRID"){
    if(policy!="HYBRID2"){
      mean.total.payment.under100 <- sum((target.fams[target.fams$FPL<=100,"Payments"]+target.fams[target.fams$FPL<=100,"OOP"])*target.fams[target.fams$FPL<=100,"WT"]/target.fams[target.fams$FPL<=100,"FamIncome"])/sum(target.fams[target.fams$FPL<=100,"WT"])
      mean.total.payment.100to150 <- sum((target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]+target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"OOP"])*target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"])/sum(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"])
      mean.total.payment.150to250 <- sum((target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]+target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"OOP"])*target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"])/sum(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"])
    } else{mean.total.payment.under100=0;mean.total.payment.100to150=0;mean.total.payment.150to250=0}
    mean.total.payment.250to400 <- sum((target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]+target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"OOP"])*target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"])/sum(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"])
  } else {mean.total.payment.under100=0;mean.total.payment.100to150=0;mean.total.payment.150to250=0;mean.effective.repayment.rate.250to400=0}
  mean.total.payment.400to600 <- sum((target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]+target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"OOP"])*target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"])/sum(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"])
  mean.total.payment.600to800 <- sum((target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]+target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"OOP"])*target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"])/sum(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"])
  mean.total.payment.over800 <- sum((target.fams[target.fams$FPL>800,"Payments"]+target.fams[target.fams$FPL>800,"OOP"])*target.fams[target.fams$FPL>800,"WT"]/target.fams[target.fams$FPL>800,"FamIncome"])/sum(target.fams[target.fams$FPL>800,"WT"])
  
  if(policy!="HYBRID"){
    if(policy!="HYBRID2"){
      median.total.payment.under100 <- wtd.quantile(unlist((target.fams[target.fams$FPL<=100,"Payments"]+target.fams[target.fams$FPL<=100,"OOP"])/target.fams[target.fams$FPL<=100,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL<=100,"WT"]))[[1]]
      median.total.payment.100to150 <- wtd.quantile(unlist((target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"Payments"]+target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"OOP"])/target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>100 & target.fams$FPL<=150,"WT"]))[[1]]
      median.total.payment.150to250 <- wtd.quantile(unlist((target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"Payments"]+target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"OOP"])/target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>150 & target.fams$FPL<=250,"WT"]))[[1]]
    } else{median.total.payment.under100=0;median.total.payment.100to150=0;median.total.payment.150to250=0}
    median.total.payment.250to400 <- wtd.quantile(unlist((target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"Payments"]+target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"OOP"])/target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>250 & target.fams$FPL<=400,"WT"]))[[1]]
  } else {median.total.payment.under100=0;median.total.payment.100to150=0;median.total.payment.150to250=0;median.effective.repayment.rate.250to400=0}
  median.total.payment.400to600 <- wtd.quantile(unlist((target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"Payments"]+target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"OOP"])/target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>400 & target.fams$FPL<=600,"WT"]))[[1]]
  median.total.payment.600to800 <- wtd.quantile(unlist((target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"Payments"]+target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"OOP"])/target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>600 & target.fams$FPL<=800,"WT"]))[[1]]
  median.total.payment.over800 <- wtd.quantile(unlist((target.fams[target.fams$FPL>800,"Payments"]+target.fams[target.fams$FPL>800,"OOP"])/target.fams[target.fams$FPL>800,"FamIncome"]),0.5,weight=unlist(target.fams[target.fams$FPL>800,"WT"]))[[1]]
  
  table <- t(data.frame(prop.target.fams.paying.cap, prop.target.fams.paying.cap.under100, prop.target.fams.paying.cap.100to150,
                        prop.target.fams.paying.cap.150to250,prop.target.fams.paying.cap.250to400,prop.target.fams.paying.cap.400to600,
                        prop.target.fams.paying.cap.600to800,prop.target.fams.paying.cap.over800,
                        mean.total.payment.under100,mean.total.payment.100to150,mean.total.payment.150to250,
                        mean.total.payment.250to400,mean.total.payment.400to600,mean.total.payment.600to800,mean.total.payment.over800,
                        median.total.payment.under100,median.total.payment.100to150,median.total.payment.150to250,
                        median.total.payment.250to400,median.total.payment.400to600,median.total.payment.600to800,median.total.payment.over800))
  
  return(table)
}
