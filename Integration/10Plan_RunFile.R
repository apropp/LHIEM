remove(list = ls())
library(tidyverse)
library(purrr)
library(plyr)
library(openxlsx)
library(here)
require(compiler)
library(spatstat)
library(questionr)
enableJIT(3) # This seems to help with runtime
source(here("NoteBooks/Integration/10Plan_Integrated_loanextracted.R"))
inputs <- read.xlsx(here("NoteBooks/Integration/InputOutputFile.xlsx"))#, sheet=1))
OUT <- createWorkbook()
addWorksheet(OUT, "Inputs")
writeData(OUT, sheet = "Inputs", x = inputs)

for (run in 1:nrow(inputs)){
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
  if (policy=="Baseline"){
    prepare.pop(cps) # Run preparation script
    print("Running Baseline Case...")
    years <- run.dynamics(cps) # Run dynamics
    # Don't apply the plan here because we're looking at status quo
  } else if (policy=="HYBRID" | policy=="HYBRID2" | policy=="10Plan"){
    prepare.pop(cps) # Run preparation script
    print("Running Simulation...")
    years <- run.dynamics(cps) # Run dynamics
    if (run==2){
      saveRDS(years, here("NoteBooks/Outputs/10PlanBase.rds"))
    }
    print("Applying Policy...")
    years <- apply.plan(years) # Apply 10Plan or variation thereof
  } else if (policy=="rerun"){
    prepare.pop(cps) # This is really just to re-load the functions using new parameters
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
for (run in c(22,23)){
  print(run)
  cap.scale <- inputs[run,"cap.scale"]; behavioral <- inputs[run,"behavioral"]; which.prices <- inputs[run,"which.prices"]
  twentysix.option <- inputs[run,"twentysix.option"]; interest.rate <- inputs[run,"interest.rate"]; policy <- inputs[run,"policy"]
  med.inflation <- inputs[run,"med.inflation"]; healthstat.dist <- inputs[run,"healthstat.dist"]
  elderly.reaction <- inputs[run,"elderly"]; mortality.test <- inputs[run,"mortality.test"]; unins.inc <- inputs[run,"unins.inc"]
  other.test <- inputs[run,"other.test"]
  prepare.pop(cps)
  
  #dir.create(here::here("NoteBooks/Tables_Plots/Figures",run))
  file.name <- paste("Dyn_model_run_",run,".2020-01-10",".rds",sep="")
  years<-readRDS(here("NoteBooks/Outputs/",file.name))

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
saveWorkbook(OUT, "FinalResults_full_011-2.xlsx", overwrite=TRUE)


