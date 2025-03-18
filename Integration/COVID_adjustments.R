#### A script written by Adrienne Propp in July 2020
#### These functions adjust the 10Plan model to incorporate COVID adjustments

source(here("NoteBooks/Income/CPS_data_COVID.R"))
# How does unemploymment rate translate to # jobs lost?
# https://www.cnbc.com/2020/03/30/coronavirus-job-losses-could-total-47-million-unemployment-rate-of-32percent-fed-says.html
# https://www.clevelandfed.org/en/newsroom-and-events/publications/economic-commentary/2020-economic-commentaries/ec-202009-unemployment-costs-of-covid.aspx
mult <- 12/7.6 # Millions of jobs per % unemployment https://www.bls.gov/news.release/pdf/empsit.pdf

COVID.modifications <- function(cps,level){
  covid.modification.levels <- read.xlsx(here::here("NoteBooks/Integration/COVID_modifications.xlsx"))#, sheet=1))
  
  modify.income.growth <- covid.modification.levels[level,"modify.income.growth"] # TODO
  unemployment <- covid.modification.levels[level,"unemployment"]
  ESI.responsiveness <- covid.modification.levels[level,"ESI.responsiveness"]
  decrease.ESI <- covid.modification.levels[level,"decrease.ESI"]
  increase.Medicaid <- covid.modification.levels[level,"increase.Medicaid"]
  increase.NGPriv <- covid.modification.levels[level,"increase.NGPriv"]
  increase.Uninsured <- covid.modification.levels[level,"increase.Uninsured"]
  
  # Fix insurance status
  cps <- cps[sample(1:nrow(cps)),] # Randomize order
  cps <- cps[order(match(cps$InsCat,"Other Private")),] # Order so those with ESI to the top
  cps$c <- cumsum(cps$ASECWTH) # Take cumulative sum of weights
  cps$InsCat2[cps$c <= increase.Medicaid] <- 3 # Medicaid
  cps$InsCat2[cps$c>increase.Medicaid & cps$c<=(increase.Medicaid+increase.NGPriv)] <- 1 # NonGroup Private
  cps$InsCat2[cps$c>(increase.Medicaid+increase.NGPriv) & cps$c<=(increase.Medicaid+increase.NGPriv+increase.Uninsured)] <- 4 # Uninsured
  cps$InsCat <- factor(cps$InsCat2, labels=c("NonGroup Private","Other Public","Medicaid","Uninsured","Other Private"))

  # Fix income, income trajectories
  # First select those to lose employment
  cps$id_cps <- c(1:nrow(cps)) # Need to establish unique IDs for everyone when assigning who loses job
  working_age_pop <- cps[cps$AGE<65 & cps$AGE>=15 & cps$w0==1,] # Between 15 & 65, and currently working
  working_age_pop <- working_age_pop[sample(1:nrow(working_age_pop)),] # Randomize order
  working_age_pop$c <- cumsum(working_age_pop$ASECWTH) # Take cumulative sum of weights
  #ids.losing.esi <- cps$taxid[cps$c <= decrease.ESI]
  ids.losing.job <- working_age_pop$id_cps[working_age_pop$c <= mult*(unemployment-3.5)*1e6] # Millions of people losing job
  names.to.overwrite <- c(paste("w",0:15,sep=''),paste("t",1:15,sep=''),"taxable") # these are the vars that need to be replaced
  # Recalculate income trajectories, resetting starting income of those selected to lost job
  intermediate_cps <- set.income.trajectories(cps,ids.losing.job)
  cps_covid <- build.family.object(intermediate_cps)
  # cps_covid2 <- cps_covid %>%
  #   select(names.to.overwrite,taxid)
  # cps_covid$taxid=as.numeric(cps_covid)
  # cps_covid$CPSID=as.numeric(cps_covid)
  # cps_covid$CPSIDP=as.numeric(cps_covid)
  # cps_covid$taxid <- unlist(cps_covid$taxid,use.names=FALSE)
  # cps_covid$CPSID <- unlist(cps_covid$CPSID,use.names=FALSE)
  # cps_covid$CPSIDP <- unlist(cps_covid$CPSIDP,use.names=FALSE)
  # cps$taxid <- unlist(cps$taxid,use.names=FALSE)
  # cps$CPSID <- unlist(cps$CPSID,use.names=FALSE)
  # cps$CPSIDP <- unlist(cps$CPSIDP,use.names=FALSE)
  # cps2 <- cps %>%
  #   left_join(cps_covid[,c("taxid","taxable","CPSID","CPSIDP")], by=c("taxid","CPSID","CPSIDP"))
  # 
  # cps2 <- cps %>%
  #   left_join(cps_covid2, by="taxid")
  # 
  # try = left_join(cps,cps_covid2,by="taxid")
  # 
  NewIncome <- fam[,c("taxid","taxable")]
  NewIncome$taxid <- unlist(NewIncome$taxid,use.names=FALSE)
  cps2 <- select(cps,-names.to.overwrite) %>%
    left_join(NewIncome, by="taxid")
  cps_old <- cps
  cps <- cps2
  
  # This is the proper way to do it, but it's all the same and it's faster to just load and re-save
  #build.family.object(intermediate_cps,family.type="twentysix") 
  #build.family.object(intermediate_cps,family.type="immigrant") 

  load(file=("NoteBooks/Integration/R_Objects/10Plan_CPS_fam_COVID.RData"))
  load(file=("NoteBooks/Integration/R_Objects/10Plan_CPS_fam_u65_COVID.RData"))
  #save(fam_covid,file=("NoteBooks/Integration/R_Objects/10Plan_26_income_COVID.RData"))
  #save(fam_covid.under65,file=("NoteBooks/Integration/R_Objects/10Plan_26_income_u65_COVID.RData"))
  #save(fam_covid,file=("NoteBooks/Integration/R_Objects/10Plan_immig_income_COVID.RData"))
  #save(fam_covid.under65,file=("NoteBooks/Integration/R_Objects/10Plan_immig_income_u65_COVID.RData"))
  
  # Replace family objects
  rm(fam)
  rm(fam26)
  rm(famImmig)
  fam <- fam_covid
  fam26 <- fam_covid
  famImmig <- fam_covid
  
  # Recalculate FPL
  # TODO: reconsider this if doing Hybrid
  cps$FPL <- calc.FPL.for.pop(cps, year=1)
  
  # Fix deductible
  cps$Deduct <- 0
  cps$Deduct[cps$AgeGroup=="<19" & cps$InsCat=="NonGroup Private"] <- sample(dat$Deduct.x[dat$AgeGroup=="<19" & dat$InsCat=="NonGroup Private"], size=nrow(cps[cps$AgeGroup=="<19" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="<19" & dat$InsCat=="NonGroup Private"], replace=TRUE)
  cps$Deduct[cps$AgeGroup=="19-34" & cps$InsCat=="NonGroup Private"] <- sample(dat$Deduct.x[dat$AgeGroup=="19-34" & dat$InsCat=="NonGroup Private"], size=nrow(cps[cps$AgeGroup=="19-34" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="19-34" & dat$InsCat=="NonGroup Private"], replace=TRUE)
  cps$Deduct[cps$AgeGroup=="35-49" & cps$InsCat=="NonGroup Private"] <- sample(dat$Deduct.x[dat$AgeGroup=="35-49" & dat$InsCat=="NonGroup Private"], size=nrow(cps[cps$AgeGroup=="35-49" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="35-49" & dat$InsCat=="NonGroup Private"], replace=TRUE)
  cps$Deduct[cps$AgeGroup=="50-64" & cps$InsCat=="NonGroup Private"] <- sample(dat$Deduct.x[dat$AgeGroup=="50-64" & dat$InsCat=="NonGroup Private"], size=nrow(cps[cps$AgeGroup=="50-64" & cps$InsCat=="NonGroup Private",]),prob=dat$LONGWT[dat$AgeGroup=="50-64" & dat$InsCat=="NonGroup Private"], replace=TRUE)
  
  return(cps)
}