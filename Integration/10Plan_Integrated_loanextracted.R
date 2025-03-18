###########
## Notes ##
###########
# A year's loans will reflect amount after payments (copays + 10P repayment)
# Assuming that every eligible household will enroll their eligible members. In report, discuss switching of programs. 
# Currently setting deceased to 0 and newborns to mothers' weight. Do we weight families or individuals? Assume individuals.
# Currently prescription refills are counted as a "visit", so incur copay if over $25 (otherwise just paid fully).
# OOP could become > 10% of income for individuals who visit doctor often. This incentivizes fewer, more costly visits to increase borrowing potential.
# 26 year olds have their total loans forgiven, so that they start off full year of 26 with no loans
# All family members with a loan balance get proportional payment, even if not eligible that year

####################
## Load Functions ##
####################
load(here("NoteBooks/Integration/R_Objects/cps.Rda"))
load(here("NoteBooks/Integration/R_Objects/Visits.Rda"))
load(here("NoteBooks/Integration/R_Objects/BehavioralAdjustments.Rda"))

prepare.pop <- function(cps){
  source(here("NoteBooks/Integration/FunctionScripts/10Plan_Parameters.R"))
  source(here("NoteBooks/Integration/FunctionScripts/10Plan_FertilityFunctions.R"))   # Functions script for fertility & childbirth
  source(here("NoteBooks/Integration/FunctionScripts/10Plan_ExpenditureFunctions.R")) # Functions script for medical expenditures
  source(here("NoteBooks/Integration/FunctionScripts/10Plan_IncomeFunctions.R"))    # Functions script for income evolution
  source(here("NoteBooks/Integration/FunctionScripts/10Plan_RepaymentFunctions.R"))   # Functions script for loan repayment
  source(here("NoteBooks/Integration/FunctionScripts/10Plan_HealthStatFunctions.R"))  # Functions script for health status
  source(here("NoteBooks/Integration/FunctionScripts/10Plan_MortalityFunctions.R"))   # Functions script for mortality
}

########################
## Prepare Population ##
########################
run.dynamics <- function(cps){
  # Load pre-processed CPS dataset
  cps10 <- cps[1:10000,]
  #cps10 <- cps10[cps10$AGE<65,]
  
  # Initialize
  #ind.df <- data.frame(PID=cps10$CPSIDP,FID=cps10$CPSID,TID=cps10$taxid,Sex=cps10$Sex,Age=cps10$AGE,AgeGroup=cps10$AgeGroup,Race=cps10$Race,HS=cps10$HS,Income=cps10$Income,FamIncome=cps10$FamIncome,FPL=0,Cap=0,MedSpend=cps10$MedSpend,Visits=0,OOP=0,Borrow=0,InsCat=cps10$InsCat,Preg=factor("not pregnant"),Elig=0,Survive=1,Morbidity=0,WT=cps10$ASECWT)
  ind.df <- data.frame(PID=cps10$CPSIDP,FID=cps10$CPSID,TID=cps10$taxid,id=0,Sex=cps10$Sex,Age=cps10$AGE,AgeGroup=cps10$AgeGroup,
                       Race=cps10$Race,HS=NA,WageProp=pmin(1,cps10$WageProp),FamIncome=cps10$FamIncome,
                       FPL=cps10$FPL,Cap=0,MedSpend=cps10$MedSpend,Visits=0,OOP=0,
                       Borrow=0,Ever.Borrowed=0,Yearly.Interest=0,Tot.Interest=0,Ever.Forgiven=0,Ever.Repaid=0,
                       From.Children=0,To.Parents=0,InsCat=cps10$InsCat,Deduct=cps10$Deduct,Preg=NA,Elig=0,Survive=1,
                       Morbidity=NA,WT=cps10$ASECWT,WTH=cps10$ASECWTH,Immig=cps10$Immig)
  ind.df[loans] <- 0; ind.df[forgiveness] <- 0; ind.df[totals] <- 0 # Start out with no loans or forgiven balances
  ind.df$Preg <- as.factor(ifelse(apply(ind.df,1,child.this.year),"pregnant","not pregnant")) # Predict childbirths
  #ind.df$Elig[ind.df$Age>=65] <- 0 # Ensure nobody above age 65 is marked as eligible
  #ind.df$Elig[ind.df$Age<65 & (ind.df["InsCat"]=="NonGroup Private" | ind.df["InsCat"]=="Uninsured")] <- 1 # Set up eligibility for 10Plan
  ind.df$Elig[ind.df["InsCat"]=="NonGroup Private" | ind.df["InsCat"]=="Uninsured"] <- 1 # Set up eligibility for 10Plan
  ind.df <- initialize.health.stat(ind.df) # Initialize health status & Morbidity
  twentysix <- ind.df[ind.df$Age==26,] # We'll need to sample from this population for Income, InsCat of newly 26 population each year. Retain weight of the people we are modifying but sample from this set for income
  eligible.immigs <- ind.df[ind.df$Immig==1 & ind.df$Elig==1,]
  ineligible.immigs <- ind.df[ind.df$Immig==1 & ind.df$Elig==0,]
  immigs.pop <- ind.df[ind.df$Immig==1,]
  if (policy=="Baseline"){
    ind.df$OOP <- cps$SLF
  }
  
  # Set unique person id
  ind.df$id <- c(1:nrow(ind.df))
  max.id <- max(ind.df$id)
  max.tid <- max(cps$taxid)
  
  # Set up our list of yearly records - initially they are all a copy of year 1, but we will modify as we go along
  years <- replicate(n=m.years, expr=ind.df, simplify=F) # List of datasets for individuals
  
  #######################
  ## Run Dynamic Model ##
  #######################
  set.seed(898)
  total <- m.years*3; pb <- txtProgressBar(min = 0, max = total, style = 3); system.time({
    for (tick in (2:(m.years+1))){
      past <- years[[tick-1]]                   # Load last year's state
      curr <- past                              # Start out on previous state
      #curr <- past[past$Age<65,]                              # Start out on previous state
      #past.pop.live <- past[past$Age<65 & past$Survive==1,]
      past.pop.live <- past[past$Survive==1,]
      
      ## Specify populations of interest
      #curr[curr["Age"]>=85,"Survive"] <- 0      # For our purposes, assume our population consists only of individuals <85 years old
      alive <- curr[curr["Survive"]==1,]; dec <- setdiff(curr,alive)      # This is our "alive" population. We track income for all individuals who are alive.
      if(nrow(alive)+nrow(dec)!=nrow(curr)){return("Problem")}            # Check
      curr <- remove.wage.income(curr, tick-1) # Remove proportion of wage income from retirees & decedents
      
      ## From here, only consider those surviving  
      curr <- alive
      
      ## Make adjustments from last year
      curr[curr["Age"]==26,"TID"] <- max.tid + c(1:nrow(curr[curr["Age"]==26,])) # Give 26 year olds their own taxid
      max.id <- max(curr$id); max.tid <- max(curr$TID)               # Adjust max id and taxid
      #curr[curr["Age"]>=65,"Elig"] <- 0       # Nobody over 65 is eligible
      #curr[curr["Age"]>=65,"MedSpend"] <- 0   # Set spending for anyone over 65 to 0 so we don't have to complicate our calculations later
      curr[curr["MedSpend"]==0,c("Visits","OOP","Borrow")] <-0  # If medical spending is zero, assume no visits, copays, or borrowing
      ## Update income for everyone
      reset.twentysix(curr[curr$Age==26,], tick-1)
      curr["FPL"] <- calc.FPL.for.pop(curr, tick-1) # Make sure FPL up to date before updating income
      curr <- new.income2(curr, tick-1) # Evolve family income for entire population
      curr[,"Elig"] <- 0
      curr["FPL"] <- calc.FPL.for.pop(curr, tick-1) # Update FPL after updating income
      if(policy=="HYBRID"){
        curr[,"InsCat"] <- ifelse(curr[,"FPL"]<=400, which(levels(curr[,"InsCat"])=="Medicaid"), curr[,"InsCat"])
      } else if (policy=="HYBRID2"){
        curr[,"InsCat"] <- ifelse(curr[,"FPL"]<=250, which(levels(curr[,"InsCat"])=="Medicaid"), curr[,"InsCat"])
      }
      curr$InsCat <- factor(curr$InsCat, labels=c("NonGroup Private","Other Public","Medicaid","Uninsured","Other Private"))
      #curr[curr["Age"]<65 & (curr["InsCat"]=="NonGroup Private" | curr["InsCat"]=="Uninsured"),"Elig"] <- 1       # Update eligible population
      curr[curr["InsCat"]=="NonGroup Private" | curr["InsCat"]=="Uninsured","Elig"] <- 1       # Update eligible population
      
      ## Start process of updating health spending
      pop <- curr[curr["Age"]<65,] # This is our population of interest. We only care about expenditures for the population < 65 years old. 
      # However, we still care about non-eligible people because they might become eligible if income changes.
      # So basically, 'pop' is our "potentially eligible population", and 'curr' is our "income-relevant population"
      
      setTxtProgressBar(pb, 3*(tick-1)-2)
      
      ## Update HS for pop
      pop[,c("HS","Morbidity")] <- transition.health.stat(pop)
      
      ## Predict spending for pop
      #pop[,"Preg"] <- as.factor(ifelse(apply(pop,1,child.this.year),"pregnant","not pregnant")) # Predict childbirths
      pop <- predict.childbirths(pop, tick-1) # Predict childbirths
      pop[,"MedSpend"] <- evolve.spending.R(pop,unins.inc) # Predict medical spending
      actual.inflation <- (sum(pop[,"MedSpend"]*pop[,"WT"])/sum(pop[,"WT"]))/(sum(past.pop.live[,"MedSpend"]*past.pop.live[,"WT"])/sum(past.pop.live[,"WT"]))
      pop[,"MedSpend"] <- (medical.inflation.rate/actual.inflation)*pop[,"MedSpend"] # Recalibrate medical spending to desired inflation rate
      pop[,"Visits"] <- est.visits(pop) # Predict # visits - this is faster
      
      # Predict out of pocket spending. For the baseline run, we just inflate the TOTSLF variable sampled from the MEPS
      if (policy=="Baseline"){
        pop[,"OOP"] <- pop[,"OOP"]*medical.inflation.rate
      } else {
        pop[,"OOP"] <- pmin(pop[,"MedSpend"],mapply(total.OOP, expenditures=pop[,"MedSpend"], visits=pop[,"Visits"])) # Calculate estimated OOP copays based on # visits
      }
      
      setTxtProgressBar(pb, 3*(tick-1)-1)
      
      ## Recombine alive, pop. Overwrite relevant lines with pop. From here, no more dealing with pop.
      #curr[curr["Age"]<65,c("Preg","MedSpend","Visits","OOP")] <- pop[c("Preg","MedSpend","Visits","OOP")]
      curr <- pop
      
      ## Predict mortality
      curr <- predict.survival2(curr, tick-1) # Create "death" indicator to include this year's deaths. Individual will have "Survive"==0 if selected to die this year
      dying <- which(curr["Survive"]==0) # This gives indices for those who will die
      if(mortality.test=="chronic"){
        chronic.dying <- which(curr$Survive==0 & curr$Morbidity=="chronic")
        curr[chronic.dying,"MedSpend"] <- pmax(curr[chronic.dying,"MedSpend"],inflate.death.exp(curr[chronic.dying,])) # Adjust medical spending for decedents
      } else {
        curr[dying,"MedSpend"] <- pmax(curr[dying,"MedSpend"],inflate.death.exp(curr[dying,])) # Adjust medical spending for decedents
      }
      #curr[curr["Age"]>=65,"MedSpend"] <- 0
      
      ## Leave out the previously dead population altogether - they are no longer relevant to our model. The "alive" group includes those who will die this year
      
      ## Add in our newborn population
      new.borns <- add.newborns(curr)
      new.borns$id <- max.id+c(1:nrow(new.borns))
      max.id<- max(new.borns$id)
      curr <- rbind(curr,new.borns)
      
      actual.inflation <- (sum(curr[,"MedSpend"]*curr[,"WT"])/sum(curr[,"WT"]))/(sum(past[,"MedSpend"]*past[,"WT"])/sum(past[,"WT"]))
      curr[,"MedSpend"] <- (medical.inflation.rate/actual.inflation)*curr[,"MedSpend"] # Recalibrate medical spending to desired inflation rate
      
      ## Evolve age & age group for those who have survived
      curr[curr["Survive"]==1,"Age"] <- sapply(curr[curr["Survive"]==1,"Age"], function(x) x+1)           # Evolve age
      curr[curr["Survive"]==1,"AgeGroup"] <- sapply(curr[curr["Survive"]==1,"Age"], determine.age.group)  # Evolve age group
      #curr[,"Repayment.total"] <- pmin(apply(curr[,loans],1,sum), curr[,"Cap"]) # Repayment is either equal to the cap, or the full amount of loans if possible to completely pay off
      #curr[,"Loans.total"] <- pmax(0, apply(curr[,loans],1,sum) - curr[,"Repayment.total"])
      
      ## Add immigration
      curr <- add.immigrants2(curr,immigs.pop,max.tid,max.id,tick-1)
      max.tid <- max(curr$TID)
      max.id <- max(curr$id)
      
      ## Confirm the entire population has the right FPL & eligibility flag - this has to be done again since we added immigrants
      curr[,"Elig"] <- 0
      curr["FPL"] <- calc.FPL.for.pop(curr, tick-1)
      if(policy=="HYBRID"){
        curr[,"InsCat"] <- ifelse(curr[,"FPL"]<=400, which(levels(curr[,"InsCat"])=="Medicaid"), curr[,"InsCat"])
      } else if (policy=="HYBRID2"){
        curr[,"InsCat"] <- ifelse(curr[,"FPL"]<=250, which(levels(curr[,"InsCat"])=="Medicaid"), curr[,"InsCat"])
      }
      curr$InsCat <- factor(curr$InsCat, labels=c("NonGroup Private","Other Public","Medicaid","Uninsured","Other Private"))
      #curr[curr["Age"]<65 & (curr["InsCat"]=="NonGroup Private" | curr["InsCat"]=="Uninsured"),"Elig"] <- 1       # Update eligible population
      curr[curr["InsCat"]=="NonGroup Private" | curr["InsCat"]=="Uninsured","Elig"] <- 1       # Update eligible population
      
      
      ## Update list of yearly records with current year
      years[[tick]] <- curr
      
      setTxtProgressBar(pb, 3*(tick-1))
    }}) 
  close(pb)
  return(years)
}


##################
## Apply 10Plan ##
##################
apply.plan <- function(years){
  set.seed(12345)
  total <- m.years*2; pb <- txtProgressBar(min = 0, max = total, style = 3); system.time({
    for (tick in (2:(m.years+1))){
      past <- years[[tick-1]]                               # Load last year's state
      past <- past[past[,"Survive"]==1 & past[,"Age"]<85,]  # We only want to evolve loans for those who survived LAST year (aka those who made it to this year)
      curr <- years[[tick]]; curr <- curr[order(curr$id),]                # Load this year's state
      curr[1:nrow(past),c(loans)] <- past[order(past$id),c(loans)]        # Load last year's loans for the relevant population - the new additions will have $0 loan balance
      curr[1:nrow(past),c(overalltotals)] <- past[order(past$id),c(overalltotals)]
      
      ## Incorporate any behavioral adjustments to spending
      if(elderly.reaction=="increase.care") {curr["MedSpend"] <- inflate.spending.pre.medicare(curr)}
      if(behavioral=="CONSERVATIVE"){
        curr["Deduct"] <- curr["Deduct"]*medical.inflation.rate
        curr[curr$InsCat=="Uninsured","MedSpend"] <- curr[curr$InsCat=="Uninsured","MedSpend"]*uninsured.conservative # Adjust uninsured
        curr[curr$InsCat=="NonGroup Private","MedSpend"] <- ifelse(curr[curr$InsCat=="NonGroup Private","MedSpend"]<=curr[curr$InsCat=="NonGroup Private","Deduct"], curr[curr$InsCat=="NonGroup Private","MedSpend"]*insured.unmet.conservative, curr[curr$InsCat=="NonGroup Private","Deduct"]*insured.unmet.conservative + (curr[curr$InsCat=="NonGroup Private","MedSpend"] - curr[curr$InsCat=="NonGroup Private","Deduct"])*insured.met.conservative) # Adjust insured
      }
      if(behavioral=="AGGRESSIVE"){
        curr["Deduct"] <- curr["Deduct"]*medical.inflation.rate
        curr[curr$InsCat=="Uninsured","MedSpend"] <- curr[curr$InsCat=="Uninsured","MedSpend"]*uninsured.aggressive # Adjust uninsured
        curr[curr$InsCat=="NonGroup Private","MedSpend"] <- ifelse(curr[curr$InsCat=="NonGroup Private","MedSpend"]<=curr[curr$InsCat=="NonGroup Private","Deduct"], curr[curr$InsCat=="NonGroup Private","MedSpend"]*insured.unmet.aggressive, curr[curr$InsCat=="NonGroup Private","Deduct"]*insured.unmet.aggressive + (curr[curr$InsCat=="NonGroup Private","MedSpend"] - curr[curr$InsCat=="NonGroup Private","Deduct"])*insured.met.aggressive) # Adjust insured
      }
      
      ## Calculate repayment caps based on family income & FPL
      if(cap.scale=="MARGINAL") {curr["Cap"] <- calc.repayment.cap.marginal.vec(curr["FPL"], curr["FamIncome"])}
      if(cap.scale=="CONTINUOUS") {curr["Cap"] <- calc.repayment.cap.continuous.vec(curr["FPL"], curr["FamIncome"])}
      if(cap.scale=="none") {curr["Cap"] <- 0}
      
      setTxtProgressBar(pb, 2*(tick-1)-1)
      
      ## Add this year's loans, adjust interest
      if(other.test=="REDUCE.COPAY"){
        low.inc <- which(curr$FPL<400) # Low income group
        V <- curr[low.inc,"OOP"]/25 # Determine numberr of visits
        curr[low.inc,"OOP"] <- reduced.copay*V # Re-calculate OOP
      }
      
      if(behavioral=="BORROW.LESS"){
        #curr[,"Borrow"] <- if_else(curr[,"Elig"]==1, pmin(borrowing.param*curr[,"MedSpend"],curr[,"MedSpend"]-curr[,"OOP"]), 0) # Assume max 85% is borrowed this year
        curr[,"Borrow"] <- if_else(curr[,"Elig"]==1, pmax(curr[,"MedSpend"]-curr[,"OOP"]-(borrowing.param*curr[,"Cap"]),0), 0) # Assume they'll pay up to 15% over required cap
        curr[,"OOP"] <- curr[,"MedSpend"] - curr[,"Borrow"] # Adjust OOP to include additional not borrowed
      } else{
        curr[,"Borrow"] <- if_else(curr[,"Elig"]==1, curr[,"MedSpend"]-curr[,"OOP"], 0) # Assume everything above copays is borrowed this year
      }
      curr[,"Ever.Borrowed"] <- curr[,"Ever.Borrowed"] + curr[,"Borrow"]              # Keep track of borrowing to date, not including interest
      curr[,"Yearly.Interest"] <- rowSums((interest.rate-1)*curr[,loans])             # This year's interest to be added
      curr[,"Tot.Interest"] <- curr[,"Tot.Interest"] + curr[,"Yearly.Interest"]       # Keep track of total interest accumulated
      curr[,loans] <- interest.rate*curr[,loans]              # Add interest to existing loans
      curr[,paste("Loan.",tick-1,sep='')] <- curr[,"Borrow"]  # Add a new loan for this year, equal to amount borrowed this year
      
      ## Determine forgiveness
      # NOTE: Since age is transitioned at the end of the year, anyone who turned 26 at any point during the year shows up as 26 years old.
      # For this reason, we use age==26 and age==65, one year older than the explicit age.
      if(tick>15){
        curr[,"Forgiven.expire"] <- curr[,paste("Loan.",tick-15,sep='')]                      # Forgiven debt due to expiry
        curr[,paste("Loan.",tick-15,sep='')] <- 0
      }                                     
      curr[,"Forgiven.death"] <- ifelse(curr[,"Survive"]==0, rowSums(curr[,loans]), 0)        # Forgiven debt due to death
      curr[curr["Survive"]==0, loans] <- 0
      
      if (twentysix.option=="FORGIVE"){
        curr[,"Forgiven.26"] <- ifelse(curr[,"Age"]==26, rowSums(curr[,loans]), 0)
        curr[curr["Age"]==26, loans] <- 0
      } else if (twentysix.option=="TO.PARENTS"){
        curr <- twentysix.loans(curr,tick-1)
        curr[curr["Age"]==26, loans] <- 0
      } else {return("Error with loans for 26-year-olds")}
      
      curr[,"Forgiven.65"] <- ifelse(curr[,"Age"]==65, rowSums(curr[,loans]), 0)
      curr[curr["Age"]==65, loans] <- 0
      
      curr[,"Forgiven.thisyear"] <- apply(curr[,forgiveness],1,sum)                  # This year's forgiveness from all reasons
      if(any(curr[,"Forgiven.thisyear"]!=rowSums(curr[,c("Forgiven.expire","Forgiven.death","Forgiven.26","Forgiven.65")]))){return("Problem")} # Check
      curr[,"Ever.Forgiven"] <- curr[,"Ever.Forgiven"] + curr[,"Forgiven.thisyear"]  # Keep track of forgiven for all years
      
      ## Reset forgiven loans
      #curr[curr["Age"]==26, c(loans,"Loans.totalthisyear")] <- 0  # Reset loans & total forgiven, paid & borrowed for anyone who turned 26 this year
      #curr[curr["Age"]==65, c(loans,"Loans.totalthisyear")] <- 0  # Reset loans & total forgiven, paid & borrowed for anyone who turned 65 this year
      
      ## Make payments
      curr <- distribute.payment(curr,tick-1)                 # Adjust current loans given this year's payment cap. Note the family payment is distributed across family members starting with oldest loans first.
      
      years[[tick]] <- curr
      setTxtProgressBar(pb, 2*(tick-1))
    }  })
  return(years)
}



#################
## Validations ##
#################
# for (i in 1:15){print(table(is.na(years[[i]])))}                        # Ensure no NAs
# for (i in 1:15){print(table(years[[i]][,"Survive"]))}                   # Consider number of people dying per year. Death rate about 1%
# for (i in 1:15){print(table(years[[i]][,"Sex"],years[[i]][,"Preg"]))}   # Make sure only women are having children, and evaluate number per year
# for (i in 1:15){
#   hist(years[[i]][,"Age"],main=paste("Histogram of Age, Year",i))
#   d <- density(years[[i]][,"Age"])
#   #plot(d,main=paste("Histogram of Age, Year",i))
# }
# # Age distribution seems quite consistent
