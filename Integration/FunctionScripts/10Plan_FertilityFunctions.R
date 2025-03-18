#### A script written by Adrienne Propp in Oct 2019
#### These functions pull the necessary tables & functions from Raff Vardavas'
#### notebook for modeling births

# Statistics for medical costs in first year of life from:
# http://www.ncsl.org/research/health/prenatal-care-postcard.aspx

# Statistics for maternity costs from
# https://bmjopen.bmj.com/content/4/1/e004017
# Maximum: 37% (average discount price) of $72,569 (maximum caesarean section charge)
# Median: $5123. Mean ~ median+6% according to https://www.healthaffairs.org/doi/full/10.1377/hlthaff.2014.1088?url_ver=Z39.88-2003&rfr_id=ori%3Arid%3Acrossref.org&rfr_dat=cr_pub%3Dpubmed
# So we will use log(1.06*5123)~8.6 as our mean
# We will use log(835) as our min
# All of this needs to be transformed from 2011 dollars using the inflation factor 1.142987, from CPI https://data.bls.gov/cgi-bin/cpicalc.pl?cost1=10000.00&year1=201101&year2=201901 

############################################################################
############################################################################
## This part creates the lookup table from the fertility data.
## This only needs to be redone if changes are made to Raff's notebook.
############################################################################
#Predicted.Mx.output.file <- "NoteBooks/Fertility/Data/BirthRateUS2017.xlsx"
#br<-read.xlsx(here(Predicted.Mx.output.file))
#print(br)
#br.lookup <- melt(br,id="mothers")
#colnames(br.lookup) <- c("Race","AGEG","birth.rate")
##within(br.lookup, rm(gender))
#br.lookup <- br.lookup %>% 
#  filter(AGE!="births") %>%
#  mutate(SEX="Female")
#save(br.lookup,file="br_lookup.Rda")
############################################################################
############################################################################

library(Runuran)
load(here("NoteBooks/Integration/R_Objects/br_lookup.Rda"))
load(here("NoteBooks/Integration/R_Objects/infant_health.Rda"))
br.lookup$Race[br.lookup$Race=="all"] <- "other"
# path <- getwd()
# load(file.path(path,"R_Objects/br_lookup.Rda"))
# load(file.path(path,"R_Objects/infant_health.Rda"))
# load(file.path(path,"R_Objects/nvisits.Rda"))

#loans <- paste("Loan.",1:m.years,sep=""); cols.fam <- c(cols.fam, loans)

# Here we want to link the birth rate based on race, age
# pop<-meps %>%
#   left_join(br.lookup,by=c("Race","AGEG","SEX")) %>%
#   mutate(n.children=0,
#          totexp.extra=0)
# 
# birthrate <- br.lookup$birth.rate[]

#########################
### Predict Pregnancy ###
#########################
# Function to predict whether or not a woman will have a baby this year
child.this.year <- function(indiv){
  if(indiv["Sex"]=="male" | indiv["Age"]>50){
    child <- FALSE # Skip if male or over 50
  } else {
    indiv["AGEG"] <- as.character(cut(as.numeric(indiv["Age"]),breaks=seq(0,100,5),include.lowest = F,right=F)) # Appropriate age groupings
    if(indiv["Race"]=="other"){indiv["Race"]="all"} # Adjust race - anyone other than white, black, hispanic will get overall birth rate
    if(indiv["AGEG"][[1]] %in% levels(br.lookup$AGEG)){
      birthrate <- br.lookup$birth.rate[br.lookup$Race==indiv["Race"][[1]] & as.character(br.lookup$AGEG)==as.character(indiv["AGEG"][[1]])]
      child <- as.logical(rbinom(1,1,birthrate))
      #print(birthrate)
    } else {child <- FALSE }}
  return(child)
}

predict.childbirths <- function(pop, year){
  population <- pop
  threshold <- births.per.year[year]
  population$Preg <- "not pregnant"
  population$AGEG <- as.character(cut(as.numeric(population$Age),breaks=seq(0,100,5),include.lowest=F,right=F)) # Appropriate age groupings
  #levels(population$Race)[levels(population$Race)=="other"] <- "all" # Adjust race - anyone other than white, black, hispanic will get overall birth rate
  #population["birthrate"] <- ifelse(population$Sex=="female" & population$AGEG %in% levels(br.lookup$AGEG), br.lookup$birth.rate[br.lookup$Race==population$Race & as.character(br.lookup$AGEG)==population$AGEG],0)
  population <- population %>% dplyr::left_join(br.lookup,by=c("Sex","Race","AGEG"))
  population$birth.rate[is.na(population$birth.rate)] <- 0
  population$birth <- rbinom(nrow(population),1,population$birth.rate)
  while (sum(population$WT[population$birth==1])<threshold){
    population$birth[population$birth==0] <- rbinom(nrow(population[population$birth==0,]),1,population$birth.rate[population$birth==0])
  }
  population <- population[sample(1:nrow(population)),] # Randomize order
  population <- population[order(-population$birth),] # Order so those selected to have a child rise to the top
  population$c <- cumsum(population$WT) # Take cumulative sum of weights
  population$Preg[population$c <= threshold] <- "pregnant" # Those with top fertility selected to have a child, until threshold reached
  population$Preg[population$c > threshold] <- "not pregnant" # The rest survive
  return(population[,names(pop)])
}



##############################
### Predict Maternal Costs ###
##############################
# Function to predict cost of childbirth (assuming it occurs)
child.cost <- function(childbirth=TRUE){
  #cost <- 1.142987*pmin(pmax(rlnorm(1, meanlog = 8.6, sdlog = 1), 835), .37*72569)
  cost <- 1.142987*urlnorm(1, meanlog = 8.6, sdlog = 1, lb=835, ub=.37*72569) # urlnorm is similar to rlnorm but allows for truncation
  return(cost)
}

##################################
### Add Newborns to Population ###
##################################
# Function to add newborns to the population
add.newborns <- function(population){
  preg.pop <- population[population["Preg"]=="pregnant",]
  newborns <- preg.pop %>%
    mutate(Sex = factor(rbinom(nrow(.),1,0.5)),
           Age = 0, AgeGroup = "<19",
           Preg = "not pregnant", Morbidity = 0,
           HS0 = rbinom(nrow(.), 4, (infant.health.meps-1)/4)+1,
           HS = as.factor(ifelse(HS0<=3 ,"good","bad")), # This line is complicated, but uses average infant health to stochasically generate health status on 1-5 scale which we convert
           MedSpend = rpois(nrow(.),HS0^2.5*1000), Visits = 0, OOP=0, Borrow = 0, Immig=0,
           PID = FID + (PID%%100+1)
    ) %>%
    dplyr::select(-HS0)
  #newborns["Borrow"] <-  max(0, MedSpend-25)
  newborns[,loans] <- 0 # Set newborn loans to zero
  newborns[,"Visits"] <- est.visits(newborns)
  newborns[,"Morbidity"] <- as.factor(ifelse(newborns[,"HS"]=="good","none","chronic"))
  newborns[,"OOP"] <- mapply(total.OOP, expenditures=newborns[,"MedSpend"], visits=newborns[,"Visits"])
  newborns[,"Borrow"] <- if_else(newborns[,"Elig"]==1, newborns[,"MedSpend"]-newborns[,"OOP"], 0)
  levels(newborns$Sex) <- c("male","female")
  return(newborns)
}






