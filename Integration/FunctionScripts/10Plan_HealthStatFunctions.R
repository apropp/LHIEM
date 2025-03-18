#### A script written by Adrienne Propp in Oct 2019
#### These functions pull the necessary tables & functions from Raff Vardavas'
#### notebook for modeling health status transitions
#<<<<<<< Updated upstream
#=======
# setwd("/Users/apropp/Documents/10Plan/cuban10plan/NoteBooks/Integration")
# path <- getwd()
#>>>>>>> Stashed changes

# load(file.path(path,"R_Objects/health_stat_transitions.Rda"))
load(here("NoteBooks/Integration/R_Objects/health_stat_transitions_unadjusted.Rda"))
transition.rates <- transition.rates.no.MEPS.correction
names(transition.rates)=c("Age","ProbChronic.Male","ProbChronic.Female","Acute.Male","Acute.Female","AcuteRecov")

############################################################################
############################################################################
## This part creates the lookup table from the fertility data
## because the data needs to be pivoted to be easily used.
## This only needs to be redone if changes are made to Raff's notebook.
############################################################################
# #devtools::install_github("tidyverse/tidyr")
# # Adjust names for sampling & transition tables
# initial.rates <- sampling.table
# colnames(initial.rates) <- c("Age","ProbGood.Male","ProbGood.Female","Acute","Chronic")
# colnames(transition.rates) <- c("Age","ProbChronic.Male","ProbChronic.Female","ProbAcute.Male","ProbAcute.Female","AcuteRecov")
# 
# a=melt(transition.rates,id=c("Age","AcuteRecov","ProbAcute.Male","ProbAcute.Female")) # This is an alternative to the below method
# Melt into a lookup table
# transition.rates <- transition.rates %>% pivot_longer(-c(Age,AcuteRecov,ProbChronic.Male,ProbChronic.Female),names_to="Sex",values_to="Prob.Acute")
# transition.rates$Sex <- as.factor(transition.rates$Sex); levels(transition.rates$Sex)=c("female","male")
# transition.rates <- transition.rates %>% pivot_longer(-c(Age,Sex,AcuteRecov,Prob.Acute),names_to="Sex2",values_to="Prob.Chronic")
# transition.rates$Sex2 <- as.factor(transition.rates$Sex2); levels(transition.rates$Sex2)=c("female","male"); transition.rates <- transition.rates[transition.rates["Sex"]==transition.rates["Sex2"],!(names(transition.rates) %in% "Sex2")]
# save(transition.rates,file=here("NoteBooks/Integration/R_Objects/HealthStat_Unadjusted.Rda"))
# save(initial.rates,transition.rates,file=file.path(path,"R_Objects/HealthStatRates.Rda"))
#=======
#load(file.path(path,"R_Objects/HealthStatRates.Rda"))
############################################################################

##########################################
## Predict Transitions in Health Status ##
##########################################

load(file.path(here("/NoteBooks/Integration"),"R_Objects/HealthStatRates.Rda"))
if(healthstat.dist=="unadjusted"){load(here("NoteBooks/Integration/R_Objects/HealthStat_Unadjusted.Rda"))} # This is one of our sensitivity analyses

# Initialize a population with appropriate health status
initialize.health.stat <- function(pop){
  initial.rates$Age <- as.numeric(initial.rates$Age)
  population <- pop %>%
    left_join(initial.rates,by=c("Age")) %>%
    dplyr::mutate(HS=ifelse(Sex=="male",ifelse(rbinom(nrow(pop),1,initial.rates$ProbGood.Male),"good","bad"),ifelse(rbinom(nrow(pop),1,initial.rates$ProbGood.Female),"good","bad")),
                  Morbidity=ifelse(HS=="good","none",ifelse(rbinom(nrow(pop),1,initial.rates$Acute),"acute","chronic")))
  population <- dplyr::select(population, -c("ProbGood.Male","ProbGood.Female","Acute","Chronic"))
  return(population)
}

# Transition health status
# If HS is good, first sample for transition to chronic morbidity, then sample for transition to acute morbidity if that fails.
# If HS is bad,
#                if chronic morbidity, remain there
#                else sample for chronic morbidity, then sample for recovery if that fails.
transition.health.stat <- function(pop){
  population <- pop %>%
    left_join(transition.rates,by=c("Age","Sex")) %>%
    dplyr::mutate(Morbidity=ifelse(HS=="good",
                                   ifelse(rbinom(nrow(pop),1,Prob.Chronic),
                                          "chronic",
                                          ifelse(rbinom(nrow(pop),1,Prob.Acute),"acute","none")),
                                   ifelse(Morbidity=="chronic",
                                          "chronic",
                                          ifelse(rbinom(nrow(pop),1,Prob.Chronic),
                                                 "chronic",
                                                 ifelse(rbinom(nrow(pop),1,AcuteRecov),"none","acute")))))
  population <- population %>%
    dplyr::mutate(HS=ifelse(Morbidity=="none","good","bad")) # New health status based on transition in morbidity
  new <- population[,c("HS","Morbidity")]
  return(new)
}


###################################################
## Transitions are based on the following logic: ##
###################################################

# * If HealthStatus==Good. (then by definition CHRONIC==0)
#     + Binomial trial for development of chronic condition.
#         + If success, HealthStatus $\to$ bad, CHRONIC $\to$ 1.  Break
#     * Binomial trial for development of acute condition.
#         + If success, HealthStatus $\to$ bad, ACUTE $\to$ 1. Break
# * If HealthStatus==Bad
#     + If CHRONIC==1, break
#     + If ACUTE==1 & CHRONIC ==0 , binomial trial for recovery.
#         + If success, HealthStatus $\to$ good, ACUTE $\to$ 0
#     + If ACUTE==1 & CHRONIC ==0 , binomial trial for development of chronic condition
#         + If success, HealthStatus stays bad, CHRONIC $\to$ 1.  Break
