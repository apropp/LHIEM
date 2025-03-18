#### A script written by Adrienne Propp in Oct 2019
#### These functions pull the necessary tables & functions from Raff Vardavas'
#### notebook for modeling mortality

# path <- getwd()
# load(file.path(path,'R_Objects/mx_table.Rdata'))
# load(file.path(path,'R_Objects/mx_cost.Rdata'))
load(here("NoteBooks/Integration/R_Objects/mx_table.RData"))
load(here("NoteBooks/Integration/R_Objects/mx_cost.Rdata"))

# Determine age group from age
determine.age.group <- function(age){
  #AgeGroup <- ifelse(age>=65,"65+",ifelse(age>=50,"50-64",ifelse(age>=35,"35-49",ifelse(age>=19,"19-34","<19"))))
  AgeGroup <- ifelse(age>=50,"50-64",ifelse(age>=35,"35-49",ifelse(age>=19,"19-34","<19")))
}

# Predict whether each individual will survive or die this year.
predict.survival <- function(pop){
  population <- get.mx(pop)
  survive <- rbinom(nrow(population),1,1-population$mx)
  return(survive)
}

predict.survival2 <- function(pop, year){
  threshold <- deaths.per.year[year] # Number of deaths required per year
  population <- get.mx(pop) # Get mortality rates
  population$d <- rbinom(nrow(population),1,population$mx)
  while (sum(population$WT[population$d==1])<threshold){
    population$d[population$d==0] <- rbinom(nrow(population[population$d==0,]),1,population$mx[population$d==0])
  }
  population <- population[sample(1:nrow(population)),] # Randomize order
  population <- population[order(-population$d),] # Order so those selected to die rise to the top
  population$c <- cumsum(population$WT) # Take cumulative sum of weights
  population$Survive[population$c <= threshold] <- 0 # Those with top mortality selected to die, until threshold reached
  population$Survive[population$c > threshold] <- 1 # The rest survive
  return(population[,names(pop)])
}

# Inflate medical expenditure if selected for death
inflate.death.exp <- function(pop){
  population <- get.mx(pop); range <- mx.range; model <- model.end.of.life.spending
  mx <- pmin(pmax(population$mx,range[1]),range[2])
  R <- predict(model, mx)
  return(R)
}

# Function to read mortality table
get.mx <- function(pop){
  mx_table <- mx_table %>% mutate(Age=as.numeric(Age))
  avg.mort.table <- pop %>%
    dplyr::select(Age,Sex,MedSpend,WT) %>% 
    dplyr::group_by(Age,Sex) %>% 
    dplyr::summarise(totexp.at.reference.percentile=pmax(1,round(weighted.quantile(MedSpend, WT, probs=0.564, na.rm = T)),0)) %>% 
    left_join(mx_table,by=c("Age","Sex"))
  
  population <- pop %>% 
    left_join(avg.mort.table,by=c("Age","Sex")) %>% 
    dplyr::mutate(spending.ratio = MedSpend/totexp.at.reference.percentile) %>% 
    dplyr::mutate(MedSpend=round(MedSpend,0)) %>% 
    dplyr::rename(avg.mx=mx)
  
  # Determine which scaled version of mortality ratio model to use (sensitivity analyses)
  if (mortality.test=="rescale.up"){
    Mortality.Model <- inter.model.mx.ratio.Upper
  } else if (mortality.test=="rescale.down"){
    Mortality.Model <- inter.model.mx.ratio.Lower
  } else {
    Mortality.Model <- inter.model.mx.ratio
  }
  
  population$mx.ratio <- get.predicted.mx(population$spending.ratio, model=Mortality.Model, range=spending.ratio.range)
  population$mx <- pmin(1,population$mx.ratio*population$avg.mx)
  #population$mx <- population$avg.mx
  deaths <- rbinom(nrow(population),1,1-population$mx)
  return(population)
}


# # This is an old version of the predict.survival function. No longer being used
# predict.survival0 <- function(population){
#   population <- population %>% 
#     left_join(avg.mort.table,by=c("Age","Sex")) %>% 
#     dplyr::mutate(spending.ratio = MedSpend/totexp.at.reference.percentile) %>% 
#     dplyr::mutate(MedSpend=round(MedSpend,0)) %>% 
#     dplyr::rename(avg.mx=mx)
#   
#   population$mx.ratio <- get.predicted.mx(population$spending.ratio, model=inter.model.mx.ratio ,range=spending.ratio.range)
#   population$mx <- pmin(1,population$mx.ratio*population$avg.mx)
#   deaths <- rbinom(nrow(population),1,1-population$mx)
#   return(deaths)
# }





# mx.range <- range(Einav1462.BD$Mortality)
# model.end.of.life.spending <- loess(Health.Spending~Mortality,data=Einav1462.BD)
# 
# totexp.end.of.life <- inflate.spending.at.death(pop.die$mx, model=model.end.of.life.spending,range=mx.range)
# pop.die$totexp.end.of.life <- pmax(pop.die$totexp,totexp.end.of.life)