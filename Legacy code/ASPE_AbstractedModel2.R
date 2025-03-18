#### A script written by Adrienne Propp in Feb 2019
#### Intended to complement Task 7 of ASPE Project with Carter Price
#### Simulates implementation of MyCare program

library('metRology') # Required for triangular distribution (used for h_fam distribution)
source("ASPEfunc.R") # Functions script
source("ASPEdat.R") # Data & specific numbers defined in task sheet

#set.seed(123) # Provides reproducible results
years <- 30; debtyears <- 20; n <- 5000 # Model parameters

## DEFINE DISTRIBUTIONS
# Define income distribution; mean & sd defined in ASPEdat.R
location <- log(w.m^2/sqrt(w.sd^2 + w.m^2)); shape <- sqrt(log(1 + (w.sd^2/w.m^2)))
incomes <- rlnorm(n,location,shape); #mean(incomes); sd(incomes)

# Define medican expenditure distribution; mean defined in ASPEdat.R
expenditures <- rexp(n, 1/m.m)

# Define household size distribution based
sizes <- sample(x=7, size=n, replace=T, prob=c(28.01,34.52,15.15,12.91,5.83,2.23,1.34)) # Sample from 1-7 with defined probabilities
# data from https://www.statista.com/statistics/242189/disitribution-of-households-in-the-us-by-household-size/ 

noise <- 0.007 # Noise parameter, fitted to give reasonable levels of fluctuation

## PREALLOCATE variables & arrays
individuals <- array(0, c(years, 7, n)) # Rows for years
colnames(individuals) <- c("Wage","MedExpend","CatIns-Event","CatIns-Debt","HSATotal","GovSpending","HSAContrib-Fam")
# Columns for 1. income, 2. med. spending, 3. cat.ins (event), 4. cat.ins (accumulated), 5. HSA total, 6. gov. spending, 7. fam. contr.
debt <- array(0, c(years, debtyears, n)) # Rows for years; Columns for debt from x years prior (up to 20)
TotalContributions <- vector('numeric', years) # Total contributions to HSA by government
DebtDischarged <- vector('numeric', years) # Debt discharged after 20 years
maxx <- 0; due <- 0; payoff <- maxx-due
contribution.f <- 0; contribution.g <- vector('numeric',years); catastrophic <- vector('numeric',2)

## ITERATE for each individual, iterate through # of years
for (tick in (1:n)){
  size <- sizes[tick]; FPL <- FPLs[size] # FPLs given in ASPEdat.R; first choose family size, then take appropriate FPL level
  indiv <- matrix(0, years, 7) # Pre-allocate matrix of (wt, mt, ct-event, ct-debt, HSAt, govt, hfam) (same as individuals, but for each household separately)
  w <- incomes[tick]; m <- expenditures[tick]; #m <- mm # Initial values for wage & medical spending from previously randomly selected values
  d <- matrix(0, years, debtyears) # Pre-allocate debt matrix
  
  HSA <- 0;  # Initialize HSA
  TSL <- FALSE; DEBTOR <- FALSE # Indicate if total stop loss threshold for catastrophic spending has been met & if individual has accrued debt
  
  xs <- seq(1:years); indiv[,1] <- exp(rnorm(1,8.5,1.5))*log(xs) + w # Populate wage column using function
  
  # Define medical trajectories
    # includes random yearly fluctuations as well as low probability of catastrophic event
  trajectory <- rbinom(1,1,0.5) # Randomly choose spending trajectory for men or women, 50-50 chances, success gives female trajectory
  if (trajectory == 1){
    indiv[,2] <- exp(rnorm(1,5,1))*log(xs) + m + rexp(years,rate=noise) + rnorm(1,35000,5000)*rbinom(years,1,0.01) # female trajectory is logarithmic - increases most rapidly early in life
  } else {
    indiv[,2] <- runif(1,1.1,1.35)^xs + m + rexp(years,rate=noise) + rnorm(1,35000,5000)*rbinom(years,1,0.01) # male trajectory is exponential - increases most rapidly later in life
  }
  
  # Determine government contribution
  contribution.g <- Contribution.Gov(indiv[,1],size); TotalContributions <- TotalContributions + contribution.g
  
  # Determine household contribution, sampled from a distribution with maximum value as listed in Table 1 of memo
  indiv[,7] <- round(rtri(years, min=0, max=Contribution.Fam(indiv[,1],size), mode=0), digits=2)
  
  # Iterate through the years
  for (tock in (1:years)){
    discharged <- 0 # Start with no discharged debt for the year
    w <- indiv[tock,1]; m <- indiv[tock,2] # Take this year's income & spending values
    due <- ActualDue(m,w,TSL) # Amount of med spending for this year, the rest is CatIns
    catastrophic <- unlist(CatIns(m,w,TSL)) # Calculate CatIns
    indiv[tock,3:4] <- catastrophic #[1]; indiv[tock,4] <- catastrophic[2] # Add to CatIns to matrix: first entry is standard yearly CatIns, second is TSL accumulated CatIns
    contribution.f <- indiv[tock,7]
    maxx <- YearlyMax(w) # Maximum amount to be paid this year out of HSA, or out of pocket if HSA depleted
    HSA <- HSA + contribution.g[tock] + contribution.f # Add contributions HSA from this year at beginning of year
    
    # Course of action depends on if individual has debt (must pay first-in-first-out)
    if (DEBTOR == FALSE){
      if (due <= maxx){
        HSA <- ifelse(due < HSA, HSA - due, 0) # If owe less this year than can pay, pay as much as possible out of HSA
      } else {
        d.new <- due - maxx; DEBTOR <- TRUE; d[tock,1] <- d.new # If can't pay it all, the additional will become debt
        HSA <- ifelse(maxx < HSA, HSA - maxx, 0)
        }
    } else {
      d.new <- due; d.old <- d[tock-1,]; discharged <- tail(d.old,1) # Determine if any old debt discharged
      DebtDischarged[tock] <- DebtDischarged[tock] + discharged # Keep track of aggregate debt discharged
      d.old <- head(d.old,-1); d.now <- c(d.new,d.old); d[tock,] <- d.now # Copy last years's debt & add new debt from this year
      payoff <- maxx # Max amount of debt we can pay off this year
      d[tock,] <- PayDebt(d[tock,], payoff, debtyears) # PayDebt function defined in ASPEfunc.R
      
      if (sum(d[tock,]) == 0) { DEBTOR <- FALSE } # Check if no debt left - then no longer a "debtor"
      HSA <- ifelse(maxx < HSA, HSA - maxx, 0) # HSA reduced by amount paid off or completely depleted
    }

    indiv[tock,5] <- HSA # Save remaining HSA balance
    indiv[tock,6] <- discharged + contribution.g[tock] + sum(catastrophic) # Log how much gov spent on this household this year
    
    if (TSL == FALSE & sum(d[tock,]) > GAMMA(w)*w) {TSL <- TRUE} # If total stop loss (total debt accumulated) exceeds threshold
    if (TSL == TRUE & sum(d[tock,]) < BETA(w)*w) {TSL <- FALSE} # If already exceeded TSL threshold, it only stops after debt reduced to B*w

    } # End time loop

debt[,,tick] <- d # Save household debt to aggregate debt matrix
individuals[,,tick] <- indiv # Save household data to aggregate data matrix

}



## PLOTTING
# Baseline Values against which to compare during sensitivity analysis
Gov.n <- apply(individuals[,6,], 1, mean) # Average total spending per household
AA <- Gov.n; AAA <- apply(individuals[,6,],1,sum) # Average & total spending
BB <- TotalContributions/n; BBB <- TotalContributions # Average & total HSA spending
CC1 <- apply(individuals[,3,],1,mean); CCC1 <- apply(individuals[,3,],1,sum) # Average & total CatIns event spending
CC2 <- apply(individuals[,4,],1,mean); CCC2 <- apply(individuals[,4,],1,sum) # Average & total CatIns accumulation spending
DD <- DebtDischarged/n; DDD <- DebtDischarged # Average & total DebtDischarged spending
II <- individuals
DDD <- debt
SS <- sizes
plot(AA,main="Government Cost Breakdown",ylim=c(0,300),xlab="Years",ylab="Average Cost per Household",type="l",lwd=2)
points(BB,col="blue",type="l",lty=2,lwd=2); points(CC1,col="orange",type="l",lty=2,lwd=2); points(CC2,col="red",type="l",lty=2,lwd=2); points(DD,col="purple",type="l",lty=2,lwd=2)
legend("topright",legend=c("Overall","HSA contributions", "Annual-debt cat. insurance","Total-debt cat. insurance","Discharged debt"),fill=c("black","blue","orange","red","purple"))

#plot(apply(individuals[,1,],1,mean),main="Trajectory of Average Income Per Household",xlab="Years",ylab="Dollars")
