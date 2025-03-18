#### A script written by Adrienne Propp in Oct 2019
#### These functions help us calculate repayment rates
#### We have two possible methods for calculation: marginal & continuous


# Loan  list
L. <- paste("L.",1:(m.years+1),sep=""); N. <- paste("N.",1:(m.years+1),sep="")

# Percentages of FPL and associated repayment % tiers (baseline and hybrid)
levels.10plan <- c(100, 150, 250, 400, 600, 800)
levels.hybrid <- c(400, 450, 525, 600, 700, 800)
levels.hybrid2 <- c(250, 300, 400, 525, 650, 800)
tiers.10plan <- c(.02, .03, .075, .09, .1, .15)
tiers.10plan2 <- c(.01, .02, .03, .05, .1, .15)
tiers.hybrid <- c(.02, .03, .05, .08, .1, .15)
tiers.hybrid2 <- c(.02, .03, .05, .08, .1, .15)

# Select levels & tiers based on policy chosen
# Note, for the baseline we need levels & tiers as placeholders to run the script, but won't actually allow for loans/repayment
if (policy=="10Plan" | policy=="rerun" | policy=="Baseline"){
  levels <- levels.10plan
  tiers <- tiers.10plan
  if (other.test=="LOWER.CAPS"){
    tiers <- tiers.10plan2
  }
} else if (policy=="HYBRID"){
  levels <- levels.hybrid
  tiers <- tiers.hybrid
} else if (policy=="HYBRID2"){
  levels <- levels.hybrid2
  tiers <- tiers.hybrid2
} else {print("Choose a policy option - 10Plan, Baseline, or Hybrid Medicaid Expansion")}


################################
### Calculate Repayment Caps ###
################################
# For continuous scale method, need to prep calculations for incremental increase in repayment cap as % of income
inc.mult <- rep(0,5)
for (i in 1:length(inc.mult)){
  inc.mult[i] <- 1000*(tiers[i+1]-tiers[i])/(levels[i+1]-levels[i])
}

# This function calculates repayment using marginal rate method, given a vector of poverty levels & income (thus for a population, not an individual)
calc.repayment.cap.marginal.vec <- function(pov.levels, incomes){
  tot=rep(0,nrow(pov.levels))
  mult <- incomes[,1]/pov.levels[,1] # This we use as a multiplier to calculate next notch
  
  one <- which(pov.levels>levels[1])
  tot[one] <- tot[one] + tiers[1]*pmin(incomes[one,], levels[2]*mult[one])
  
  two <- which(pov.levels>levels[2])
  tot[two] <- tot[two] + tiers[2]*(pmin(incomes[two,], levels[3]*mult[two])-levels[1]*mult[two]) # Pay 3% on next 100%
  
  three <- which(pov.levels>levels[3])
  tot[three] <- tot[three] + tiers[3]*(pmin(incomes[three,], levels[4]*mult[three])-levels[2]*mult[three]) # Pay 7.5% on next 150%
  
  four <- which(pov.levels>levels[4])
  tot[four] <- tot[four] + tiers[4]*(pmin(incomes[four,], levels[5]*mult[four])-levels[3]*mult[four]) # Pay 9% on next 200%
  
  five <- which(pov.levels>levels[5])
  tot[five] <- tot[five] + tiers[5]*(pmin(incomes[five,], levels[6]*mult[five])-levels[4]*mult[five]) # Pay 10% on next 200%
  
  six <- which(pov.levels>levels[6])
  tot[six] <- tot[six] + tiers[6]*(incomes[six,]-levels[5]*mult[six]) # Pay 15% on anything above 800%
  return(tot)
} # Repayment cap is $0 if not above 100% FPL for a given year


# This function calculates repayment using continuous scale method, given a vector of poverty levels & income (thus for a population, not an individual)
calc.repayment.cap.continuous.vec <- function(pov.levels, incomes){
  mult <- incomes/pov.levels # This we use as a multiplier to calculate next notch
  tot <- vector(length=nrow(pov.levels))
  
  six <- which(pov.levels>levels[6]); tot[six] <- incomes[six,]*tiers[6]
  five <- setdiff(which(pov.levels>levels[5]), six); tot[five] <- incomes[five,]*(tiers[5]+inc.mult[5]/mult[five,]*(incomes[five,]-mult[five,]*levels[5])/1000)
  four <- setdiff(which(pov.levels>levels[4]), c(five,six)); tot[four] <- incomes[four,]*(tiers[4]+inc.mult[4]/mult[four,]*(incomes[four,]-mult[four,]*levels[4])/1000)
  three <- setdiff(which(pov.levels>levels[3]), c(four,five,six)); tot[three] <- incomes[three,]*(tiers[3]+inc.mult[3]/mult[three,]*(incomes[three,]-mult[three,]*levels[3])/1000)
  two <- setdiff(which(pov.levels>levels[2]), c(three,four,five,six)); tot[two] <- incomes[two,]*(tiers[2]+inc.mult[2]/mult[two,]*(incomes[two,]-mult[two,]*levels[2])/1000)
  one <- setdiff(which(pov.levels>levels[1]), c(two,three,four,five,six)); tot[one] <- incomes[one,]*(tiers[1]+inc.mult[1]/mult[one,]*(incomes[one,]-mult[one,]*levels[1])/1000)
  zeros <- setdiff(which(!is.na(pov.levels)),c(one,two,three,four,five,six)); tot[zeros] <- 0
  return(tot)
}

########################################
### Distribute Payment, Update Loans ###
########################################
# Take dataset of individuals and their yearly borrowing and update the loan dataset
# This function takes a population and distributes repayments across family members' loans
distribute.payment <- function(pop, years.through){
  Units <- pop %>%
    dplyr::group_by(TID) %>%
    dplyr::summarise(FamIncome=mean(FamIncome), N=n(), N_Elig=sum(Elig), 
                     L.1=sum(Loan.1), L.2=sum(Loan.2), L.3=sum(Loan.3), L.4=sum(Loan.4), L.5=sum(Loan.5), L.6=sum(Loan.6), L.7=sum(Loan.7), L.8=sum(Loan.8), L.9=sum(Loan.9), L.10=sum(Loan.10), L.11=sum(Loan.11), L.12=sum(Loan.12), L.13=sum(Loan.13), L.14=sum(Loan.14), L.15=sum(Loan.15), L.16=sum(Loan.16), L.17=sum(Loan.17),
                     N.1=sum(Elig[Loan.1 != 0]), N.2=sum(Elig[Loan.2 != 0]), N.3=sum(Elig[Loan.3 != 0]), N.4=sum(Elig[Loan.4 != 0]), N.5=sum(Elig[Loan.5 != 0]), N.6=sum(Elig[Loan.6 != 0]), N.7=sum(Elig[Loan.7 != 0]), N.8=sum(Elig[Loan.8 != 0]), N.9=sum(Elig[Loan.9 != 0]), N.10=sum(Elig[Loan.10 != 0]), N.11=sum(Elig[Loan.11 != 0]), N.12=sum(Elig[Loan.12 != 0]), N.13=sum(Elig[Loan.13 != 0]), N.14=sum(Elig[Loan.14 != 0]), N.15=sum(Elig[Loan.15 != 0]),N.16=sum(Elig[Loan.16 != 0]),N.17=sum(Elig[Loan.17 != 0]))      
  
  joined <- left_join(pop,Units,by="TID") # Merge with household unit info
  #paidoff <- which(apply(joined[L.],1,sum) < joined[,"Cap"]); pop[paidoff,loans] <- 0 # If possible to pay off entire family's loans for all years, do it
  #notpaid <- setdiff(1:nrow(joined),paidoff) # Indices for anyone in a family with loans left to pay
  notpaid <- 1:nrow(joined)
  r <- joined[,"Cap"] # This is the full repayment - in the loop below, we decrease it as we use it to pay off loans
  payment <- rep(0,nrow(joined)) # Initialize
  #payment[paidoff] <- apply(joined[paidoff,loans],1,sum) # The payment of people who pay off entire family's loans
  
  if (years.through>1){
    for (i in 1:(years.through-1)){
      # First we deal with those who have repayment of zero this year
      nopay <- which(joined[notpaid,"Cap"]==0); r[notpaid[nopay]] <- 0; payment[notpaid[nopay]] <- 0
      nopay <- notpaid[nopay]
      
      # Here we look for the individuals whose entire family's loan in year i is less than the repayment cap
      paid.y <- which(joined[notpaid,paste("L.",i,sep="")] <= r[notpaid])   # If can pay off entire family's loan this year, do it
      paid.y <- notpaid[paid.y]
      r[paid.y] <- r[paid.y] - joined[paid.y,paste("L.",i,sep="")]          # Remainder that can be used to pay off remaining loans in other years if the above occured. Remaining from cap - full loan amount this year
      pop[paid.y,paste("Loan.",i,sep="")] <- 0                              # Set loans for each individual in these families this year to zero
      payment[paid.y] <- payment[paid.y] + joined[paid.y,paste("Loan.",i,sep="")]
      
      #noloans <- which(pop[notpaid,paste("Loans.",i,sep="")]==0)
      #if(any(is.na(r))){print(r); return("R is NA")}
      
      # Here we get everyone else
      notpaid.y <- setdiff(notpaid,c(nopay,paid.y)) # All whose families couldn't pay off all loans this year but can pay something
      #if(any(joined[notpaid.y,paste("L.",i,sep="")]==0)){print(joined[notpaid.y,paste("L.",i,sep="")]); return("Div 0")}
      pay <- (joined[notpaid.y,paste("Loan.",i,sep="")]/joined[notpaid.y,paste("L.",i,sep="")])*r[notpaid.y] # Allocate everyone their proportion of repayment
      pop[notpaid.y,paste("Loan.",i,sep="")] <- pop[notpaid.y,paste("Loan.",i,sep="")] - pay
      payment[notpaid.y] <- payment[notpaid.y] + pay
      r[notpaid.y] <- 0
      if(all(r==0)){break}
      # cut <- r[notpaid.y]/joined[notpaid.y,paste("N.",i,sep="")] # This is the share each individual will get. Remaining repayment divided by # members who need repayment.
      # pop[notpaid.y,paste("Loan.",i,sep="")] <- pop[notpaid.y,paste("Loan.",i,sep="")] - cut
      # cut <- pmin(0, pop[notpaid.y,paste("Loan.",i,sep="")]) # In case one family member only had a little bit left to pay off & more should be distributed to other family members, capture the negative
      # while (cut > 0){
    }
  }
  pop[,"Loans.totalthisyear"] <- apply(pop[,loans],1,sum)
  pop[,"Repayment.thisyear"] <- payment
  pop[,"Ever.Repaid"] <- pop[,"Ever.Repaid"] + pop[,"Repayment.thisyear"]
  return(pop)
}

##############################
### Deal with 26 year olds ###
##############################
# This function contains a switch for 26-year olds, whether to distribute loans to family members or not
# Assume that parents are over 40 years old.
# Only distribute to parents under 65, since those above 65 can't have loans.
# Problems:
# What if 26 year old has no parents or just one parent? What if 26 year olds' parents are over 65?
# Assuming the 26 year old's loans get split equally between all 40-65 year old adults in household
twentysix.loans <- function(pop, year){
  twentysix.fams <- pop$TID[which(pop$Age==26)]
  for (id in twentysix.fams){
    children <- which(pop$TID==id & pop$Age==26)
    parent.ids <- which(pop$TID==id & pop$Age>40)
    N <- length(parent.ids)
    for (child in children){
      if (N==0){
        pop[child,"Forgiven.26"] <- rowSums(pop[child,loans])
      } else {
        loan.burden.each.parent <- pop[child,loans]/N
        pop[parent.ids,loans] <- sweep(pop[parent.ids,loans,],2,unlist(loan.burden.each.parent,use.names=FALSE),"+")
        pop[parent.ids,"From.Children"] <- pop[parent.ids,"From.Children"] + sum(loan.burden.each.parent)
        pop[child,"To.Parents"] <- sum(pop[child,loans])
      }
    }
  }
  return(pop)
}

# # This is an old version of the function above, before we decided we wanted to redistribute loans to parents
# twentysix.loans0 <- function(pop, year){
#   if (year < 8) {return(pop)}
#   else{
#     twentysix.fams <- pop$TID[which(pop$Age==26)]
#     for (id in twentysix.fams){
#       parent.ids <- which(pop$TID==id & pop$Age>40 & pop$Age<65)
#       N <- length(parent.ids)
#       loan.burden.each.parent <- pop[which(pop$TID==id & pop$Age==26),loans]/N
#       pop[parent.ids,loans] <- sweep(pop[parent.ids,loans,],2,unlist(loan.burden.each.parent,use.names=FALSE),"+")
#     }
#   return(pop)
#   }
# }



########################################
########################################
### Old Functions -  These functions ###
### are not currently used because   ###
### they operate at the individual   ###
### rather than population level     ###
########################################
########################################
# # This function calculates repayment using marginal rate method
# calc.repayment.cap.marginal <- function(pov.level, income){
#   tot <- 0
#   mult <- income/pov.level # This we use as a multiplier to calculate next notch
#   if (pov.level>levels[1]){
#     tot <- tot + tiers[1]*min(income, levels[2]*mult) # Pay 2% on first 150% FPL
#     if (pov.level>levels[2]){
#       tot <- tot + tiers[2]*(min(income, levels[3]*mult)-levels[1]*mult) # Pay 3% on next 100%
#       if (pov.level>levels[3]){
#         tot <- tot + tiers[3]*(min(income, levels[4]*mult)-levels[2]*mult) # Pay 7.5% on next 150%
#         if (pov.level>levels[4]){
#           tot <- tot + tiers[4]*(min(income, levels[5]*mult)-levels[3]*mult) # Pay 9% on next 200%
#           if (pov.level>levels[5]){
#             tot <- tot + tiers[5]*(min(income, levels[6]*mult)-levels[4]*mult) # Pay 10% on next 200%
#             if (pov.level>levels[6]){
#               tot <- tot + tiers[6]*(income-levels[5]*mult) # Pay 15% on anything above 800%
#             }
#           }
#         }
#       }
#     }
#     return(tot)
#   } 
#   else {return(0)}
# } # Repayment cap is $0 if not above 100% FPL for a given year
# 
# # This function calculates repayment using continuous scale method
# calc.repayment.cap.continuous <- function(pov.level, income){
#   mult <- income/pov.level # This we use as a multiplier to calculate next notch
#   if (pov.level>levels[6]){
#     tot <- income*tiers[6]
#   } else if (pov.level>levels[5]){
#     tot <- income*(tiers[5]+inc.mult[5]/mult*(income-mult*levels[5])/1000)
#   } else if (pov.level>levels[4]){
#     tot <- income*(tiers[4]+inc.mult[4]/mult*(income-mult*levels[4])/1000)
#   } else if (pov.level>levels[3]){
#     tot <- income*(tiers[3]+inc.mult[3]/mult*(income-mult*levels[3])/1000)
#   } else if (pov.level>levels[2]){
#     tot <- income*(tiers[2]+inc.mult[2]/mult*(income-mult*levels[2])/1000)
#   } else if (pov.level>levels[1]){
#     tot <- income*(tiers[1]+inc.mult[1]/mult*(income-mult*levels[1])/1000)
#   } else {tot <- 0}
#   return(tot)
# } # Repayment cap is $0 if not above 100% FPL for a given year

