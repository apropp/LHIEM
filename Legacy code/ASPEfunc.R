#### A script written by Adrienne Propp in Feb 2019
#### Intended to complement ASPEfunc.R for ASPE Project with Carter Price

## Define functions to calculate needed values

# FPL column number - defines which column in Tables 1 & 2 the individual falls into
FPLcol <- function(w){
  fpl <- w/FPL
  index <- max(which(fpl > FPL.multiplier))
  return(index)
}

# Threshold parameter for catastrophic insurance
BETA <- function(w){
  B <- beta[FPLcol(w)]
  return(B)
}

# Threshold parameter for repayment
ALPHA <- function(w){
  A <- alpha[FPLcol(w)]
  return(A)
}

# Threshold parameter for total accumulated debt
GAMMA <- function(w){
  G <- gamma[FPLcol(w)]
  return(G)
}

# Year's required payment
ActualDue <- function(m, w, TSL){
  B <- BETA(w)
  if (TSL == TRUE) { actual <- 0 } else { actual <- min(m, B*w) }
  return(actual)
}

# Catastrophic insurance portion, split into each part
CatIns <- function(m, w, TSL){
  B <- BETA(w)
  G <- GAMMA(w)
  if (TSL == TRUE) { c1 <- 0; c2 <- m } else { c1 <- max(0, m - B*w); c2 <- 0 }
  return(list(c1,c2))
}

# Yearly maximum payable by household
YearlyMax <- function(w){
  ym <- ALPHA(w)*w
  return(ym)
}

# # Contribution to HSA
# Contribution <- function(m, w){
#   h <- max(0, h.fam + h.gov + h.2.gov)
#   return(h)
# }

## Contributions to HSA
# Gov contribution
Contribution.Gov <- function(w, size){
  a <- ifelse(size == 1, 1, 2)
  #h <- h.gov[a,FPLcol(w)] + h.2.gov[a,FPLcol(w)]
  h <- h.gov[a,sapply(w,FPLcol)] #+ h.2.gov[a,sapply(w,FPLcol)]
  return(h)
}
# This next one gives max family contributions, but actual contribution sampled from a triangular distribution reaching up to this value
Contribution.Fam <- function(w, size){
  a <- ifelse(size == 1, 1, 2)
  #h <- h.fam[a,FPLcol(w)]
  h <- h.fam[a,sapply(w,FPLcol)]
  return(h)
}

# Function which pays off as much debt as possible up to yearly maximum payment
PayDebt <- function(d,payoff,debtyears){
  for (yback in (debtyears:1)){
    if (payoff == 0){ break } # If can't pay off any more, stop loop
    else if (d[yback] == 0){ next } # If no debt from yback years prior, move on
    else if (d[yback] > payoff){
      d[yback] <- d[yback] - payoff
      payoff <- 0
      break # Used up all available funds
    }
    else if (d[yback] <= payoff){
      d[yback] <- 0
      payoff <- payoff-d[yback]
      #next # Paid off debt from yback years prior, keep going
    }
  }
  return(d)
}

