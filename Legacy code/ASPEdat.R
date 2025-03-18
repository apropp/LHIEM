#### A script written by Adrienne Propp in Feb 2019
#### Data to input to ASPEfunc.R for ASPE Project with Carter Price

FPL.multiplier <- c(0, 1.38, 2.49, 3.99, 5.49, 6.99) # 6 possibilities
FPLs <- c(12140, 16460, 20780, 25100, 29420, 33740, 38060) # https://familiesusa.org/product/federal-poverty-guidelines

# MyCare repayment parameters 
alpha <- c(0.02, 0.05, 0.08, 0.1, 0.12, 0.15) # 0.02 = 2%
beta <- c(0.08, 0.2, 0.32, 0.4, 0.48, 0.6) # 0.08 = 8%
gamma <- c(0.2, 0.5, 0.8, 1.0, 1.2, 1.5) # 0.2 = 20%

# HSA contribution parameters (s means single individuals, f is for families of 2+)
h.fam.s <- c(250, 500, 1000, 10800, 10800, 10800) # dollars
h.gov.s <- c(1000, 500, 250, 0, 0, 0) # dollars
h.2.gov.s <- c(500, 250, 125, 0, 0, 0) # dollars
h.fam.f <- c(500, 1000, 2000, 29050, 29050, 29050) # dollars
h.gov.f <- c(2000, 1000, 500, 0, 0, 0) # dollars
h.2.gov.f <- c(1000, 500, 250, 0, 0, 0) # dollars

h.fam <- matrix(data=rbind(h.fam.s, h.fam.f),nrow=2)
h.gov <- matrix(data=rbind(h.gov.s, h.gov.f),nrow=2)
h.2.gov <- matrix(data=rbind(h.2.gov.s, h.2.gov.f),nrow=2)


# Income distribution data
# Assume this includes individuals (ALL households)
w.m <- 86220; w.sd <- 30000 # Reasonable numbers based on https://www.census.gov/content/dam/Census/library/publications/2018/demo/p60-263.pdf
# Other sources:
# https://www.thebalance.com/what-is-average-income-in-usa-family-household-history-3306189
# https://www.census.gov/content/dam/Census/library/publications/2018/acs/acsbr17-01.pdf this one gives median, mean > median
# https://www.usatoday.com/story/money/personalfinance/2016/11/24/average-american-household-income/93002252/

#m.s <- 48251; s.s <- 20000

# Medical Expenditure parameter:
m.m <- 1422 # average expenditure





