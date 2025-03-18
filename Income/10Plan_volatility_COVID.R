# This code was modified by Adrienne Popp in summmer 2020. This script gets run outside of the model but is necessary to get
# year-specific income deltas
rm(list=ls())
library(Hmisc)
library(expm)
library(rpart)
library(rpart.plot)
library(dplyr)

# TODO: What is the difference between the "h" and "s" values?
# TODO: We could use year by year?
  # But then what to do after years run out? Project median, LB & UB
  # How to vary? Instead of drawing from middle 40 % of deltas we could shift that up or down by ex. 5%

r <- .021 # inflation rate

#data <- read.csv("J269473/10Plan_data.csv")
#data <- read.csv("J269480/10Plan_data.csv")
data <- read.csv("Notebooks/Income/10Plan_data.csv")

file <- "NoteBooks/Income/cpi-u-rs.csv"
cpi <- read.csv(file, header = TRUE, sep = ",")

#nam.list <- c("delta","age","sex","kids","married","famid","inc","w")
#delta.temp <- as.data.frame(matrix(0,nrow = tab_len, ncol = length(nam.list)))
#names(delta.temp) <- nam.list

data$id <- 1:nrow(data)
#Convert dollars into real 2018 dollars
delt.list <- c("inc64h","inc86h","inc108h","inc1210h","inc1412h","inc1614h","inc64s","inc86s","inc108s","inc1210s","inc1412s","inc1614s")
data[,delt.list] <- NA
data$inc2004h <- data$ER27931*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2004]
data$inc2004s <- data$ER27943*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2004]

data$inc2006h <- data$ER40921*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2006]
data$inc2006s <- data$ER40933*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2006]
id.list <- data$id[!is.na(data$inc2004h) & data$inc2004h > 1 & !is.na(data$inc2006h) & data$inc2006h > 1]
nam.list <- c("delta","age","sex","kids","married","famid","inc",'w')
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2006h[data$id %in% id.list]/data$inc2004h[data$id %in% id.list]) - 1
delta.temp$age <- 2004 - data$ER33906[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2004h[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER28078[data$id %in% id.list]
data$inc64h[data$id %in% id.list] <- sqrt(data$inc2006h[data$id %in% id.list]/data$inc2004h[data$id %in% id.list]) - 1
#delta.mat0406 <- delta.temp
delta.temp$year <- 6 #04-06
delta.mat <- delta.temp

id.list <- data$id[!is.na(data$inc2004s) & data$inc2004s > 1 & !is.na(data$inc2006s) & data$inc2006s > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2006s[data$id %in% id.list]/data$inc2004s[data$id %in% id.list]) - 1
delta.temp$age <- 2004 - data$ER33906[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2004s[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER28078[data$id %in% id.list]
data$inc64s[data$id %in% id.list] <- sqrt(data$inc2006s[data$id %in% id.list]/data$inc2004s[data$id %in% id.list]) - 1
#delta.mat0406 <- rbind(delta.mat0406,delta.temp)
delta.temp$year <- 6 #04-06
delta.mat <- rbind(delta.mat, delta.temp)

data$inc2008h <- data$ER46829*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2008]
data$inc2008s <- data$ER46841*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2008]
id.list <- data$id[!is.na(data$inc2006h) & data$inc2006h > 1 & !is.na(data$inc2008h) & data$inc2008h > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2008h[data$id %in% id.list]/data$inc2006h[data$id %in% id.list]) - 1
delta.temp$age <- 2006 - data$ER34006[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2006h[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER41069[data$id %in% id.list]
data$inc86h[data$id %in% id.list] <- sqrt(data$inc2008h[data$id %in% id.list]/data$inc2006h[data$id %in% id.list]) - 1
#delta.mat0608 <- delta.temp
delta.temp$year <- 8 #06-08
delta.mat <- rbind(delta.mat, delta.temp)

id.list <- data$id[!is.na(data$inc2006s) & data$inc2006s > 1 & !is.na(data$inc2008s) & data$inc2008s > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2008s[data$id %in% id.list]/data$inc2006s[data$id %in% id.list]) - 1
delta.temp$age <- 2006 - data$ER34006[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2006h[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER41069[data$id %in% id.list]
data$inc86s[data$id %in% id.list] <- sqrt(data$inc2008s[data$id %in% id.list]/data$inc2006s[data$id %in% id.list]) - 1
#delta.mat0608 <- rbind(delta.mat0608,delta.temp)
delta.temp$year <- 8 #06-08
delta.mat <- rbind(delta.mat, delta.temp)

data$inc2010h <- data$ER52237*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2010]
data$inc2010s <- data$ER52249*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2010]
id.list <- data$id[!is.na(data$inc2008h) & data$inc2008h > 1 & !is.na(data$inc2010h) & data$inc2010h > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2010h[data$id %in% id.list]/data$inc2008h[data$id %in% id.list]) - 1
delta.temp$age <- 2008 - data$ER34106[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2008h[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER47012[data$id %in% id.list]
data$inc108h[data$id %in% id.list] <- sqrt(data$inc2010h[data$id %in% id.list]/data$inc2008h[data$id %in% id.list]) - 1
#delta.mat0810 <- delta.temp
delta.temp$year <- 10 #08-10
delta.mat <- rbind(delta.mat, delta.temp)

id.list <- data$id[!is.na(data$inc2008s) & data$inc2008s > 1 & !is.na(data$inc2010s) & data$inc2010s > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2010s[data$id %in% id.list]/data$inc2008s[data$id %in% id.list]) - 1
delta.temp$age <- 2008 - data$ER34106[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2008s[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER47012[data$id %in% id.list]
data$inc108s[data$id %in% id.list] <- sqrt(data$inc2010s[data$id %in% id.list]/data$inc2008s[data$id %in% id.list]) - 1
#delta.mat0810 <- rbind(delta.mat0810,delta.temp)
delta.temp$year <- 10 #08-10
delta.mat <- rbind(delta.mat, delta.temp)

data$inc2012h <- data$ER58038*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2012]
data$inc2012s <- data$ER58050*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2012]
id.list <- data$id[!is.na(data$inc2010h) & data$inc2010h > 1 & !is.na(data$inc2012h) & data$inc2012h > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2012h[data$id %in% id.list]/data$inc2010h[data$id %in% id.list]) - 1
delta.temp$age <- 2010 - data$ER34206[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2010h[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER52436[data$id %in% id.list]
data$inc1210h[data$id %in% id.list] <- sqrt(data$inc2012h[data$id %in% id.list]/data$inc2010h[data$id %in% id.list]) - 1
#delta.mat1012 <- delta.temp
delta.temp$year <- 12 #10-12
delta.mat <- rbind(delta.mat, delta.temp)

id.list <- data$id[!is.na(data$inc2010s) & data$inc2010s > 1 & !is.na(data$inc2012s) & data$inc2012s > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2012s[data$id %in% id.list]/data$inc2010s[data$id %in% id.list]) - 1
delta.temp$age <- 2010 - data$ER34206[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2010s[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER52436[data$id %in% id.list]
data$inc1210s[data$id %in% id.list] <- sqrt(data$inc2012s[data$id %in% id.list]/data$inc2010s[data$id %in% id.list]) - 1
#delta.mat1012 <- rbind(delta.mat1012,delta.temp)
delta.temp$year <- 12 #10-12
delta.mat <- rbind(delta.mat, delta.temp)

data$inc2014h <- data$ER65216*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2014]
data$inc2014s <- data$ER65244*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2014]
id.list <- data$id[!is.na(data$inc2012h) & data$inc2012h > 1 & !is.na(data$inc2014h) & data$inc2014h > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2014h[data$id %in% id.list]/data$inc2012h[data$id %in% id.list]) - 1
delta.temp$age <- 2012 - data$ER34307[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2012h[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER58257[data$id %in% id.list]
data$inc1412h[data$id %in% id.list] <- sqrt(data$inc2014h[data$id %in% id.list]/data$inc2012h[data$id %in% id.list]) - 1
#delta.mat1214 <- delta.temp
delta.temp$year <- 14 #12-14
delta.mat <- rbind(delta.mat, delta.temp)

id.list <- data$id[!is.na(data$inc2012s) & data$inc2012s > 1 & !is.na(data$inc2014s) & data$inc2014s > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2014s[data$id %in% id.list]/data$inc2012s[data$id %in% id.list]) - 1
delta.temp$age <- 2012 - data$ER34307[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2012s[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER58257[data$id %in% id.list]
data$inc1412s[data$id %in% id.list] <- sqrt(data$inc2014s[data$id %in% id.list]/data$inc2012s[data$id %in% id.list]) - 1
#delta.mat1214 <- rbind(delta.mat1214,delta.temp)
delta.temp$year <- 14 #12-14
delta.mat <- rbind(delta.mat, delta.temp)

data$inc2016h <- data$ER71293*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2016]
data$inc2016s <- data$ER71321*cpi$AVG[cpi$YEAR == 2018]/cpi$AVG[cpi$YEAR == 2016]
id.list <- data$id[!is.na(data$inc2014h) & data$inc2014h > 1 & !is.na(data$inc2016h) & data$inc2016h > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2016h[data$id %in% id.list]/data$inc2014h[data$id %in% id.list]) - 1
delta.temp$age <- 2014 - data$ER34506[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2014h[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER65492[data$id %in% id.list]
data$inc1614h[data$id %in% id.list] <- sqrt(data$inc2016h[data$id %in% id.list]/data$inc2014h[data$id %in% id.list]) - 1
#delta.mat1416 <- delta.temp
delta.temp$year <- 16 #14-16
delta.mat <- rbind(delta.mat, delta.temp)

id.list <- data$id[!is.na(data$inc2014s) & data$inc2014s > 1 & !is.na(data$inc2016s) & data$inc2016s > 1]
delta.temp <- as.data.frame(matrix(0,nrow = length(id.list), ncol = length(nam.list)))
names(delta.temp) <- nam.list
delta.temp$delta <- sqrt(data$inc2016s[data$id %in% id.list]/data$inc2014s[data$id %in% id.list]) - 1
delta.temp$age <- 2014 - data$ER34506[data$id %in% id.list]
delta.temp$sex <- data$ER32000[data$id %in% id.list]
delta.temp$kids <- data$ER36020[data$id %in% id.list]
delta.temp$married <- data$ER32049[data$id %in% id.list]
delta.temp$inc <- data$inc2014s[data$id %in% id.list]
delta.temp$famid <- data$id[data$id %in% id.list]
delta.temp$w <- data$ER65492[data$id %in% id.list]
data$inc1614s[data$id %in% id.list] <- sqrt(data$inc2016s[data$id %in% id.list]/data$inc2014s[data$id %in% id.list]) - 1
#delta.mat1416 <- rbind(delta.mat1416,delta.temp)
delta.temp$year <- 16 #14-16
delta.mat <- rbind(delta.mat, delta.temp)

#summary(data[,delt.list])

print(summary(delta.mat))
print(dim(delta.mat))

delta.mat <- delta.mat[order(delta.mat$inc),]
delta.mat$rank <- 0
delta.mat$rank <- delta.mat$w/sum(delta.mat$w)
delta.mat$rank <- cumsum(delta.mat$rank)

delta.mat <- delta.mat[order(delta.mat$inc*(1+delta.mat$delta)),]
delta.mat$rank2 <- 0
delta.mat$rank2 <- delta.mat$w/sum(delta.mat$w)
delta.mat$rank2 <- cumsum(delta.mat$rank2)

delta.mat$age.group <- 0
delta.mat$age.group[delta.mat$age >= 18 & delta.mat$age < 25] <- 1
delta.mat$age.group[delta.mat$age >= 25 & delta.mat$age < 35] <- 2
delta.mat$age.group[delta.mat$age >= 35 & delta.mat$age < 50] <- 3
delta.mat$age.group[delta.mat$age >= 50 & delta.mat$age < 65] <- 4
delta.mat <- delta.mat[delta.mat$age.group != 0,]

delta.mat$inc.group <- 1
delta.mat$inc.group[delta.mat$rank > .2 & delta.mat$rank <= .4] <- 2
delta.mat$inc.group[delta.mat$rank > .4 & delta.mat$rank <= .6] <- 3
delta.mat$inc.group[delta.mat$rank > .6 & delta.mat$rank <= .8] <- 4
delta.mat$inc.group[delta.mat$rank > .8] <- 5

delta.mat$group <- delta.mat$sex + 2*(delta.mat$age.group - 1) + 8*(delta.mat$inc.group - 1)

delta.mat$jloss <- 0
delta.mat$jloss[delta.mat$delta < -.5] <- 1
save(delta.mat,file="10Plan_PSID_COVID.RData")


# summary(delta.mat$delta[delta.mat$delta >-.5 & delta.mat$delta < 1])
# summary(delta.mat$delta[delta.mat$delta >-.9 & delta.mat$delta < 1])
# summary(delta.mat$delta[delta.mat$delta >-.9 & delta.mat$delta < 1 & delta.mat$inc > 10000])
# summary(delta.mat$delta[delta.mat$delta >-.9 & delta.mat$delta < 1 & delta.mat$inc > 15000])
# 
# deltas.collapsed <- delta.mat %>%
#   group_by(group,year) %>%
#   summarise(min=min(delta),max=max(delta),
#             meandelta=mean(delta),
#             median=median(delta),
#             inc=mean(inc.group),ageg=mean(age.group))
# #Generate transition rates from very low income to more income
# #Generate transitions between big drop, growth, and big jump
# #Generate values for baseline growth
# library(ggplot2)
# deltas.collapsed %>%
#   ggplot( aes(x=year, y=median, group=group,color=group)) +
#   geom_line() +
#   ylab("Median Delta") +
#   xlab("Year")

ng <- max(delta.mat$group)
# Set up deltas by year
for(Y in seq(8,16,2)){
  thresh <- -.7
  #load(file="10Plan_PSID_COVID.RData")
  #load(file="10Plan_PSID.RData")
  
  # Find proportion in each group that has job loss, put in column `jloss`
  delta1.n <- c()
  for (g in 1:ng) {
    delta1.n <- rbind(delta1.n,c(g,sum(delta.mat$w[delta.mat$group==g & delta.mat$delta<thresh & delta.mat$year==Y])/sum(delta.mat$w[delta.mat$group==g & delta.mat$year==Y])))
  }
  delta1.n <- as.data.frame(delta1.n)
  names(delta1.n) <- c("g","jloss")
  delta1.n[,"year"] <- Y
  if(!exists("delta1")){delta1 <- c()}
  delta1 <- rbind(delta1,delta1.n)
  
  # Find prop moving out of bottom 15th percentile
  thr1 <- .15
  thr2 <- .15
  scaler <- 1
  if(!exists("delta2")){delta2 <- c()}
  delta2.n <- sum(delta.mat$w[delta.mat$rank<thr1 & delta.mat$rank2>thr2 & delta.mat$year==Y])/sum(delta.mat$w[delta.mat$rank<thr1 & delta.mat$year==Y])*scaler
  delta2.n <- as.data.frame(delta2.n)
  names(delta2.n) <- c("prop")
  delta2.n[,"year"] <- Y
  delta2 <- rbind(delta2,delta2.n)
  
  # Find the incomes & weights of those moving out of 15th percentile
  delta3.n <- cbind(delta.mat$inc[delta.mat$rank<thr1 & delta.mat$rank2>thr2 & delta.mat$year==Y],delta.mat$w[delta.mat$rank<thr1 & delta.mat$rank2>thr2 & delta.mat$year==Y])
  delta3.n <- as.data.frame(delta3.n)
  names(delta3.n) <- c("inc","w")
  delta3.n[,"year"] <- Y
  if(!exists("delta3")){delta3 <- c()}
  delta3 <- rbind(delta3,delta3.n)
  
  # This holds the deltas for everyone, sorted by group
  delta4.n <- subset(delta.mat, year==Y, select=c(delta, w, year, group))
  delta4.n <- as.data.frame(delta4.n)
  delta4.n$delta <- 1 + delta4.n$delta + r
  if(!exists("delta4")){delta4 <- c()}
  delta4 <- rbind(delta4,delta4.n)
  
  # This assumes those that are selected to lose jobs go into bottom 15th percentile
  delta5.n <- cbind(delta.mat$inc[delta.mat$rank<thr1 & delta.mat$year==Y],delta.mat$w[delta.mat$rank<thr1 & delta.mat$year==Y])
  delta5.n <- as.data.frame(delta5.n)
  names(delta5.n) <- c("inc","w")
  delta5.n[,"year"] <- Y
  if(!exists("delta5")){delta5 <- c()}
  delta5 <- rbind(delta5,delta5.n)
}

save(delta1,delta2,delta3,delta4,delta5,file="Deltas_COVID.RData")
