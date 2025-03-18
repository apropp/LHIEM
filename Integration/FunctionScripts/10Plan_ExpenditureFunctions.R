#### A script written by Adrienne Propp in Oct 2019
#### These functions help us calculate medical expenditures.

library(rpart)
library(treeClust)
set.seed(484)

# path <- getwd()
# #load("nonzero_spending_reg.Rda")
# load(file.path(path,"R_Objects/CART.Rda"))
# load(file.path(path,"R_Objects/zvnz_logit.Rda"))
# load(file.path(path,"R_Objects/nvisits.Rda"))
#load(here("NoteBooks/Integration/R_Objects/nvisits.Rda"))
#load(here("NoteBooks/Integration/R_Objects/zvnz_logit.Rda"))
#load(here("NoteBooks/Integration/R_Objects/CART.Rda"))

#######################
### Evolve Spending ###
#######################

# This version is for use with evolve.spending.R, which Raff has helped me optimize for speed by separating the population by partition first
# We have a separate regression for use with those who had zero spending in year t, but are predicted to have nonzero spending in year t+1
predict.from.zeros.R <- function(x){
  reg <- z.partition.regression
  L <- predict(reg,x,type="response")
  r <- sample(reg$residuals,nrow(x),replace = TRUE)[[1]]
  S <- z.smear*exp(L)+exp(r)
  return(S)
}
#pmin(1,0.35+predict(mylogit,newdata=.,type="response"))
# Fastest version of evolve.spending
evolve.spending.R <- function(past.dat,unins.inc="none"){
    factor <- uninsured.increase.demand
    if(unins.inc=="zero.increase"){factor <- 0};
    if(unins.inc=="35.increase"){factor <- uninsured.increase.demand.higher}
    new <- past.dat %>%
      mutate(nonzeroY1 = ifelse(MedSpend==0,"zero","nonzero"), TOTEXPY1=MedSpend, Income=as.numeric(FamIncome)) %>%
    # mutate(prob = ifelse(Age>=65,0,ifelse(InsCat=="Uninsured",
    #                      ifelse(unins.inc=="zero.increase",predict(mylogit,newdata=.,type="response"),
    #                             ifelse(unins.inc=="35.increase",pmin(1,0.35+predict(mylogit,newdata=.,type="response")),pmin(1,0.2+predict(mylogit,newdata=.,type="response")))),predict(mylogit,newdata=.,type="response"))), 
    #        nonzero=ifelse(rbinom(nrow(past.dat),1,prob),"nonzero","zero")) %>%
      mutate(prob = ifelse(InsCat=="Uninsured", pmin(1,factor+predict(mylogit,newdata=.,type="response")), predict(mylogit,newdata=.,type="response")),
                         # ifelse(unins.inc=="zero.increase",predict(mylogit,newdata=.,type="response"),
                         #        ifelse(unins.inc=="higher.increase",0,pmin(1,0.2+predict(mylogit,newdata=.,type="response")))),
                         # predict(mylogit,newdata=.,type="response")), 
           nonzero=ifelse(rbinom(nrow(past.dat),1,prob),"nonzero","zero")) %>%
      mutate(LY1 = ifelse(MedSpend==0,NA,log(MedSpend))) %>%
    #mutate(leaf = ifelse(Age>65,NA,ifelse(MedSpend==0,0,rpart.predict.leaves(nonzero.CART,.,type="where"))))
      mutate(leaf = ifelse(MedSpend==0,0,rpart.predict.leaves(nonzero.CART,.,type="where")))
  
  
  d <- vector(length=nrow(new))         # This will store our spending values
  nas <- which(rowSums(is.na(new[,]))>4 | is.na(new["leaf"]))
  z1 <- which(new["leaf"]==0)           # Those who had zero spending last year
  z2 <- which(new["nonzero"]=="zero");  # Those who are predicted to have zero spending this year
  nz <- which(new["nonzero"]=="nonzero" & new["MedSpend"]!=0) # Those with nonzero spending both years
  p <- which(new["Preg"]=="pregnant")   # Those who are pregnant
  
  if(length(z1)!=0){d[z1] <- predict.from.zeros.R(new[z1,])}
  if(length(z2)!=0){d[z2] <- 0} # Rows with zero spending
  if(length(nas)!=0){d[nas] <- NA} # Rows with "NA"
  
  for(i.leaf in c(1:length(partition.regressions))){
    reg <- partition.regressions[[i.leaf]]
    if(!is.null(reg)){
      smear <- smears[[i.leaf]]
      
      i.set <- which(new[,"leaf"]==i.leaf )
      sub <- new[i.set,]
      
      if(nrow(sub)!=0){
        L <- predict(reg,sub,type="response")
        r <- sample(reg$residuals,nrow(sub),replace = TRUE)[[1]]
        S <- smear*exp(L)+exp(r)
        
        d[i.set] <- S
      }
    }
  }
  
  d[p] <- d[p] + unlist(rerun(length(p),child.cost())) # This runs the child.cost() function as many times as we need
  return(d)
}

# Inflate spending of pre-Medicare population (sensitivity analysis)
inflators <- seq(0.13,0.65,length.out=15)
Age <- seq(50,64)
inflation.by.age <- data.frame(Age,inflators)

inflate.spending.pre.medicare <- function(pop){
  a <- which(pop$Age %in% inflation.by.age$Age)
  S <- pop$MedSpend
  q <- pop %>%
    dplyr::left_join(inflation.by.age,by="Age")
  S[a] <- q$MedSpend[a]*(1+q$inflators[a])
  return(S)
}



#########################
### Outdated Versions ###
#########################

# evolve.spending0 <- function(past.dat){
#   new <- past.dat %>%
#     mutate(nonzeroY1 = ifelse(MedSpend==0,"zero","nonzero"), TOTEXPY1=MedSpend, FamIncome=as.numeric(FamIncome)) %>%
#     mutate(prob = ifelse(Age>65,0,predict(mylogit,newdata=.,type="response")), nonzero=ifelse(rbinom(nrow(past.dat),1,prob),"nonzero","zero")) %>%
#     mutate(LY1 = ifelse(MedSpend==0,NA,log(MedSpend))) %>%
#     mutate(leaf = ifelse(Age>65,NA,ifelse(MedSpend==0,0,rpart.predict.leaves(nonzero.CART,.,type="where"))))
#   
#   ### CHECK ON THIS - NEED ZERO SPENDING FOR THOSE WITH NONZERO=ZERO
#   d <- vector(length=nrow(new))
#   for (i in 1:nrow(new)){
#     if(rowSums(is.na(new[i,]))>4){d[i]=NA}
#     else if (is.na(new[i,"leaf"])){d[i]=NA}
#     else if (new[i,"leaf"]==0) {d[i]=predict.from.zeros(new[i,])}
#     else if (new[i,"nonzero"]=="nonzero") {d[i]=predict.from.nonzeros(new[i,])}
#     if(new[i,"Preg"]=="pregnant"){d[i]=d[i]+child.cost()}
#    # else if (new[i,"nonzero"]=="zero") {d[i]=predict.from.zeros(new[i,])}
#   }
#   return(d)
# }
# 
# # This version is ever so slightly faster
# evolve.spending <- function(past.dat){
#   new <- past.dat %>%
#     mutate(nonzeroY1 = ifelse(MedSpend==0,"zero","nonzero"), TOTEXPY1=MedSpend, Income=as.numeric(FamIncome)) %>%
#     mutate(prob = ifelse(Age>65,0,predict(mylogit,newdata=.,type="response")), nonzero=ifelse(rbinom(nrow(past.dat),1,prob),"nonzero","zero")) %>%
#     mutate(LY1 = ifelse(MedSpend==0,NA,log(MedSpend))) %>%
#     mutate(leaf = ifelse(Age>65,NA,ifelse(MedSpend==0,0,rpart.predict.leaves(nonzero.CART,.,type="where"))))
#   
#   d <- vector(length=nrow(new))         # This will store our spending values
#   nas <- which(rowSums(is.na(new[,]))>4 | is.na(new["leaf"]))
#   z1 <- which(new["leaf"]==0)           # Those who had zero spending last year
#   z2 <- which(new["nonzero"]=="zero");  # Those who are predicted to have zero spending this year
#   nz <- which(new["nonzero"]=="nonzero" & new["MedSpend"]!=0) # Those with nonzero spending both years
#   p <- which(new["Preg"]=="pregnant")   # Those who are pregnant
#   d[nas] <- NA # Rows with "NA"
#   d[z2] <- 0 # Rows with zero spending
#   for (i in z1){
#     d[i] <- predict.from.zeros(new[i,]) # TODO RV: Adrienne this procedure can be made more efficent please see my email dated 11-19-2019 
#   }
#   for (i in nz){
#     d[i] <- predict.from.nonzeros(new[i,]) # TODO RV: Adrienne this procedure can be made more efficent please see my email dated 11-19-2019 
#   }
#   d[p] <- d[p] + unlist(rerun(length(p),child.cost())) # This runs the child.cost() function as many times as we need
#   return(d)
# }
# 
# # We switched it so now not using CART if zero spending last year
# #predict.from.nonzeros <- function(x){
# #  predict(partition.regressions[[x$leaf]],x,type="response")
# #}
# predict.from.nonzeros <- function(x){
#   reg <- partition.regressions[[x$leaf]]
#   smear <- smears[[x$leaf]]
#   L <- predict(reg,x,type="response"); r <- sample(reg$residuals,1)[[1]]
#   S <- smear*exp(L)+exp(r)
#   return(S)
# }
# predict.from.zeros <- function(x){
#   reg <- z.partition.regression
#   L <- predict(reg,x,type="response"); r <- sample(reg$residuals,1)[[1]]
#   S <- z.smear*exp(L)+exp(r)
#   return(S)
# }

# predict.nonzero.spending <- function(indiv.data){
#   preg <- child.this.year(indiv.data); indiv.data["Preg"] <- ifelse(preg, "pregnant", "not pregnant")
#   indiv.data["nonzeroY1"] <- ifelse(indiv.data["MedSpend"]==0,"zero","nonzero")
#   indiv.data["TOTEXPY1"] <- indiv.data["MedSpend"]
#   prob <- predict(mylogit, indiv.data, type="response")
#   nonzero <- ifelse(rbinom(1,1,prob), TRUE, FALSE)
#   result <- list(nonzero,preg)
#   return(result)
# }


####################################################################


#############
### Notes ###
#############

    # evolve.spending <- function(past){
    #   new <- past %>%
    #     mutate(nonzeroY1 = ifelse(MedSpend==0,"zero","nonzero"),TOTEXPY1=MedSpend) %>%
    #     mutate(prob = ifelse(Elig==0,0,predict(mylogit,newdata=.,type="response")), nonzero=ifelse(rbinom(1,1,prob),"nonzero","zero")) %>%
    #     mutate(LY1 = ifelse(MedSpend==0,NA,log(MedSpend))) %>%
    #     mutate(leaf = ifelse(Elig==0,0,ifelse(MedSpend==0,rpart.predict.leaves(zero.CART,.,type="where"),rpart.predict.leaves(nonzero.CART,.,type="where"))))
    #   return(new)}
    #   v <- vector(,nrow(new))
    #   for (indiv in 1:nrow(new)){
    #     if (a["Elig"]==0){return(NA)}
    #     a <- new[indiv,]
    #     if (a["MedSpend"]==0){
    #       reg <- z.partition.regressions[[a["leaf"][[1]]]]
    #     } else {
    #       reg <- partition.regressions[[a["leaf"][[1]]]]
    #     }
    #     v[indiv] <- predict(reg,a,type="response")+sample(reg$residuals,1)[[1]]
    #   }  
    #   new["MedSpend"] <- v
    #   new <- new[-c("nonzeroY1","TOTEXPY1","prob","nonzero","LY1","leaf")]
    # }
    # 
    # va=Vectorize(rpart.predict.leaves)
    # 
    # #evolve.spending <- function(past){
    #   new <- past %>%
    #     mutate(nonzeroY1 = ifelse(MedSpend==0,"zero","nonzero"),TOTEXPY1=MedSpend) %>%
    #     mutate(prob = ifelse(Elig==0,0,predict(mylogit,newdata=.,type="response")), nonzero=ifelse(rbinom(1000,1,prob),"nonzero","zero")) %>%
    #     mutate(LY1 = ifelse(MedSpend==0,NA,log(MedSpend))) %>%
    #     mutate(leaf = ifelse(Elig==0,0,ifelse(MedSpend==0,rpart.predict.leaves(zero.CART,.,type="where"),1)))
    #   
    #   %>%
    #     mutate(L = ifelse(leaf==3,predict(partition.regressions[[3]],.,type="response"),
    #                       ifelse(leaf==8,predict(partition.regressions[[8]],.,type="response"),
    #                            ifelse(leaf==9,predict(partition.regressions[[9]],.,type="response"),
    #                                    ifelse(leaf==10,predict(partition.regressions[[10]],.,type="response"),
    #                                            ifelse(leaf==12,predict(partition.regressions[[12]],.,type="response"),
    #                                                    ifelse(leaf==13,predict(partition.regressions[[13]],.,type="response"),
    #                                                           ifelse(leaf==4 & MedSpend!=0,predict(partition.regressions[[4]],.,type="response"),
    #                                                                  ifelse(leaf==4 & MedSpend==0,predict(z.partition.regressions[[4]],.,type="response"),
    #                                                                         ifelse(leaf==5 & MedSpend==0, predict(z.partition.regressions[[5]],.,type="response"),
    #                                                                                ifelse(leaf==2 & MedSpend==0, predict(z.partition.regressions[[2]],.,type="response"),0))))))))))) %>%
    #     mutate(r = ifelse(leaf==3,sample(partition.regressions[[3]]$residuals,1000,replace=TRUE),
    #                       ifelse(leaf==8,sample(partition.regressions[[8]]$residuals,1000,replace=TRUE),
    #                              ifelse(leaf==9,sample(partition.regressions[[9]]$residuals,1000,replace=TRUE),
    #                                     ifelse(leaf==10,sample(partition.regressions[[1]]$residuals,1000,replace=TRUE),
    #                                            ifelse(leaf==12,sample(partition.regressions[[12]]$residuals,1000,replace=TRUE),
    #                                                   ifelse(leaf==13,sample(partition.regressions[[13]]$residuals,1000,replace=TRUE),
    #                                                          ifelse(leaf==4 & MedSpend!=0,sample(partition.regressions[[4]]$residuals,1000,replace=TRUE),
    #                                                                 ifelse(leaf==4 & MedSpend==0,sample(z.partition.regressions[[4]]$residuals,1000,replace=TRUE),
    #                                                                        ifelse(leaf==5 & MedSpend==0, sample(z.partition.regressions[[5]]$residuals,1000,replace=TRUE),
    #                                                                               ifelse(leaf==2 & MedSpend==0, sample(z.partition.regressions[[2]]$residuals,1000,replace=TRUE),0))))))))))) %>%
    #     mutate(S = exp(L+r))
    # 
    # new2 <- new %>% 
    #     mutate(L = case_when(
    #       leaf==3 ~ predict(partition.regressions[[3]],.,type="response"),
    #       leaf==12 ~ predict(partition.regressions[[12]],.,type="response"),
    #       leaf==9 ~ predict(partition.regressions[[9]],.,type="response"),
    #       leaf==10 ~ predict(partition.regressions[[10]],.,type="response"),
    #       leaf==4 ~ predict(partition.regressions[[4]],.,type="response"),
    #       leaf==8 ~ predict(partition.regressions[[8]],.,type="response"),
    #       leaf==13 ~ predict(partition.regressions[[13]],.,type="response"),
    #       leaf==5 & MedSpend==0 ~ predict(z.partition.regressions[[5]],.,type="response"),
    #       leaf==2 & MedSpend==0 ~ predict(z.partition.regressions[[2]],.,type="response"),
    #       leaf==4 & MedSpend==0 ~ predict(z.partition.regressions[[4]],.,type="response"),
    #       TRUE ~ 0
    #     )
    #   )
    # 
    # 
    # new <- past %>%
    #   mutate(nonzeroY1=ifelse(MedSpend==0,"zero","nonzero"),TOTEXPY1=MedSpend) %>%
    #   mutate(prob = predict(mylogit,newdata=.,type="response"), nonzero=ifelse(rbinom(1,1,prob),"nonzero","zero")) %>%
    #   mutate(LY1 = ifelse(MedSpend==0,NA,log(MedSpend))) %>%
    #   mutate(leaf = rpart.predict.leaves(nonzero.CART,new,type="where")) %>%
    #   mutate(L = exp(predict(partition.regressions[[leaf[1]]], new, type="response"))) %>%
    #   mutate(r= sample(partition.regressions[[leaf[1]]]$residuals,10)) %>%
    #   mutate(NewSpend = exp(L+r))
    # 
    # p <- function(leaf, data){
    #   L = predict(partition.regressions[[leaf]], data, type="response")
    # }
    # 
    # new <- past %>%
    #   mutate(nonzeroY1=ifelse(MedSpend==0,"zero","nonzero"),TOTEXPY1=MedSpend) %>%
    #   mutate(prob = predict(mylogit,newdata=.,type="response"), nonzero=ifelse(rbinom(1,1,prob),"nonzero","zero")) %>%
    #   mutate(LY1 = ifelse(MedSpend==0,NA,log(MedSpend))) %>%
    #   mutate(leaf = ifelse(MedSpend==0,rpart.predict.leaves(zero.CART,new,type="where"),rpart.predict.leaves(nonzero.CART,new,type="where"))) %>%
    #   mutate(L = map2(3,., ~predict(partition.regressions[[.x]],newdata=.y,type="response")))
    #   mutate(L = predict(partition.regressions[["leaf"]], new, type="response"))
    #   mutate(L = ifelse(MedSpend==0, exp(predict(z.partition.regressions[[leaf]], new, type="response")), exp(predict(partition.regressions[[leaf]], new, type="response"))))
    #   mutate(L = exp(predict(partition.regressions[[leaf], new, type="response"))) %>%
    #   mutate(r= sample(partition.regressions[[leaf]$residuals,10)) %>%
    #   mutate(NewSpend = exp(L+r))
    # 
    # # THIS ONE IS GOOD!!!!!
    # 
    #   #%>%
    #     mutate(L = ifelse(MedSpend==0, 0, pred_v(map(leaf, ~partition.regressions[[.x]]),newdata=.,type="response")))
    #     #mutate(L = ifelse(MedSpend==0, 0, pred_v(map(leaf, ~partition.regressions[[.]],newdata=new,type="response")))
    #   
    # check: predict(partition.regressions[[3]],past[1,],type="response")
    #     mutate(L = ifelse(MedSpend==0, 0, pred_v(map(leaf, ~partition.regressions[.x]),newdata=.,type="response")))
    #   
    # pred_v <- Vectorize(predict)
    # 
    # v=c()
    # for (indiv in 1:nrow(new)){
    #   a <- new[indiv,]
    #   if (a["MedSpend"]==0){
    #     reg <- z.partition.regressions[[a["leaf"][[1]]]]
    #   } else {
    #     reg <- partition.regressions[[a["leaf"][[1]]]]
    #   }
    #   v[indiv] <- predict(reg,a,type="response")+sample(reg$residuals,1)[[1]]
    # }  
    #   
    #   %>%
    #     rowwise() %>%
    #     mutate(r <- predict(partition.regressions[[leaf]]))
    #   
    #   new <- past1 %>%
    #     mutate(nonzeroY1=ifelse(MedSpend==0,"zero","nonzero"),TOTEXPY1=MedSpend) %>%
    #     mutate(prob = predict(mylogit,newdata=.,type="response"), nonzero=ifelse(rbinom(1,1,prob),"nonzero","zero")) %>%
    #     mutate(LY1 = ifelse(MedSpend==0,NA,log(MedSpend))) %>%
    #     mutate(leaf = ifelse(MedSpend==0,rpart.predict.leaves(zero.CART,new,type="where"),rpart.predict.leaves(nonzero.CART,new,type="where"))) %>%
    #     mutate(L = ifelse(MedSpend==0, predict(na.omit(map(leaf, ~z.partition.regressions[[.x]]),newdata=.,type="response")),na.omit(predict(map(leaf, ~partition.regressions[[.x]]),newdata=.,type="response"))))
    #   
    #   %>%
    #     mutate(L = ifelse(MedSpend==0, predict(map(leaf, ~z.partition.regressions[[.x]]),newdata=.,type="response"),predict(map(leaf, ~partition.regressions[[.x]]),newdata=.,type="response")))
    #   
    # %>%
    #   mutate()
    #   mutate(SpendNext = ifelse(nonzero=="zero"),0,ifelse(MedSpend==0,DOSOMETHINGWITHZEROS,))
    #   
    # p <- function(a){
    #   L = ifelse(a["MedSpend"]==0, predict(z.partition.regressions[[a["leaf"][[1]]]],newdata=a,type="response"),predict(partition.regressions[[a["leaf"][[1]]]],newdata=a,type="response"))
    #   return(L)
    # }
    # 
    # predict.spending <- function(indiv.data){
    #   result <- predict.nonzero.spending(indiv.data); nonzero <- result[[1]]; preg <- result[[2]]
    #   if (nonzero==FALSE){
    #     prediction <- 0
    #   } else {
    #     indiv.data["LY1"] <- log(indiv.data["MedSpend"])
    #     reg <- partition.regressions[[rpart.predict.leaves(nonzero.CART, indiv.data, type="where")]]
    #     L <- predict(reg, indiv.data, type="response")[[1]]
    #     #prediction <- exp(L) + exp(sample(reg$residuals, 1)[[1]])
    #     prediction <- exp(L+sample(reg$residuals,1)[[1]])
    #     #prediction <- L + sample(reg$residuals, 1)[[1]]
    #   }
    #   if (preg==TRUE){
    #     prediction <- prediction + child.cost()
    #   }
    #   return(prediction)
    # }
    # 
    # # This was explored as an alternative
    # predict.spending <- function(indiv.data){
    #   #nonzero <- predict.nonzero.spending(indiv.data)
    #   #if (nonzero==FALSE){
    #   #  prediction <- 0
    #   #} else {
    #     b=rpart.predict.leaves(nonzero.CART, indiv.data, type="where")[[1]]
    #     #reg <- partition.regressions[[4]]
    #     #L <- predict(reg, indiv.data, type="response")[[1]]
    #     #prediction <- L + sample(reg$residuals, 1)[[1]]
    #   #}
    #   #return(prediction)
    # }
    # 
    # total <- 990; pb <- txtProgressBar(min = 0, max = total, style = 3)
    # for (i in 1:990){
    #   indiv.data=dat.np.both[i,]
    #   dat.np.both$PRED[i]=predict.spending(indiv.data)
    #   setTxtProgressBar(pb, i)
    # }
    # close(pb)
    # 
    # d <- dat.np.both %>% mutate(c=PRED*LONGWT)
