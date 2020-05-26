#cal portfolio average
#setwd("d:/SharedFolder/myR/Bloomberg")
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")



mAll = read.csv("mAll.csv",header = T)


############################ 
#remove outliners group the following portfolioindex
#market cap outliner at  X152:2014-02-13
#fturnover outliner at X273:2012-08-21  X1003:2012-06-18  X1383:2012-06-04 X273:2011-12-23
#frisk: X767:2011-01-10
#fprofit: X8116:2014-06-03
#plev: X1159:2014-09-12
#ftobinq : X767:2011-01-10
#outliners= c("X1159:2014-09-12")

#a<-which((mAll$PortfolioIndex %in% outliners)==TRUE)
#mAll<-mAll[-a,]
#clear 1% and 99% of all values  ********Windorize the data*********
i = 8L
for(i in 8L:9L){
  q1Q = quantile(mAll[,i],0.99)
  q1= which(mAll[,i] > q1Q)
  mAll[q1,i] = q1Q
  
}
for(i in 11L:27L){
  q1Q = quantile(mAll[,i],0.99)
  q1= which(mAll[,i] > q1Q)
  mAll[q1,i] = q1Q
  
}


############### self difference calculation ########################
mAll$X_risk = mAll$frisk-mAll$prisk
mAll$X_spread = mAll$fspread-mAll$pspread
mAll$X_profit = mAll$fProfit-mAll$pProfit
mAll$X_ROA = mAll$fROA-mAll$pROA
mAll$X_lev = mAll$fLev-mAll$pLev
mAll$X_illiquid = mAll$filliquid-mAll$pilliquid
mAll$X_turnover= mAll$fTurnover-mAll$pTurnover
mAll$X_tobinq = mAll$fTobinQ-mAll$pTobinQ
mAll$pNII = ((mAll$NIIQp1+mAll$NIIQp2+mAll$NIIQp3+mAll$NIIQp4)/4)
mAll$fNII = ((mAll$NIIQn5+mAll$NIIQn6+mAll$NIIQn7+mAll$NIIQn8)/4)
mAll$X_NII = mAll$fNII - mAll$pNII  
mAll$X_LNII = log10(1+mAll$fNII)- log10(1+mAll$pNII)

mAll$pNII2Q = ((mAll$NIIQp1+mAll$NIIQp2)/2)
mAll$fNII2Q = ((mAll$NIIQn5+mAll$NIIQn6)/2)
mAll$X_NII2Q = mAll$fNII2Q - mAll$pNII2Q  
mAll$X_LNII2Q = log10(1+mAll$fNII2Q)- log10(1+mAll$pNII2Q)

mAll$pPIH = ((mAll$PIHQp1+mAll$PIHQp2+mAll$PIHQp3+mAll$PIHQp4)/4)
mAll$fPIH = ((mAll$PIHQn5+mAll$PIHQn6+mAll$PIHQn7+mAll$PIHQn8)/4)
mAll$X_PIH = mAll$fPIH - mAll$pPIH  
mAll$X_LPIH = log10(1+mAll$fPIH)- log10(1+mAll$pPIH)

mAll$pPIH2Q = ((mAll$PIHQp1+mAll$PIHQp2)/2)
mAll$fPIH2Q = ((mAll$PIHQn5+mAll$PIHQn6)/2)
mAll$X_PIH2Q = mAll$fPIH2Q - mAll$pPIH2Q  
mAll$X_LPIH2Q = log10(1+mAll$fPIH2Q)- log10(1+mAll$pPIH2Q)

mAll$pNIS = ((mAll$NISQp1+mAll$NISQp2+mAll$NISQp3+mAll$NISQp4)/4)
mAll$fNIS = ((mAll$NISQn5+mAll$NISQn6+mAll$NISQn7+mAll$NISQn8)/4)
mAll$X_NIS = mAll$fNIS - mAll$pNIS  
mAll$X_LNIS = log10(1+mAll$fNIS)- log10(1+mAll$pNIS)

mAll$pNIS2Q = ((mAll$NISQp1+mAll$NISQp2)/2)
mAll$fNIS2Q = ((mAll$NISQn5+mAll$NISQn6)/2)
mAll$X_NIS2Q = mAll$fNIS2Q - mAll$pNIS2Q  
mAll$X_LNIS2Q = log10(1+mAll$fNIS2Q)- log10(1+mAll$pNIS2Q)


mAll$pPSH = ((mAll$PSHQp1+mAll$PSHQp2+mAll$PSHQp3+mAll$PSHQp4)/4)
mAll$fPSH = ((mAll$PSHQn5+mAll$PSHQn6+mAll$PSHQn7+mAll$PSHQn8)/4)
mAll$X_PSH = mAll$fPSH - mAll$pPSH  
mAll$X_LPSH = log10(1+mAll$fPSH)- log10(1+mAll$pPSH)

mAll$pPSH2Q = ((mAll$PSHQp1+mAll$PSHQp1)/2)
mAll$fPSH2Q = ((mAll$PSHQn5+mAll$PSHQn6)/2)
mAll$X_PSH2Q = mAll$fPSH2Q - mAll$pPSH2Q  
mAll$X_LPSH2Q = log10(1+mAll$fPSH2Q)- log10(1+mAll$pPSH2Q)


mAll$pNAZ = ((mAll$NAZQp1+mAll$NAZQp2+mAll$NAZQp3+mAll$NAZQp4)/4)
mAll$fNAZ = ((mAll$NAZQn5+mAll$NAZQn6+mAll$NAZQn7+mAll$NAZQn8)/4)
mAll$X_NAZ = mAll$fNAZ - mAll$pNAZ  
mAll$X_LNAZ = log10(1+mAll$fNAZ)- log10(1+mAll$pNAZ)

mAll$pNAZ2Q = ((mAll$NAZQp1+mAll$NAZQp2)/2)
mAll$fNAZ2Q = ((mAll$NAZQn5+mAll$NAZQn6)/2)
mAll$X_NAZ2Q = mAll$fNAZ2Q - mAll$pNAZ2Q  
mAll$X_LNAZ2Q = log10(1+mAll$fNAZ2Q)- log10(1+mAll$pNAZ2Q)

group <- mAll
group$Date <- as.character(group$Date)
group$Date<- as.Date(group$Date)

group$treatedID<-as.character(group$treatedID)

grp <- as.character(group$treatedID)
ugrp <-unique(grp)
a <- which(group$RSS == 0)
group2 <- group[a,]
group2$Code <- as.character(group2$Code)
group2$treatedID <- as.character(group2$treatedID)
lessgrp = 0
j = 1L

for(j in 1L: length(ugrp)){
  
  c <-which(group$treatedID == ugrp[j]) #include RSS
  b <- which(group2$treatedID == ugrp[j]) #exclude RSS
  if(length(b)<5){
   lessgrp=lessgrp+1
   print(ugrp[j])
  }
  #print(paste(as.character(j),as.character(length(b)),sep=":"))
  group[c,"MarketCapM"] <- mean(group2[b,"MarketCapM"])
  group[c,"pm11Return"] <- mean(group2[b,"pm11Return"])
  group[c,"Book2Market"] <- mean(group2[b,"Book2Market"])
  group[c,"m12Return"] <- mean(group2[b,"m12Return"])
  group[c,"Return24months"] <- mean(group2[b,"Return24months"])
  group[c,"D_Return24months"] <- mean((group2[,"Return24months"] - group[c,"Return24months"]))
  group[c,"mktReturn"] <- mean(group2[b,"mktReturn"])
  group[c,"pilliquid"] <- mean(group2[b,"pilliquid"])
  group[c,"filliquid"] <- mean(group2[b,"filliquid"])
  group[c,"pLev"] <- mean(group2[b,"pLev"])
  group[c,"fLev"] <- mean(group2[b,"fLev"]) 
  group[c,"prisk"] <- mean(group2[b,"prisk"])
  group[c,"frisk"] <- mean(group2[b,"frisk"])
  group[c,"pROA"] <- mean(group2[b,"pROA"])
  group[c,"fROA"] <- mean(group2[b,"fROA"])
  group[c,"pProfit"] <- mean(group2[b,"pProfit"])
  group[c,"fProfit"] <- mean(group2[b,"fProfit"])
  group[c,"pTobinQ"] <- mean(group2[b,"pTobinQ"])
  group[c,"fTobinQ"] <- mean(group2[b,"fTobinQ"])
  group[c,"pTurnover"] <- mean(group2[b,"pTurnover"])
  group[c,"fTurnover"] <- mean(group2[b,"fTurnover"])
  group[c,"fspread"] <- mean(group2[b,"fspread"])
  group[c,"pspread"] <- mean(group2[b,"pspread"])
  
  group[c,"X_risk"] = mean(group2[b,"X_risk"])
  group[c,"X_spread"] = mean(group2[b,"X_spread"])
  group[c,"X_profit"] = mean(group2[b,"X_profit"])
  group[c,"X_ROA"] = mean(group2[b,"X_ROA"])
  group[c,"X_lev"] = mean(group2[b,"X_lev"])
  group[c,"X_illiquid"] = mean(group2[b,"X_illiquid"])
  group[c,"X_turnover"]= mean(group2[b,"X_turnover"])
  group[c,"X_tobinq"] =   mean(group2[b,"X_tobinq"])
  group[c,"distance"] <- mean(group2[b,"distance"])
  #  group[c,"SplitRatio"] <- mean(group2[b,"SplitRatio"])
  #NII
  group[c,"pNII"] <- mean(group2[b,"pNII"])
  group[c,"fNII"] <- mean(group2[b,"fNII"])
  group[c,"X_NII"] <- mean(group2[b,"X_NII"])
  group[c,"X_LNII"] <- mean(group2[b,"X_LNII"])
  group[c,"pPIH"] <- mean(group2[b,"pPIH"])
  group[c,"fPIH"] <- mean(group2[b,"fPIH"])
  group[c,"X_PIH"] <- mean(group2[b,"X_PIH"])
  group[c,"X_LPIH"] <- mean(group2[b,"X_LPIH"])
  group[c,"pNIS"] <- mean(group2[b,"pNIS"])
  group[c,"fNIS"] <- mean(group2[b,"fNIS"])
  group[c,"X_NIS"] <- mean(group2[b,"X_NIS"])
  group[c,"X_LNIS"] <- mean(group2[b,"X_LNIS"])
  group[c,"pPSH"] <- mean(group2[b,"pPSH"])
  group[c,"fPSH"] <- mean(group2[b,"fPSH"])
  group[c,"X_PSH"] <- mean(group2[b,"X_PSH"])
  group[c,"X_LPSH"] <- mean(group2[b,"X_LPSH"])
  
  group[c,"pNAZ"] <- mean(group2[b,"pNAZ"])
  group[c,"fNAZ"] <- mean(group2[b,"fNAZ"])
  group[c,"X_NAZ"] <- mean(group2[b,"X_NAZ"])
  group[c,"X_LNAZ"] <- mean(group2[b,"X_LNAZ"])
  
  group[c,"pNII2Q"] <- mean(group2[b,"pNII2Q"])
  group[c,"fNII2Q"] <- mean(group2[b,"fNII2Q"])
  group[c,"X_NII2Q"] <- mean(group2[b,"X_NII2Q"])
  group[c,"X_LNII2Q"] <- mean(group2[b,"X_LNII2Q"])
  group[c,"pPIH2Q"] <- mean(group2[b,"pPIH2Q"])
  group[c,"fPIH2Q"] <- mean(group2[b,"fPIH2Q"])
  group[c,"X_PIH2Q"] <- mean(group2[b,"X_PIH2Q"])
  group[c,"X_LPIH2Q"] <- mean(group2[b,"X_LPIH2Q"])
  group[c,"pNIS2Q"] <- mean(group2[b,"pNIS2Q"])
  group[c,"fNIS2Q"] <- mean(group2[b,"fNIS2Q"])
  group[c,"X_NIS2Q"] <- mean(group2[b,"X_NIS2Q"])
  group[c,"X_LNIS2Q"] <- mean(group2[b,"X_LNIS2Q"])
  group[c,"pPSH2Q"] <- mean(group2[b,"pPSH2Q"])
  group[c,"fPSH2Q"] <- mean(group2[b,"fPSH2Q"])
  group[c,"X_PSH2Q"] <- mean(group2[b,"X_PSH2Q"])
  group[c,"X_LPSH2Q"] <- mean(group2[b,"X_LPSH2Q"])
  
  group[c,"pNAZ2Q"] <- mean(group2[b,"pNAZ2Q"])
  group[c,"fNAZ2Q"] <- mean(group2[b,"fNAZ2Q"])
  group[c,"X_NAZ2Q"] <- mean(group2[b,"X_NAZ2Q"])
  group[c,"X_LNAZ2Q"] <- mean(group2[b,"X_LNAZ2Q"])
  
}
group$X <- NULL
write.csv(group,"mAllavg.csv")  #save portfolio avg value

# calculate difference in difference

mDall <- mAll

#for(j in 14L:24L){
#  mDall[,j]= mAll[,j]- group[,j]
#}

#mDall[,"MarketCapM"] <- mDall[,"MarketCapM"] 
mDall[,"D_Return24months"] <- mDall[,"Return24months"] - group[,"Return24months"]
mDall[,"D_m12Return"] <- mDall[,"m12Return"] - group[,"m12Return"]
mDall[,"D_pm11Return"] <- mDall[,"pm11Return"] - group[,"pm11Return"]
mDall[,"D_Book2Market"] <- mDall[,"Book2Market"] - group[,"Book2Market"]
mDall[,"D_mktReturn"] <- mDall[,"mktReturn"] - group[,"mktReturn"]
mDall[,"D_pilliquid"] <- mDall[,"pilliquid"] - group[,"pilliquid"]
mDall[,"D_filliquid"] <- mDall[,"filliquid"] - group[,"filliquid"]
mDall[,"D_pLev"] <- mDall[,"pLev"] - group[,"pLev"]
mDall[,"D_fLev"] <- mDall[,"fLev"] - group[,"fLev"]
mDall[,"D_pProfit"] <- mDall[,"pProfit"] - group[,"pProfit"]
mDall[,"D_fProfit"] <- mDall[,"fProfit"] - group[,"fProfit"]
mDall[,"D_prisk"] <- mDall[,"prisk"] - group[,"prisk"]
mDall[,"D_frisk"] <- mDall[,"frisk"] - group[,"frisk"]
mDall[,"D_pROA"] <- mDall[,"pROA"] - group[,"pROA"]
mDall[,"D_fROA"] <- mDall[,"fROA"] - group[,"fROA"]
mDall[,"D_pTobinQ"] <- mDall[,"pTobinQ"] - group[,"pTobinQ"]
mDall[,"D_fTobinQ"] <- mDall[,"fTobinQ"] - group[,"fTobinQ"]
mDall[,"D_pTurnover"] <- mDall[,"pTurnover"] - group[,"pTurnover"]
mDall[,"D_fTurnover"] <- mDall[,"fTurnover"] - group[,"fTurnover"]
mDall[,"D_pspread"] <- mDall[,"pspread"] - group[,"pspread"]
mDall[,"D_fspread"] <- mDall[,"fspread"] - group[,"fspread"]
mDall[,"D_pNII"]<- mDall[,"pNII"] - group[,"pNII"]
mDall[,"D_fNII"]<-mDall[,"fNII"] - group[, "fNII"]
mDall[,"D_pPIH"]<- mDall[,"pPIH"] - group[,"pPIH"]
mDall[,"D_fPIH"]<-mDall[,"fPIH"] - group[, "fPIH"]
mDall[,"D_pNIS"]<- mDall[,"pNIS"] - group[,"pNIS"]
mDall[,"D_fNIS"]<-mDall[,"fNIS"] - group[, "fNIS"]
mDall[,"D_pPSH"]<- mDall[,"pPSH"] - group[,"pPSH"]
mDall[,"D_fPSH"]<-mDall[,"fPSH"] - group[, "fPSH"]
mDall[,"D_pNAZ"]<- mDall[,"pNAZ"] - group[,"pNAZ"]
mDall[,"D_fNAZ"]<-mDall[,"fNAZ"] - group[, "fNAZ"]

mDall[,"D_pNII2Q"]<- mDall[,"pNII2Q"] - group[,"pNII2Q"]
mDall[,"D_fNII2Q"]<-mDall[,"fNII2Q"] - group[, "fNII2Q"]
mDall[,"D_pPIH2Q"]<- mDall[,"pPIH2Q"] - group[,"pPIH2Q"]
mDall[,"D_fPIH2Q"]<-mDall[,"fPIH2Q"] - group[, "fPIH2Q"]
mDall[,"D_pNIS2Q"]<- mDall[,"pNIS2Q"] - group[,"pNIS2Q"]
mDall[,"D_fNIS2Q"]<-mDall[,"fNIS2Q"] - group[, "fNIS2Q"]
mDall[,"D_pPSH2Q"]<- mDall[,"pPSH2Q"] - group[,"pPSH2Q"]
mDall[,"D_fPSH2Q"]<-mDall[,"fPSH2Q"] - group[, "fPSH2Q"]
mDall[,"D_pNAZ2Q"]<- mDall[,"pNAZ2Q"] - group[,"pNAZ2Q"]
mDall[,"D_fNAZ2Q"]<-mDall[,"fNAZ2Q"] - group[, "fNAZ2Q"]


mDall[,"DD_Return24months"] <- mDall[,"D_Return24months"] - group[,"D_Return24months"]

mDall[,"DD_risk"] <- mDall[,"X_risk"] - group[,"X_risk"]
mDall[,"DD_spread"] <- mDall[,"X_spread"] - group[,"X_spread"]
mDall[,"DD_profit"] <- mDall[,"X_profit"] - group[,"X_profit"]
mDall[,"DD_ROA"] <- mDall[,"X_ROA"] - group[,"X_ROA"]
mDall[,"DD_lev"] <- mDall[,"X_lev"] - group[,"X_lev"]
mDall[,"DD_illiquid"] <- mDall[,"X_illiquid"] - group[,"X_illiquid"]
mDall[,"DD_turnover"] <- mDall[,"X_turnover"] - group[,"X_turnover"]
mDall[,"DD_tobinq"] <- mDall[,"X_tobinq"] - group[,"X_tobinq"]

mDall[,"DD_NII"]<- mDall[,"X_NII"] - group[,"X_NII"]
mDall[,"DD_LNII"]<-mDall[,"X_LNII"] - group[, "X_LNII"]
mDall[,"DD_PIH"]<- mDall[,"X_PIH"] - group[,"X_PIH"]
mDall[,"DD_LPIH"]<-mDall[,"X_LPIH"] - group[, "X_LPIH"]

mDall[,"DD_NIS"]<- mDall[,"X_NIS"] - group[,"X_NIS"]
mDall[,"DD_LNIS"]<-mDall[,"X_LNIS"] - group[, "X_LNIS"]
mDall[,"DD_PSH"]<- mDall[,"X_PSH"] - group[,"X_PSH"]
mDall[,"DD_LPSH"]<-mDall[,"X_LPSH"] - group[, "X_LPSH"]
mDall[,"DD_NAZ"]<- mDall[,"X_NAZ"] - group[,"X_NAZ"]
mDall[,"DD_LNAZ"]<-mDall[,"X_LNAZ"] - group[, "X_LNAZ"]

mDall[,"DD_NII2Q"]<- mDall[,"X_NII2Q"] - group[,"X_NII2Q"]
mDall[,"DD_LNII2Q"]<-mDall[,"X_LNII2Q"] - group[, "X_LNII2Q"]
mDall[,"DD_PIH2Q"]<- mDall[,"X_PIH2Q"] - group[,"X_PIH2Q"]
mDall[,"DD_LPIH2Q"]<-mDall[,"X_LPIH2Q"] - group[, "X_LPIH2Q"]

mDall[,"DD_NIS2Q"]<- mDall[,"X_NIS2Q"] - group[,"X_NIS2Q"]
mDall[,"DD_LNIS2Q"]<-mDall[,"X_LNIS2Q"] - group[, "X_LNIS2Q"]
mDall[,"DD_PSH2Q"]<- mDall[,"X_PSH2Q"] - group[,"X_PSH2Q"]
mDall[,"DD_LPSH2Q"]<-mDall[,"X_LPSH2Q"] - group[, "X_LPSH2Q"]
mDall[,"DD_NAZ2Q"]<- mDall[,"X_NAZ2Q"] - group[,"X_NAZ2Q"]
mDall[,"DD_LNAZ2Q"]<-mDall[,"X_LNAZ2Q"] - group[, "X_LNAZ2Q"]


mDall$X<- NULL
write.csv(mDall,"mAllDID.csv")  #save portfolio avg value

# calculate delta after DID

