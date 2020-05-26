#Get  
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("ABtemp1.csv",header = T)
temp$X<-NULL

pdataAll <- NULL


datafile <- c("AB1temp24mR4.csv",
              "AB1tempF2filliquid15.csv",
              "AB1tempF2fspread12.csv",
              "AB1tempF2NIIQ0-16.csv",
              "AB1tempF2NIIQn5-16.csv",
              "AB1tempF2NIIQn6-16.csv",
              "AB1tempF2NIIQn7-16.csv",
              "AB1tempF2NIIQn8-16.csv",
              "AB1tempF2NIIQp1-16.csv",
              "AB1tempF2NIIQp2-16.csv",
              "AB1tempF2NIIQp3-16.csv",
              "AB1tempF2NIIQp4-16.csv",
              "AB1tempF2PIHQ0-19.csv",
              "AB1tempF2PIHQn5-19.csv",
              "AB1tempF2PIHQn6-19.csv",
              "AB1tempF2PIHQn7-19.csv",
              "AB1tempF2PIHQn8-19.csv",
              "AB1tempF2PIHQp1-19.csv",
              "AB1tempF2PIHQp2-19.csv",
              "AB1tempF2PIHQp3-19.csv",
              "AB1tempF2PIHQp4-19.csv",
              "AB1tempF2NISQ0-20.csv",
              "AB1tempF2NISQn5-20.csv",
              "AB1tempF2NISQn6-20.csv",
              "AB1tempF2NISQn7-20.csv",
              "AB1tempF2NISQn8-20.csv",
              "AB1tempF2NISQp1-20.csv",
              "AB1tempF2NISQp2-20.csv",
              "AB1tempF2NISQp3-20.csv",
              "AB1tempF2NISQp4-20.csv",
              "AB1tempF2PSHQ0-21.csv",
              "AB1tempF2PSHQn5-21.csv",
              "AB1tempF2PSHQn6-21.csv",
              "AB1tempF2PSHQn7-21.csv",
              "AB1tempF2PSHQn8-21.csv",
              "AB1tempF2PSHQp1-21.csv",
              "AB1tempF2PSHQp2-21.csv",
              "AB1tempF2PSHQp3-21.csv",
              "AB1tempF2PSHQp4-21.csv",
              "AB1tempF2NAZQ0-22.csv",
              "AB1tempF2NAZQn5-22.csv",
              "AB1tempF2NAZQn6-22.csv",
              "AB1tempF2NAZQn7-22.csv",
              "AB1tempF2NAZQn8-22.csv",
              "AB1tempF2NAZQp1-22.csv",
              "AB1tempF2NAZQp2-22.csv",
              "AB1tempF2NAZQp3-22.csv",
              "AB1tempF2NAZQp4-22.csv",
              "AB1tempF2pilliquid15.csv",
              "AB1tempF2prisk14.csv",
              "AB1tempF2pspread12.csv",
              "AB1tempF2pturnover13.csv",
              "AB1tempF2risk14.csv",
              "AB1tempF2turnover13.csv",
              "AB1tempFBooktoMarket3.csv",
              "AB1tempFfilliquid6.csv",
              "AB1tempFfpLev10.csv",
              "AB1tempFfpROA9.csv",
              "AB1tempFfProf10.csv",
              "AB1tempFfpTobinQ10.csv",
              "AB1tempFfROA9.csv",
              "AB1tempFfspread5.csv",
              "AB1tempFfTobinQ10.csv",
              "AB1tempFmktcap1.csv",
              "AB1tempFmktReturn11.csv",
              "AB1tempFpfLev10.csv",
              "AB1tempFpilliquid6.csv",
              "AB1tempFpProfit10.csv",
              "AB1tempFprisk7.csv",
              "AB1tempFpspread5.csv",
              "AB1tempFpturnover8.csv",
              "AB1tempFrisk7.csv",
              "AB1tempFturnover8.csv",
              "AB1tempP11mR2.csv",
             "AB1tempBQprice18.csv",
              "AB1temp12mR17.csv"
)

dataname <- c("Return24months",
              "f2illiquid",
              "f2spread",
              "NIIQ0",
              "NIIQn5",
              "NIIQn6",
              "NIIQn7",
              "NIIQn8",
              "NIIQp1",
              "NIIQp2",
              "NIIQp3",
              "NIIQp4",
              "PIHQ0",
              "PIHQn5",
              "PIHQn6",
              "PIHQn7",
              "PIHQn8",
              "PIHQp1",
              "PIHQp2",
              "PIHQp3",
              "PIHQp4",
              "NISQ0",
              "NISQn5",
              "NISQn6",
              "NISQn7",
              "NISQn8",
              "NISQp1",
              "NISQp2",
              "NISQp3",
              "NISQp4",
              "PSHQ0",
              "PSHQn5",
              "PSHQn6",
              "PSHQn7",
              "PSHQn8",
              "PSHQp1",
              "PSHQp2",
              "PSHQp3",
              "PSHQp4",
              "NAZQ0",
              "NAZQn5",
              "NAZQn6",
              "NAZQn7",
              "NAZQn8",
              "NAZQp1",
              "NAZQp2",
              "NAZQp3",
              "NAZQp4",
              "p2illiquid",
              "p2risk",
              "p2spread",
              "P2Turnover",
              "f2risk",
              "f2Turnover",
              "Book2Market",
              "filliquid",
              "pLev",
              "pROA",
              "fProfit",
              "pTobinQ",
              "fROA",
              "fspread",
              "fTobinQ",
              "MarketCapM",
              "mktReturn",
              "fLev",
              "pilliquid",
              "pProfit",
              "prisk",
              "pspread",
              "pTurnover",
              "frisk",
              "fTurnover",
              "pm11Return",
             "BQprice",
              "m12Return"
)

obs = nrow(temp)   #180 observations
nVar <- length(datafile)  # no. of variables

myfiles = lapply(datafile, read.csv)


#pd1= read.csv("temp1BooktoMarket.csv",header = T)
#mycodes <- mycolnames[a]
#my2codes <- rep(mycodes,190)
#a <-which(mycol == "X1999")
#pd2= read.csv("temp2BooktoMarket.csv",header = T)
#mycol= colnames(pd2)
#a <-which(mycol == "X84602")

pdAll<-as.data.frame(myfiles[1])

i = 2L
for(i in 2L: length(myfiles)){
  a <- as.data.frame(myfiles[i])
  pdAll<-rbind(pdAll, a[,])
  
}
nrow(pdAll)
pdAll$X <- NULL
write.csv(pdAll,"pdAll.csv")
mycolnames= colnames(pdAll)
CodeDate <-pdAll$Date1
Code <-mycolnames



DateL <- as.character(pdAll$Date1[1:obs])
mDate <- data.frame("Date" = matrix(unlist(DateL), nrow=length(DateL), byrow=T))

pdataAll<-NULL

j = 2L
#last = (ncol(pdAll2)+1)/2 - 1
for(j in 2L:ncol(pdAll)){
  mCode <- c(rep(Code[(j)],obs))
  pdata<- data.frame("Code" = matrix(unlist(mCode), nrow=length(mCode), byrow=T))
  
  pdata <-cbind(pdata,mDate)
  i = 1L
  for(i in 1L:nVar){
    num = ((i-1)*obs)+1
    mD <- pdAll[num:(num+obs-1),j]
    mDL <- data.frame("mData" =matrix(unlist(mD), nrow=length(mD), byrow=T))
    pdata <-cbind(pdata,mDL)
  }
  if(is.null(pdataAll)==TRUE){
    pdataAll <- pdata
  }else{
    pdataAll <- rbind(pdataAll,pdata)
  }
  print(as.character(j))
  
}
pdataAll$X<-NULL

colnames(pdataAll)<-c("Code","Date",dataname)
str(pdataAll)

############################## verify data with original 
#library(compare)
#library(compareDF)


#i = 1L
#for(i in 1L: length(myfiles)){
#  b <- as.data.frame(myfiles[i])
#  b$X<-NULL
#  if(compare(pdataAll[1:230,2+i],b[1:230,2])$result==FALSE) print(as.character(i))
  
  
#}



################################################

write.csv(pdataAll,"pdataAll1.csv")


a1 <- which(is.na(pdataAll$Return24months)==TRUE)
a2 <- which(is.na(pdataAll$Book2Market)==TRUE)
a3 <- which(is.na(pdataAll$filliquid)==TRUE)
a4 <- which(is.na(pdataAll$pLev)==TRUE)
a5 <- which(is.na(pdataAll$pROA)==TRUE)
a6 <- which(is.na(pdataAll$fProfit)==TRUE)
a7 <- which(is.na(pdataAll$pTobinQ)==TRUE)
a8 <- which(is.na(pdataAll$fROA)==TRUE)
a9 <- which(is.na(pdataAll$fspread)==TRUE)
a10 <- which(is.na(pdataAll$fTobinQ)==TRUE)
a11 <- which(is.na(pdataAll$MarketCapM)==TRUE)
a12 <- which(is.na(pdataAll$fLev)==TRUE)
a13 <- which(is.na(pdataAll$pilliquid)==TRUE)
a14 <- which(is.na(pdataAll$pProfit)==TRUE)
a15 <- which(is.na(pdataAll$prisk)==TRUE)
a16 <- which(is.na(pdataAll$pspread)==TRUE)
a17 <- which(is.na(pdataAll$pTurnover)==TRUE)
a18 <- which(is.na(pdataAll$frisk)==TRUE)
a19 <- which(is.na(pdataAll$fTurnover)==TRUE)
a20 <- which(is.na(pdataAll$mktReturn)==TRUE)
a21 <- which(is.na(pdataAll$pm11Return)==TRUE)
#a22 <- which(is.na(pdataAll$NISQ0)==TRUE)
#a23 <- which(is.na(pdataAll$NISQn5)==TRUE)
#a24 <- which(is.na(pdataAll$NISQn6)==TRUE)
#a25 <- which(is.na(pdataAll$NISQn7)==TRUE)
#a26 <- which(is.na(pdataAll$NISQn8)==TRUE)
#a27 <- which(is.na(pdataAll$NISQp1)==TRUE)
#a28 <- which(is.na(pdataAll$NISQp2)==TRUE)
#a29 <- which(is.na(pdataAll$NISQp3)==TRUE)
#a30 <- which(is.na(pdataAll$NISQp4)==TRUE)


nalist <- c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) # ,a22,a23,a24,a25,a26,a27,a28,a29,a30
unique_nalist <- unique(nalist)
pdataAll3 <- pdataAll[-unique_nalist,]
write.csv(pdataAll3,"pdataAllClean.csv")

##########################################################################################################
#                                    2 f
##############################################################################################################
rm(list = ls())
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("ABtemp2.csv",header = T)

pdataAll <- NULL
datafile <- c("AB2temp24mR4.csv",
              "AB2tempF2filliquid15.csv",
              "AB2tempF2fspread12.csv",
              "AB2tempF2NIIQ0-16.csv",
              "AB2tempF2NIIQn5-16.csv",
              "AB2tempF2NIIQn6-16.csv",
              "AB2tempF2NIIQn7-16.csv",
              "AB2tempF2NIIQn8-16.csv",
              "AB2tempF2NIIQp1-16.csv",
              "AB2tempF2NIIQp2-16.csv",
              "AB2tempF2NIIQp3-16.csv",
              "AB2tempF2NIIQp4-16.csv",
              "AB2tempF2PIHQ0-19.csv",
              "AB2tempF2PIHQn5-19.csv",
              "AB2tempF2PIHQn6-19.csv",
              "AB2tempF2PIHQn7-19.csv",
              "AB2tempF2PIHQn8-19.csv",
              "AB2tempF2PIHQp1-19.csv",
              "AB2tempF2PIHQp2-19.csv",
              "AB2tempF2PIHQp3-19.csv",
              "AB2tempF2PIHQp4-19.csv",
              "AB2tempF2NISQ0-20.csv",
              "AB2tempF2NISQn5-20.csv",
              "AB2tempF2NISQn6-20.csv",
              "AB2tempF2NISQn7-20.csv",
              "AB2tempF2NISQn8-20.csv",
              "AB2tempF2NISQp1-20.csv",
              "AB2tempF2NISQp2-20.csv",
              "AB2tempF2NISQp3-20.csv",
              "AB2tempF2NISQp4-20.csv",
              "AB2tempF2PSHQ0-21.csv",
              "AB2tempF2PSHQn5-21.csv",
              "AB2tempF2PSHQn6-21.csv",
              "AB2tempF2PSHQn7-21.csv",
              "AB2tempF2PSHQn8-21.csv",
              "AB2tempF2PSHQp1-21.csv",
              "AB2tempF2PSHQp2-21.csv",
              "AB2tempF2PSHQp3-21.csv",
              "AB2tempF2PSHQp4-21.csv",
              "AB2tempF2NAZQ0-22.csv",
              "AB2tempF2NAZQn5-22.csv",
              "AB2tempF2NAZQn6-22.csv",
              "AB2tempF2NAZQn7-22.csv",
              "AB2tempF2NAZQn8-22.csv",
              "AB2tempF2NAZQp1-22.csv",
              "AB2tempF2NAZQp2-22.csv",
              "AB2tempF2NAZQp3-22.csv",
              "AB2tempF2NAZQp4-22.csv",
              "AB2tempF2pilliquid15.csv",
              "AB2tempF2prisk14.csv",
              "AB2tempF2pspread12.csv",
              "AB2tempF2pturnover13.csv",
              "AB2tempF2risk14.csv",
              "AB2tempF2turnover13.csv",
              "AB2tempFBooktoMarket3.csv",
              "AB2tempFfilliquid6.csv",
              "AB2tempFfpLev10.csv",
              "AB2tempFfpROA9.csv",
              "AB2tempFfProf10.csv",
              "AB2tempFfpTobinQ10.csv",
              "AB2tempFfROA9.csv",
              "AB2tempFfspread5.csv",
              "AB2tempFfTobinQ10.csv",
              "AB2tempFmktcap1.csv",
              "AB2tempFmktReturn11.csv",
              "AB2tempFpfLev10.csv",
              "AB2tempFpilliquid6.csv",
              "AB2tempFpProfit10.csv",
              "AB2tempFprisk7.csv",
              "AB2tempFpspread5.csv",
              "AB2tempFpturnover8.csv",
              "AB2tempFrisk7.csv",
              "AB2tempFturnover8.csv",
              "AB2tempP11mR2.csv",
              "AB2tempBQprice18.csv",
              "AB2temp12mR17.csv"
)

dataname <- c("Return24months",
              "f2illiquid",
              "f2spread",
              "NIIQ0",
              "NIIQn5",
              "NIIQn6",
              "NIIQn7",
              "NIIQn8",
              "NIIQp1",
              "NIIQp2",
              "NIIQp3",
              "NIIQp4",
              "PIHQ0",
              "PIHQn5",
              "PIHQn6",
              "PIHQn7",
              "PIHQn8",
              "PIHQp1",
              "PIHQp2",
              "PIHQp3",
              "PIHQp4",
              "NISQ0",
              "NISQn5",
              "NISQn6",
              "NISQn7",
              "NISQn8",
              "NISQp1",
              "NISQp2",
              "NISQp3",
              "NISQp4",
              "PSHQ0",
              "PSHQn5",
              "PSHQn6",
              "PSHQn7",
              "PSHQn8",
              "PSHQp1",
              "PSHQp2",
              "PSHQp3",
              "PSHQp4",
              "NAZQ0",
              "NAZQn5",
              "NAZQn6",
              "NAZQn7",
              "NAZQn8",
              "NAZQp1",
              "NAZQp2",
              "NAZQp3",
              "NAZQp4",
              "p2illiquid",
              "p2risk",
              "p2spread",
              "P2Turnover",
              "f2risk",
              "f2Turnover",
              "Book2Market",
              "filliquid",
              "pLev",
              "pROA",
              "fProfit",
              "pTobinQ",
              "fROA",
              "fspread",
              "fTobinQ",
              "MarketCapM",
              "mktReturn",
              "fLev",
              "pilliquid",
              "pProfit",
              "prisk",
              "pspread",
              "pTurnover",
              "frisk",
              "fTurnover",
              "pm11Return",
              "BQprice",
              "m12Return"
)

obs = nrow(temp)   #180 observations

nVar <- length(datafile)  # no. of variables


myfiles = lapply(datafile, read.csv)


#pd1= read.csv("temp1BooktoMarket.csv",header = T)
#mycodes <- mycolnames[a]
#my2codes <- rep(mycodes,190)
#a <-which(mycol == "X1999")
#pd2= read.csv("temp2BooktoMarket.csv",header = T)
#mycol= colnames(pd2)
#a <-which(mycol == "X84602")

pdAll<-as.data.frame(myfiles[1])

i = 2L
for(i in 2L: length(myfiles)){
  a <- as.data.frame(myfiles[i])
  pdAll<-rbind(pdAll, a[,])
  
}
nrow(pdAll)
pdAll$X <- NULL
write.csv(pdAll,"pdAll2.csv")
mycolnames= colnames(pdAll)
CodeDate <-pdAll$Date1
Code <-mycolnames



DateL <- as.character(pdAll$Date2000[1:obs])
mDate <- data.frame("Date" = matrix(unlist(DateL), nrow=length(DateL), byrow=T))

pdataAll<-NULL

j = 2L
#last = (ncol(pdAll2)+1)/2 - 1
for(j in 2L:ncol(pdAll)){
  mCode <- c(rep(Code[(j)],obs))
  pdata<- data.frame("Code" = matrix(unlist(mCode), nrow=length(mCode), byrow=T))
  
  pdata <-cbind(pdata,mDate)
  i = 1L
  for(i in 1L:nVar){
    num = ((i-1)*obs)+1
    mD <- pdAll[num:(num+obs-1),j]
    mDL <- data.frame("mData" =matrix(unlist(mD), nrow=length(mD), byrow=T))
    pdata <-cbind(pdata,mDL)
  }
  if(is.null(pdataAll)==TRUE){
    pdataAll <- pdata
  }else{
    pdataAll <- rbind(pdataAll,pdata)
  }
  print(as.character(j))
  
}
pdataAll$X<-NULL

colnames(pdataAll)<-c("Code","Date",dataname)
str(pdataAll)
write.csv(pdataAll,"pdataAll2.csv")


a1 <- which(is.na(pdataAll$Return24months)==TRUE)
a2 <- which(is.na(pdataAll$Book2Market)==TRUE)
a3 <- which(is.na(pdataAll$filliquid)==TRUE)
a4 <- which(is.na(pdataAll$pLev)==TRUE)
a5 <- which(is.na(pdataAll$pROA)==TRUE)
a6 <- which(is.na(pdataAll$fProfit)==TRUE)
a7 <- which(is.na(pdataAll$pTobinQ)==TRUE)
a8 <- which(is.na(pdataAll$fROA)==TRUE)
a9 <- which(is.na(pdataAll$fspread)==TRUE)
a10 <- which(is.na(pdataAll$fTobinQ)==TRUE)
a11 <- which(is.na(pdataAll$MarketCapM)==TRUE)
a12 <- which(is.na(pdataAll$fLev)==TRUE)
a13 <- which(is.na(pdataAll$pilliquid)==TRUE)
a14 <- which(is.na(pdataAll$pProfit)==TRUE)
a15 <- which(is.na(pdataAll$prisk)==TRUE)
a16 <- which(is.na(pdataAll$pspread)==TRUE)
a17 <- which(is.na(pdataAll$pTurnover)==TRUE)
a18 <- which(is.na(pdataAll$frisk)==TRUE)
a19 <- which(is.na(pdataAll$fTurnover)==TRUE)
a20 <- which(is.na(pdataAll$mktReturn)==TRUE)
a21 <- which(is.na(pdataAll$pm11Return)==TRUE)
#a22 <- which(is.na(pdataAll$NISQ0)==TRUE)
#a23 <- which(is.na(pdataAll$NISQn5)==TRUE)
#a24 <- which(is.na(pdataAll$NISQn6)==TRUE)
#a25 <- which(is.na(pdataAll$NISQn7)==TRUE)
#a26 <- which(is.na(pdataAll$NISQn8)==TRUE)
#a27 <- which(is.na(pdataAll$NISQp1)==TRUE)
#a28 <- which(is.na(pdataAll$NISQp2)==TRUE)
#a29 <- which(is.na(pdataAll$NISQp3)==TRUE)
#a30 <- which(is.na(pdataAll$NISQp4)==TRUE)


nalist <- c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20,a21) #,a22,a23,a24,a25,a26,a27,a28,a29,a30
unique_nalist <- unique(nalist)
pdataAll3 <- pdataAll[-unique_nalist,]
write.csv(pdataAll3,"pdataAllClean2.csv")

