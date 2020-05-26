#Get  
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2T")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp1.csv",header = T)

pdataAll <- NULL

datafile <- c("temp1BooktoMarket.csv","temp1F24mR.csv","temp1Filliquid.csv","temp1FLev.csv","temp1Fpilliquid.csv",
              "temp1FpLev.csv","temp1FpPRofit.csv","temp1Fprisk.csv","temp1FpRoa.csv","temp1FProfit.csv",
              "temp1FptobinQ.csv", "temp1FpTurnover.csv","temp1Frisk.csv","temp1Froa.csv","temp1FtobinQ.csv",
              "temp1Fturnover.csv", "temp1MktCap.csv","temp1P11mR.csv","temp1postAvgSpread.csv","temp1preAvgSpread.csv")

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
  pdAll<-rbind(pdAll, a[,1:2985])
  
}
nrow(pdAll)
pdAll$X <- NULL
write.csv(pdAll,"pdAll.csv")
mycolnames= colnames(pdAll)
a<-c(seq(from = 2, to = (length(mycolnames)-1), by =2))
pdAll2 <- pdAll[a]
CodeDate <-pdAll$Date1
Code <-colnames(pdAll2)


mData <- 20
DateL <- as.character(pdAll$Date1[1:190])
mDate <- data.frame("Date" = matrix(unlist(DateL), nrow=length(DateL), byrow=T))

pdataAll<-NULL

j = 1L
#last = (ncol(pdAll2)+1)/2 - 1
for(j in 1L:ncol(pdAll2)){
  mCode <- c(rep(Code[(j)],190))
  pdata<- data.frame("Code" = matrix(unlist(mCode), nrow=length(mCode), byrow=T))
  
  pdata <-cbind(pdata,mDate)
  i = 1L
  for(i in 1L:mData){
    num = ((i-1)*190)+1
    mD <- pdAll2[num:(num+189),j]
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
dataname <- c("BooktoMarket","F24mR","Filliquid","FLev","Fpilliquid",
              "FpLev","FpPRofit","Fprisk","FpRoa","FProfit",
              "FptobinQ", "FpTurnover","Frisk","Froa","FtobinQ",
              "Fturnover", "MktCap","P11mR","postAvgSpread","preAvgSpread")

colnames(pdataAll)<-c("Code","Date",dataname)
str(pdataAll)
write.csv(pdataAll,"pdataAll1.csv")


a1 <- which(is.na(pdataAll$BooktoMarket)==TRUE)
a2 <- which(is.na(pdataAll$F24mR)==TRUE)
a3 <- which(is.na(pdataAll$Filliquid)==TRUE)
a4 <- which(is.na(pdataAll$FLev)==TRUE)
a5 <- which(is.na(pdataAll$Fpilliquid)==TRUE)
a6 <- which(is.na(pdataAll$FpLev)==TRUE)
a7 <- which(is.na(pdataAll$FpPRofit)==TRUE)
a8 <- which(is.na(pdataAll$Fprisk)==TRUE)
a9 <- which(is.na(pdataAll$FpRoa)==TRUE)
a10 <- which(is.na(pdataAll$FProfit)==TRUE)
a11 <- which(is.na(pdataAll$FptobinQ)==TRUE)
a12 <- which(is.na(pdataAll$FpTurnover)==TRUE)
a13 <- which(is.na(pdataAll$Frisk)==TRUE)
a14 <- which(is.na(pdataAll$Froa)==TRUE)
a15 <- which(is.na(pdataAll$FtobinQ)==TRUE)
a16 <- which(is.na(pdataAll$Fturnover)==TRUE)
a17 <- which(is.na(pdataAll$MktCap)==TRUE)
a18 <- which(is.na(pdataAll$P11mR)==TRUE)
a19 <- which(is.na(pdataAll$postAvgSpread)==TRUE)
a20 <- which(is.na(pdataAll$preAvgSpread)==TRUE)

nalist <- c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)
unique_nalist <- unique(nalist)
pdataAll3 <- pdataAll[-unique_nalist,]
write.csv(pdataAll3,"pdataAllClean.csv")

##########################################################################################################
#                                    2 f
##############################################################################################################

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp2.csv",header = T)

pdataAll <- NULL

datafile <- c("temp2BooktoMarket.csv","temp2F24mR.csv","temp2Filliquid.csv","temp2FLev.csv","temp2Fpilliquid.csv",
              "temp2FpLev.csv","temp2FpPRofit.csv","temp2Fprisk.csv","temp2FpRoa.csv","temp2FProfit.csv",
              "temp2FptobinQ.csv", "temp2FpTurnover.csv","temp2Frisk.csv","temp2Froa.csv","temp2FtobinQ.csv",
              "temp2Fturnover.csv", "temp2MktCap.csv","temp2P11mR.csv","temp2postAvgSpread.csv","temp2preAvgSpread.csv")

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
  pdAll<-rbind(pdAll, a[,1:1693])
  
}
nrow(pdAll)
pdAll$X <- NULL
write.csv(pdAll,"pdAll2.csv")
mycolnames= colnames(pdAll)
a<-c(seq(from = 2, to = (length(mycolnames)-1), by =2))
pdAll2 <- pdAll[a]
CodeDate <-pdAll[,1]
Code <-colnames(pdAll2)


mData <- 20
DateL <- as.character(pdAll$Date2000[1:190])
mDate <- data.frame("Date" = matrix(unlist(DateL), nrow=length(DateL), byrow=T))

pdataAll<-NULL

j = 1L
#last = (ncol(pdAll2)+1)/2 - 1
for(j in 1L:ncol(pdAll2)){
  mCode <- c(rep(Code[(j)],190))
  pdata<- data.frame("Code" = matrix(unlist(mCode), nrow=length(mCode), byrow=T))
  
  pdata <-cbind(pdata,mDate)
  i = 1L
  for(i in 1L:mData){
    num = ((i-1)*190)+1
    mD <- pdAll2[num:(num+189),j]
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
dataname <- c("BooktoMarket","F24mR","Filliquid","FLev","Fpilliquid",
              "FpLev","FpPRofit","Fprisk","FpRoa","FProfit",
              "FptobinQ", "FpTurnover","Frisk","Froa","FtobinQ",
              "Fturnover", "MktCap","P11mR","postAvgSpread","preAvgSpread")

colnames(pdataAll)<-c("Code","Date",dataname)
str(pdataAll)
write.csv(pdataAll,"pdataAll2.csv")

a1 <- which(is.na(pdataAll$BooktoMarket)==TRUE)
a2 <- which(is.na(pdataAll$F24mR)==TRUE)
a3 <- which(is.na(pdataAll$Filliquid)==TRUE)
a4 <- which(is.na(pdataAll$FLev)==TRUE)
a5 <- which(is.na(pdataAll$Fpilliquid)==TRUE)
a6 <- which(is.na(pdataAll$FpLev)==TRUE)
a7 <- which(is.na(pdataAll$FpPRofit)==TRUE)
a8 <- which(is.na(pdataAll$Fprisk)==TRUE)
a9 <- which(is.na(pdataAll$FpRoa)==TRUE)
a10 <- which(is.na(pdataAll$FProfit)==TRUE)
a11 <- which(is.na(pdataAll$FptobinQ)==TRUE)
a12 <- which(is.na(pdataAll$FpTurnover)==TRUE)
a13 <- which(is.na(pdataAll$Frisk)==TRUE)
a14 <- which(is.na(pdataAll$Froa)==TRUE)
a15 <- which(is.na(pdataAll$FtobinQ)==TRUE)
a16 <- which(is.na(pdataAll$Fturnover)==TRUE)
a17 <- which(is.na(pdataAll$MktCap)==TRUE)
a18 <- which(is.na(pdataAll$P11mR)==TRUE)
a19 <- which(is.na(pdataAll$postAvgSpread)==TRUE)
a20 <- which(is.na(pdataAll$preAvgSpread)==TRUE)

nalist <- c(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,a20)
unique_nalist <- unique(nalist)
pdataAll3 <- pdataAll[-unique_nalist,]
write.csv(pdataAll3,"pdataAll2Clean.csv")
