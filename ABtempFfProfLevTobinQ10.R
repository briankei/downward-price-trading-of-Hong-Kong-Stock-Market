setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")

library(beepr)
library(lubridate)
library(zoo)
rm(list = ls())
##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pdataF= read.csv("ABtemp1.csv",header = T)
pdataF$X<-NULL
myrownames <- ymd(as.character(pdataF[,1]))
rownames(pdataF)<-myrownames

pdataF2<- pdataF # tobinQ 
pdataF3<- pdataF  #profit
pdataF4<- pdataF
pdataF5<- pdataF   #leverage
pdataF6<- pdataF


##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$ExDate <-  ymd(as.Date(as.character(pd1$ExDate)))
tobinQ1 = read.csv("tobinQ_1Yd.csv",header = T)

profit1 = read.csv("profitability1.csv",header = T)

leverage1 = read.csv("leverage1.csv",header = T)


pd1$fProfit <- NA
pd1$pProfit <- NA
pd1$fTobinQ <- NA
pd1$pTobinQ  <- NA
pd1$fLev<- NA
pd1$pLev <- NA



###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$fProfit <- NA
  pd1$pProfit <- NA
  pd1$fTobinQ <- NA
  pd1$pTobinQ  <- NA
  pd1$fLev<- NA
  pd1$pLev <- NA
  #
  ###########################################################################
  

j = 1

for(j in 1L:nrow(pd1)){

    monthreturnDataA<-profit1
    monthreturnDataB<-tobinQ1
    monthreturnDataC<-leverage1
  monthReturnA = monthreturnDataA[,codename]
  monthReturnDateA = as.character(monthreturnDataA[,datename])
  monthReturnDateA = ymd(as.Date(monthReturnDateA,format = "%m/%d/%Y"))
  monthReturnB = monthreturnDataB[,codename]
  monthReturnDateB = as.character(monthreturnDataB[,datename])
  monthReturnDateB = ymd(as.Date(monthReturnDateB,format = "%m/%d/%Y"))
  monthReturnC = monthreturnDataC[,codename]
  monthReturnDateC = as.character(monthreturnDataC[,datename])
  monthReturnDateC = ymd(as.Date(monthReturnDateC,format = "%m/%d/%Y"))
  
  BeginDay = paste("1/1/",as.character(pd1[j,"YrP1"]),sep="")
  BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
  EndDay = paste("12/31/",as.character(pd1[j,"YrP1"]),sep="")
  EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))
  
  BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
  EndDayIndexA <- which(monthReturnDateA <= EndDay)
  BeginDayIndexB <- which(monthReturnDateB >= BeginDay)
  EndDayIndexB <- which(monthReturnDateB <= EndDay)
  BeginDayIndexC <- which(monthReturnDateC >= BeginDay)
  EndDayIndexC <- which(monthReturnDateC <= EndDay)
  

  if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 ){
    dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
    pd1[j,"pProfit"] = dailyReturnA[length(dailyReturnA)]
  }
  if(length(BeginDayIndexB)>0 && length(EndDayIndexB)>0 ){
    dailyReturnB <- monthReturnB[BeginDayIndexB[1]:EndDayIndexB[length(EndDayIndexB)]]
    pd1[j,"pTobinQ"] = dailyReturnB[length(dailyReturnB)]
  }
  if(length(BeginDayIndexC)>0 && length(EndDayIndexC)>0 ){
    dailyReturnC <- monthReturnC[BeginDayIndexC[1]:EndDayIndexC[length(EndDayIndexC)]]
    pd1[j,"pLev"] = dailyReturnC[length(dailyReturnC)]
  }
  
  ########################################### post   #################################################
  
  BeginDay = paste("1/1/",as.character(pd1[j,"YrN1"]),sep="")
  BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
  EndDay = paste("12/31/",as.character(pd1[j,"YrN1"]),sep="")
  EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))

  BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
  EndDayIndexA <- which(monthReturnDateA <= EndDay)
  BeginDayIndexB <- which(monthReturnDateB >= BeginDay)
  EndDayIndexB <- which(monthReturnDateB <= EndDay)
  BeginDayIndexC <- which(monthReturnDateC >= BeginDay)
  EndDayIndexC <- which(monthReturnDateC <= EndDay)
  
  if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 ){
    dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
    pd1[j,"fProfit"] = dailyReturnA[length(dailyReturnA)]
  }
  if(length(BeginDayIndexB)>0 && length(EndDayIndexB)>0 ){
    dailyReturnB <- monthReturnB[BeginDayIndexB[1]:EndDayIndexB[length(EndDayIndexB)]]
    pd1[j,"fTobinQ"] = dailyReturnB[length(dailyReturnB)]
  }
  if(length(BeginDayIndexC)>0 && length(EndDayIndexC)>0 ){
    dailyReturnC <- monthReturnC[BeginDayIndexC[1]:EndDayIndexC[length(EndDayIndexC)]]
    pd1[j,"fLev"] = dailyReturnC[length(dailyReturnC)]
  }
  
 
  
}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$fProfit
  pdataF2[,pd1Code]= pd1$pProfit
  pdataF3[,pd1Code]= pd1$fTobinQ
  pdataF4[,pd1Code]= pd1$pTobinQ
  pdataF5[,pd1Code]= pd1$fLev
  pdataF6[,pd1Code]= pd1$pLev
  print(codename)
}
#
###########################################################################
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB1tempFfProf10.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB1tempFpProfit10.csv")
pdataF3$X <-NULL
write.csv(pdataF3,file="AB1tempFfTobinQ10.csv")
pdataF4$X <-NULL
write.csv(pdataF4,file="AB1tempFfpTobinQ10.csv")
pdataF5$X <-NULL
write.csv(pdataF5,file="AB1tempFpfLev10.csv")
pdataF6$X <-NULL
write.csv(pdataF6,file="AB1tempFfpLev10.csv")


##################### ---------------------------- 2 F ------------------------------

##############################################################################
rm(list = ls())
##############################################################################
library(beepr)
library(lubridate)
library(zoo)
##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pdataF= read.csv("ABtemp2.csv",header = T)
pdataF$X<-NULL
myrownames <- ymd(as.character(pdataF[,1]))
rownames(pdataF)<-myrownames

pdataF2<- pdataF # tobinQ 
pdataF3<- pdataF  #profit
pdataF4<- pdataF
pdataF5<- pdataF   #leverage
pdataF6<- pdataF


##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
###############################################################################



tobinQ1 = read.csv("tobinQ_2Yd.csv",header = T)

profit1 = read.csv("profitability2.csv",header = T)

leverage1 = read.csv("leverage2.csv",header = T)


pd1$fProfit <- NA
pd1$pProfit <- NA
pd1$fTobinQ <- NA
pd1$pTobinQ  <- NA
pd1$fLev<- NA
pd1$pLev <- NA



###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$fProfit <- NA
  pd1$pProfit <- NA
  pd1$fTobinQ <- NA
  pd1$pTobinQ  <- NA
  pd1$fLev<- NA
  pd1$pLev <- NA
  #
  ###########################################################################
  
  
  j = 1
  
  for(j in 1L:nrow(pd1)){
    
    monthreturnDataA<-profit1
    monthreturnDataB<-tobinQ1
    monthreturnDataC<-leverage1
    monthReturnA = monthreturnDataA[,codename]
    monthReturnDateA = as.character(monthreturnDataA[,datename])
    monthReturnDateA = ymd(as.Date(monthReturnDateA,format = "%m/%d/%Y"))
    monthReturnB = monthreturnDataB[,codename]
    monthReturnDateB = as.character(monthreturnDataB[,datename])
    monthReturnDateB = ymd(as.Date(monthReturnDateB,format = "%m/%d/%Y"))
    monthReturnC = monthreturnDataC[,codename]
    monthReturnDateC = as.character(monthreturnDataC[,datename])
    monthReturnDateC = ymd(as.Date(monthReturnDateC,format = "%m/%d/%Y"))
   
    BeginDay = paste("1/1/",as.character(pd1[j,"YrP1"]),sep="")
    BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
    EndDay = paste("12/31/",as.character(pd1[j,"YrP1"]),sep="")
    EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))
    
    BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
    EndDayIndexA <- which(monthReturnDateA <= EndDay)
    BeginDayIndexB <- which(monthReturnDateB >= BeginDay)
    EndDayIndexB <- which(monthReturnDateB <= EndDay)
    BeginDayIndexC <- which(monthReturnDateC >= BeginDay)
    EndDayIndexC <- which(monthReturnDateC <= EndDay)
    
    
    if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 ){
      dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
      pd1[j,"pProfit"] = dailyReturnA[length(dailyReturnA)]
    }
    if(length(BeginDayIndexB)>0 && length(EndDayIndexB)>0 ){
      dailyReturnB <- monthReturnB[BeginDayIndexB[1]:EndDayIndexB[length(EndDayIndexB)]]
      pd1[j,"pTobinQ"] = dailyReturnB[length(dailyReturnB)]
    }
    if(length(BeginDayIndexC)>0 && length(EndDayIndexC)>0 ){
      dailyReturnC <- monthReturnC[BeginDayIndexC[1]:EndDayIndexC[length(EndDayIndexC)]]
      pd1[j,"pLev"] = dailyReturnC[length(dailyReturnC)]
    }
    
    ########################################### post   #################################################
    
    BeginDay = paste("1/1/",as.character(pd1[j,"YrN1"]),sep="")
    BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
    EndDay = paste("12/31/",as.character(pd1[j,"YrN1"]),sep="")
    EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))
    
    BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
    EndDayIndexA <- which(monthReturnDateA <= EndDay)
    BeginDayIndexB <- which(monthReturnDateB >= BeginDay)
    EndDayIndexB <- which(monthReturnDateB <= EndDay)
    BeginDayIndexC <- which(monthReturnDateC >= BeginDay)
    EndDayIndexC <- which(monthReturnDateC <= EndDay)
    
    if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 ){
      dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
      pd1[j,"fProfit"] = dailyReturnA[length(dailyReturnA)]
    }
    if(length(BeginDayIndexB)>0 && length(EndDayIndexB)>0 ){
      dailyReturnB <- monthReturnB[BeginDayIndexB[1]:EndDayIndexB[length(EndDayIndexB)]]
      pd1[j,"fTobinQ"] = dailyReturnB[length(dailyReturnB)]
    }
    if(length(BeginDayIndexC)>0 && length(EndDayIndexC)>0 ){
      dailyReturnC <- monthReturnC[BeginDayIndexC[1]:EndDayIndexC[length(EndDayIndexC)]]
      pd1[j,"fLev"] = dailyReturnC[length(dailyReturnC)]
    }
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$fProfit
  pdataF2[,pd1Code]= pd1$pProfit
  pdataF3[,pd1Code]= pd1$fTobinQ
  pdataF4[,pd1Code]= pd1$pTobinQ
  pdataF5[,pd1Code]= pd1$fLev
  pdataF6[,pd1Code]= pd1$pLev
  print(codename)
}
#
###########################################################################
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB2tempFfProf10.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB2tempFpProfit10.csv")
pdataF3$X <-NULL
write.csv(pdataF3,file="AB2tempFfTobinQ10.csv")
pdataF4$X <-NULL
write.csv(pdataF4,file="AB2tempFfpTobinQ10.csv")
pdataF5$X <-NULL
write.csv(pdataF5,file="AB2tempFpfLev10.csv")
pdataF6$X <-NULL
write.csv(pdataF6,file="AB2tempFfpLev10.csv")

