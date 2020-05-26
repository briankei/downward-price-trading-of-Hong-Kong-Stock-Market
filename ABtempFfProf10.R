setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")

library(beepr)
library(lubridate)
library(zoo)
##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pdataF= read.csv("ABtemp1.csv",header = T)
pdataF$X<-NULL
myrownames <- ymd(as.character(pdataF[,1]))
rownames(pdataF)<-myrownames

pdataF2 <- pdataF

##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate),format = "%m/%d/%Y"))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$ExDate <-  ymd(as.Date(as.character(pd1$ExDate),format = "%m/%d/%Y"))

profit1 = read.csv("profitability1.csv",header = T)



pd1$fProfit <- NA
pd1$pProfit <- NA



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
  #
  ###########################################################################
  

j = 1

for(j in 1L:nrow(pd1)){

    monthreturnDataA<-profit1
  monthReturnA = monthreturnDataA[,codename]
  monthReturnDateA = as.character(monthreturnDataA[,datename])
  monthReturnDateA = ymd(as.Date(monthReturnDateA,format = "%m/%d/%Y"))

  BeginDay = paste("1/1/",as.character(pd1[j,"YrP1"]),sep="")
  BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
  EndDay = paste("12/31/",as.character(pd1[j,"YrP1"]),sep="")
  EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))
  
  BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
  EndDayIndexA <- which(monthReturnDateA <= EndDay)


  if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 ){
    dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
    pd1[j,"pProfit"] = dailyReturnA[length(dailyReturnA)]
  }

  ########################################### post   #################################################
  
  BeginDay = paste("1/1/",as.character(pd1[j,"YrN1"]),sep="")
  BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
  EndDay = paste("12/31/",as.character(pd1[j,"YrN1"]),sep="")
  EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))

  BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
  EndDayIndexA <- which(monthReturnDateA <= EndDay)

  if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 ){
    dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
    pd1[j,"fProfit"] = dailyReturnA[length(dailyReturnA)]
  }

 
  
}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$fProfit
  pdataF2[,pd1Code]= pd1$pProfit
  print(codename)
}
#
###########################################################################
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB1tempFfProf10.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB1tempFpProfit10.csv")


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


##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate),format = "%m/%d/%Y"))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$ExDate <-  ymd(as.Date(as.character(pd1$ExDate),format = "%m/%d/%Y"))

profit1 = read.csv("profitability2.csv",header = T)


pd1$fProfit <- NA
pd1$pProfit <- NA



#
##############################################################################



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
  #
  ###########################################################################
  
  
  j = 1
  
  for(j in 1L:nrow(pd1)){
    
    monthreturnDataA<-profit1
    monthReturnA = monthreturnDataA[,codename]
    monthReturnDateA = as.character(monthreturnDataA[,datename])
    monthReturnDateA = ymd(as.Date(monthReturnDateA,format = "%m/%d/%Y"))
 
    BeginDay = paste("1/1/",as.character(pd1[j,"YrP1"]),sep="")
    BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
    EndDay = paste("12/31/",as.character(pd1[j,"YrP1"]),sep="")
    EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))
    
    BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
    EndDayIndexA <- which(monthReturnDateA <= EndDay)

    
    if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 ){
      dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
      pd1[j,"pProfit"] = dailyReturnA[length(dailyReturnA)]
    }

    ########################################### post   #################################################
    
    BeginDay = paste("1/1/",as.character(pd1[j,"YrN1"]),sep="")
    BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
    EndDay = paste("12/31/",as.character(pd1[j,"YrN1"]),sep="")
    EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))
    
    BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
    EndDayIndexA <- which(monthReturnDateA <= EndDay)
 
    if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 ){
      dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
      pd1[j,"fProfit"] = dailyReturnA[length(dailyReturnA)]
    }

  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$fProfit
  pdataF2[,pd1Code]= pd1$pProfit
  print(codename)
}
#
###########################################################################
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB2tempFfProf10.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB2tempFpProfit10.csv")

