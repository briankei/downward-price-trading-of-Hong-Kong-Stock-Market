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
##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

pd1$BYn1 <- ymd(as.Date(as.character(pd1$BYn1)))
pd1$EYn1 <- ymd(as.Date(as.character(pd1$EYn1 )))
pd1$ExDate <- ymd(as.Date(as.character(pd1$ExDate)))
#
##############################################################################





#pd1$Return24months <- NA

Last1 = read.csv("adjRLog1.csv",header = T)
#Last2 = read.csv("adjRLog2.csv",header = T)
###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$Return12months <- NA
  #
  ###########################################################################
  
j = 1
for(j in 1L:nrow(pd1)){ 
  BeginDay = pd1[j,"BYn1"]
  EndDay = pd1[j,"EYn1"]
#  if(pd1[j,"Symbol"] <2000){
    monthreturnData<-Last1
#  }else{
#    monthreturnData<-Last2
#  }
  monthReturn = monthreturnData[,codename]
  monthReturnDate = as.character(monthreturnData[,datename])
  monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"Return12months"] = exp(sum(dailyReturn, na.rm = TRUE))
  }
  

}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$Return12months
  
  print(codename)
}

#
###########################################################################
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB1temp12mR17.csv")
########-----------------------------------------2 file -----------------------------

rm(list = ls())
##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

pd1$BYn1 <- ymd(as.Date(as.character(pd1$BYn1)))
pd1$EYn1 <- ymd(as.Date(as.character(pd1$EYn1 )))
pd1$ExDate <- ymd(as.Date(as.character(pd1$ExDate)))
#
##############################################################################

##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pdataF= read.csv("ABtemp2.csv",header = T)
pdataF$X<-NULL
myrownames <- ymd(as.character(pdataF[,1]))
rownames(pdataF)<-myrownames

Last1 = read.csv("adjRLog2.csv",header = T)
#Last2 = read.csv("adjRLog2.csv",header = T)
###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$Return12months <- NA
  #
  ###########################################################################
  
  j = 1
  for(j in 1L:nrow(pd1)){ 
    BeginDay = pd1[j,"BYn1"]
    EndDay = pd1[j,"EYn1"]
    #  if(pd1[j,"Symbol"] <2000){
    monthreturnData<-Last1
    #  }else{
    #    monthreturnData<-Last2
    #  }
    monthReturn = monthreturnData[,codename]
    monthReturnDate = as.character(monthreturnData[,datename])
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"Return12months"] = exp(sum(dailyReturn, na.rm = TRUE))
    }
    
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$Return12months
  print(codename)
}

#
###########################################################################
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB2temp12mR17.csv")



