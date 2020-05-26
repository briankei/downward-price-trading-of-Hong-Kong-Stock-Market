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

pdataF2<- pdataF # pspread and fspread 
##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$BYp1 <- ymd(as.Date(as.character(pd1$BYp1)))
pd1$EYp1 <- ymd(as.Date(as.character(pd1$EYp1)))
pd1$BYn1 <- ymd(as.Date(as.character(pd1$BYn1)))
pd1$EYn1 <- ymd(as.Date(as.character(pd1$EYn1)))
pd1$ExDate <- ymd(as.Date(as.character(pd1$ExDate)))
pd1$fTurnover <- NA
pd1$pTurnover <- NA

Last1 = read.csv("turnover1.csv",header = T)

###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$filliquid <- NA
  pd1$pilliquid <- NA
  #
  ###########################################################################
  
  
j=1L
for(j in 1L:nrow(pd1)){
  
  
    monthreturnData<-Last1
  monthReturn = monthreturnData[,codename]
  monthReturnDate = as.character(monthreturnData[,datename])
  monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
  
  BeginDay = pd1[j,"BYp1"]
  EndDay = pd1[j,"EYp1"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    
    pd1[j,"pTurnover"] = mean(dailyReturn,na.rm = TRUE)
    
  }
  ########################################### post   #################################################
  BeginDay = pd1[j,"BYn1"]
  EndDay = pd1[j,"EYn1"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    
    pd1[j,"fTurnover"] = mean(dailyReturn,na.rm = TRUE)
    
  }
  
}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$fTurnover
  pdataF2[,pd1Code]= pd1$pTurnover
  
  print(codename)
}

beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB1tempFturnover8.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB1tempFpturnover8.csv")


########## ---------------------------------- 2F --------------------------
rm(list = ls())
##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$BYp1 <- ymd(as.Date(as.character(pd1$BYp1)))
pd1$EYp1 <- ymd(as.Date(as.character(pd1$EYp1)))
pd1$BYn1 <- ymd(as.Date(as.character(pd1$BYn1)))
pd1$EYn1 <- ymd(as.Date(as.character(pd1$EYn1)))
pd1$ExDate <- ymd(as.Date(as.character(pd1$ExDate)))
pd1$fTurnover <- NA
pd1$pTurnover <- NA

##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pdataF= read.csv("ABtemp2.csv",header = T)
pdataF$X<-NULL
myrownames <- ymd(as.character(pdataF[,1]))
rownames(pdataF)<-myrownames

pdataF2<- pdataF # pspread and fspread 
##############################################################################

#
##############################################################################


Last1 = read.csv("turnover2.csv",header = T)

###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$filliquid <- NA
  pd1$pilliquid <- NA
  #
  ###########################################################################
  
  
  j=1L
  for(j in 1L:nrow(pd1)){
    
    
    monthreturnData<-Last1
    monthReturn = monthreturnData[,codename]
    monthReturnDate = as.character(monthreturnData[,datename])
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    
    BeginDay = pd1[j,"BYp1"]
    EndDay = pd1[j,"EYp1"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      
      pd1[j,"pTurnover"] = mean(dailyReturn,na.rm = TRUE)
      
    }
    ########################################### post   #################################################
    BeginDay = pd1[j,"BYn1"]
    EndDay = pd1[j,"EYn1"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      
      pd1[j,"fTurnover"] = mean(dailyReturn,na.rm = TRUE)
      
    }
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$fTurnover
  pdataF2[,pd1Code]= pd1$pTurnover
  
  print(codename)
}

beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB2tempFturnover8.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB2tempFpturnover8.csv")


