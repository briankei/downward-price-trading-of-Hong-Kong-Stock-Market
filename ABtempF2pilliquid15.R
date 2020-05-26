#setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
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
pd1[,"?.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$BYp2 <- ymd(as.Date(as.character(pd1$BYp2)))
pd1$EYp2 <- ymd(as.Date(as.character(pd1$EYp2)))
pd1$BYn2 <- ymd(as.Date(as.character(pd1$BYn2)))
pd1$EYn2 <- ymd(as.Date(as.character(pd1$EYn2)))
pd1$ExDate <- ymd(as.Date(as.character(pd1$ExDate)))
pd1$f2illiquid <- NA
pd1$p2illiquid <- NA

Last1 = read.csv("illiq1.csv",header = T)


###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$f2illiquid <- NA
  pd1$p2illiquid <- NA
  #
  ###########################################################################
  
  

j = 1L
for(j in 1L:nrow(pd1)){
  
    monthreturnData<-Last1
  monthReturn = monthreturnData[,codename]
  monthReturnDate = as.character(monthreturnData[,datename])
  monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
  
  BeginDay = pd1[j,"BYp2"]
  EndDay = pd1[j,"EYp2"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    
    pd1[j,"p2illiquid"] = mean(dailyReturn, na.rm = TRUE)
    
  }
  ########################################### post   #################################################
  BeginDay = pd1[j,"BYn2"]
  EndDay = pd1[j,"EYn2"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    
    pd1[j,"f2illiquid"] = mean(dailyReturn, na.rm = TRUE)
    
  }
  
  
  
}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$f2illiquid
  pdataF2[,pd1Code]= pd1$p2illiquid
  
  print(codename)
}

#
###########################################################################

beep("coin")

pdataF$X <-NULL
write.csv(pdataF,file="AB1tempF2filliquid15.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB1tempF2pilliquid15.csv")

########################--------------------  2f  ----------------------------
rm(list = ls())
##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"?.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$BYp2 <- ymd(as.Date(as.character(pd1$BYp2)))
pd1$EYp2 <- ymd(as.Date(as.character(pd1$EYp2)))
pd1$BYn2 <- ymd(as.Date(as.character(pd1$BYn2)))
pd1$EYn2 <- ymd(as.Date(as.character(pd1$EYn2)))
pd1$ExDate <- ymd(as.Date(as.character(pd1$ExDate)))
pd1$f2illiquid <- NA
pd1$p2illiquid <- NA

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

Last1 = read.csv("illiq2.csv",header = T)


###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$f2illiquid <- NA
  pd1$p2illiquid <- NA
  #
  ###########################################################################
  
  
  
  j = 1L
  for(j in 1L:nrow(pd1)){
    
    monthreturnData<-Last1
    monthReturn = monthreturnData[,codename]
    monthReturnDate = as.character(monthreturnData[,datename])
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    
    BeginDay = pd1[j,"BYp2"]
    EndDay = pd1[j,"EYp2"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      
      pd1[j,"p2illiquid"] = mean(dailyReturn, na.rm = TRUE)
      
    }
    ########################################### post   #################################################
    BeginDay = pd1[j,"BYn2"]
    EndDay = pd1[j,"EYn2"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      
      pd1[j,"f2illiquid"] = mean(dailyReturn, na.rm = TRUE)
      
    }
    
    
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$f2illiquid
  pdataF2[,pd1Code]= pd1$p2illiquid
  
  print(codename)
}

#
###########################################################################

beep("coin")

pdataF$X <-NULL
write.csv(pdataF,file="AB2tempF2filliquid15.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB2tempF2pilliquid15.csv")
