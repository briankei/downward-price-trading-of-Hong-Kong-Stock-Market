#set monthly market cap
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(beepr)
library(lubridate)
library(zoo)
rm(list = ls())
#############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pd1= read.csv("ABtemp1.csv",header = T)
pd1$X<-NULL
myrownames <- ymd(as.character(pd1[,1]))
rownames(pd1)<-myrownames
##############################################################################
pdataF= read.csv("tempDates2.csv",header = T)
pdataF[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pdataF$ExDate)))
rownames(pdataF)<-myrownames

#
##############################################################################
pdataF$ExDate <- ymd(as.Date(as.character(pdataF$ExDate)))
pdataF$BQp5M <- ymd(as.Date(as.character(pdataF$BQp5M)))
pdataF$Book2Market <- NA


Last1 = read.csv("BkAdjMktBV_NAdj1Dd.csv",header = T)


rownames(Last1)<-(c(as.character(as.Date(as.character(Last1$Date1),"%m/%d/%Y"))))

###############################################################################
#
CodeNames <- colnames(pd1)
pd1Code = 2L
for(pd1Code in 2L: ncol(pd1)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pdataF$Book2Market <- NA
  #
  ###########################################################################
  

j = 1L
for(j in 1L: nrow(pdataF)){
  



      monthReturn = Last1[,codename]
      monthReturnDate = as.character(Last1[,datename])
     BeginDay = as.character(pdataF[j,"BQ0Yp1"])
    BeginDay = ymd(as.Date(BeginDay))     
    EndDay =  as.character(pdataF[j,"BQp5M"]) 
    EndDay = ymd(as.Date(EndDay))
    
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      DailyReturn = na.locf(monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]])
      if(length(DailyReturn[length(DailyReturn)])>0)
        pdataF[j,"Book2Market"] = 1/DailyReturn[length(DailyReturn)] #Book2Market change
    }
    

}

  ###############################################################################
  #
  pd1[,pd1Code]= pdataF$Book2Market
  
  print(codename)
}

#
###########################################################################



beep("coin")
pd1$X <-NULL
write.csv(pd1,file="AB1tempFBooktoMarket3.csv")
#----------------------------------------------------2 F --------------------------------
rm(list = ls())

library(beepr)
library(lubridate)
library(zoo)

#############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pd1= read.csv("ABtemp2.csv",header = T)
pd1$X<-NULL
myrownames <- ymd(as.character(pd1[,1]))
rownames(pd1)<-myrownames
##############################################################################
pdataF= read.csv("tempDates2.csv",header = T)
pdataF[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pdataF$ExDate)))
rownames(pdataF)<-myrownames

#
##############################################################################
pdataF$ExDate <- ymd(as.Date(as.character(pdataF$ExDate)))
pdataF$BQp5M <- ymd(as.Date(as.character(pdataF$BQp5M)))
pdataF$Book2Market <- NA


#
##############################################################################




Last1 = read.csv("BkAdjMktBV_NAdj2Dd.csv",header = T)


rownames(Last1)<-(c(as.character(as.Date(as.character(Last1$Date2000),"%m/%d/%Y"))))

###############################################################################
#
CodeNames <- colnames(pd1)
pd1Code = 2L
for(pd1Code in 2L: ncol(pd1)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pdataF$Book2Market <- NA
  #
  ###########################################################################
  
  
  j = 1L
  for(j in 1L: nrow(pdataF)){
    
    


      monthReturn = Last1[,codename]
      monthReturnDate = as.character(Last1[,datename])
     BeginDay = as.character(pdataF[j,"BQ0Yp1"])
    BeginDay = ymd(as.Date(BeginDay))     
    EndDay =  as.character(pdataF[j,"BQp5M"]) 
    EndDay = ymd(as.Date(EndDay))
    
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      DailyReturn = na.locf(monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]])
      if(length(DailyReturn[length(DailyReturn)])>0)
        pdataF[j,"Book2Market"] = 1/DailyReturn[length(DailyReturn)] #Book2Market change
    }
    
    
  }
  
  ###############################################################################
  #
  pd1[,pd1Code]= pdataF$Book2Market
  
  print(codename)
}

#
###########################################################################



beep("coin")
pd1$X <-NULL
write.csv(pd1,file="AB2tempFBooktoMarket3.csv")



