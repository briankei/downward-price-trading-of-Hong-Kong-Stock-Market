
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(beepr)
library(lubridate)
library(zoo)
rm(list = ls())
##############################################################################
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


#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pdataF= read.csv("rSplitsFfProfLevTobinQ10Clean.csv",header = T)
# RSS stocks without market cap on rss split date or prior

pdataF$ExDate <- ymd(as.Date(as.character(pdataF$ExDate)))
pdataF$BQ0Yp1 <- ymd(as.Date(as.character(pdataF$BQ0Yp1)))
pdataF$EQ0Yp1 <- ymd(as.Date(as.character(pdataF$EQ0Yp1)))
pdataF$mktReturn <- NA

#daily record of adjusted monthyly return dated on monthend in % with NA on holidays
monthreturn1 = read.csv("adjRLog1.csv",header = T) 
#monthreturn2 = read.csv("CUMULATIVE_TOT_RETURN_2Md.csv",header = T)
###############################################################################
#
CodeNames <- colnames(pd1)
pd1Code = 2L
for(pd1Code in 2L: ncol(pd1)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pdataF$mktReturn <- NA
  #
  ###########################################################################
  
j = 1L
for(j in 1L: nrow(pdataF)){
  
  BeginDay = pdataF[j,"BQ0Yp1"]
  EndDay = pdataF[j,"EQ0Yp1"]

    monthreturnData<-monthreturn1
  monthReturn = monthreturnData[,codename]
  monthReturnDate = as.character(monthreturnData[,datename])
  monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
  BeginDayIndex <- which(monthReturnDate > BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0 ){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"mktReturn"] = exp(sum(dailyReturn, na.rm = TRUE))
    
  }
  

}
  ###############################################################################
  #
  pd1[,pd1Code]= pdataF$mktReturn
  
  print(codename)
}

#
###########################################################################
beep("coin")
pd1$X <-NULL
write.csv(pd1,file="AB1tempFmktReturn11.csv")



###########------------------------------------ 2 F --------------------------------------
rm(list = ls())
##############################################################################
pdataF= read.csv("tempDates2.csv",header = T)
pdataF[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pdataF$ExDate)))
rownames(pdataF)<-myrownames

#
##############################################################################


#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pdataF= read.csv("rSplitsFfProfLevTobinQ10Clean.csv",header = T)
# RSS stocks without market cap on rss split date or prior

pdataF$ExDate <- ymd(as.Date(as.character(pdataF$ExDate)))
pdataF$BQ0Yp1 <- ymd(as.Date(as.character(pdataF$BQ0Yp1)))
pdataF$EQ0Yp1 <- ymd(as.Date(as.character(pdataF$EQ0Yp1)))
pdataF$mktReturn <- NA

##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pd1= read.csv("ABtemp2.csv",header = T)
pd1$X<-NULL
myrownames <- ymd(as.character(pd1[,1]))
rownames(pd1)<-myrownames
##############################################################################

#
##############################################################################


#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pdataF= read.csv("rSplitsFfProfLevTobinQ10Clean.csv",header = T)
# RSS stocks without market cap on rss split date or prior

pdataF$mktReturn <- NA

#daily record of adjusted monthyly return dated on monthend in % with NA on holidays
monthreturn1 = read.csv("adjRLog2.csv",header = T) 
#monthreturn2 = read.csv("CUMULATIVE_TOT_RETURN_2Md.csv",header = T)
###############################################################################
#
CodeNames <- colnames(pd1)
pd1Code = 2L
for(pd1Code in 2L: ncol(pd1)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pdataF$mktReturn <- NA
  #
  ###########################################################################
  
  j = 1L
  for(j in 1L: nrow(pdataF)){
    
    BeginDay = pdataF[j,"BQ0Yp1"]
    EndDay = pdataF[j,"EQ0Yp1"]
    
    monthreturnData<-monthreturn1
    monthReturn = monthreturnData[,codename]
    monthReturnDate = as.character(monthreturnData[,datename])
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    BeginDayIndex <- which(monthReturnDate > BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0 ){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pdataF[j,"mktReturn"] = exp(sum(dailyReturn, na.rm = TRUE))
      
    }
    
   
  }
  ###############################################################################
  #
  pd1[,pd1Code]= pdataF$mktReturn
  
  print(codename)
}

#
###########################################################################
beep("coin")
pd1$X <-NULL
write.csv(pd1,file="AB2tempFmktReturn11.csv")