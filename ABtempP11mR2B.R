#temp dates 
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
pdataF$X<-NULL
myrownames <- ymd(as.Date(as.character(pdataF$ExDate)))
rownames(pdataF)<-myrownames
                                                                              #
##############################################################################
pdataF$ExDate <- ymd(as.Date(as.character(pdataF$ExDate)))
pdataF$BMp12P11M <- ymd(as.Date(as.character(pdataF$BMp12P11M)))
pdataF$EMp12P11M <- ymd(as.Date(as.character(pdataF$EMp12P11M)))
#pdataF$m11Return <- NA

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
  pdataF$m11Return <- NA                                                      #
  ###########################################################################
  j = 1L
  
  for(j in 1L: nrow(pdataF)){
    
    BeginDay = pdataF[j,"BMp12P11M"]
    EndDay = pdataF[j,"EMp12P11M"]
#    if( code <2000){
      monthreturnData<-monthreturn1
#    }else{
#      monthreturnData<-monthreturn2
#    }
    monthReturn = monthreturnData[,codename]
    monthReturnDate = as.character(monthreturnData[,datename])
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pdataF[j,"m11Return"] = exp(sum(dailyReturn, na.rm = TRUE))
  
      
    }
    
    
  }
  ###############################################################################
                                                                                #
  pd1[,pd1Code]= pdataF$m11Return
  print(codename)
}

pd1[,"Date1"] = pdataF$ExDate                                                               #
###########################################################################



beep("coin")
pd1$X <-NULL
write.csv(pd1,file="AB1tempP11mR2.csv")

#summary info on 11 month reeturns
summary(pdataF$m11Return)

#############-----------------------------  2 f -----------------------------------------
rm(list = ls())
############################ABtemp2.csv#########################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pd1= read.csv("ABtemp2.csv",header = T)
pd1$X<-NULL
myrownames <- ymd(as.character(pd1[,1]))
rownames(pd1)<-myrownames
##############################tempDates2.csv####################################
##############################################################################
pdataF= read.csv("tempDates2.csv",header = T)
pdataF[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pdataF$ExDate)))
rownames(pdataF)<-myrownames
#
##############################################################################
pdataF$ExDate <- ymd(as.Date(as.character(pdataF$ExDate)))
pdataF$BMp12P11M <- ymd(as.Date(as.character(pdataF$BMp12P11M)))
pdataF$EMp12P11M <- ymd(as.Date(as.character(pdataF$EMp12P11M)))

#
##############################data file##################################
#pdataF$m11Return <- NA

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
  pdataF$m11Return <- NA                                                      #
  ###########################################################################
  j = 1L
  
  for(j in 1L: nrow(pdataF)){
    
    BeginDay = pdataF[j,"BMp12P11M"]
    EndDay = pdataF[j,"EMp12P11M"]
    #    if( code <2000){
    monthreturnData<-monthreturn1
    #    }else{
    #      monthreturnData<-monthreturn2
    #    }
    monthReturn = monthreturnData[,codename]
    monthReturnDate = as.character(monthreturnData[,datename])
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pdataF[j,"m11Return"] = exp(sum(dailyReturn, na.rm = TRUE))
      
    }
    
    
  }
  ###############################################################################
  #
  pd1[,pd1Code]= pdataF$m11Return
  print(codename)
}

pd1[,"Date2000"] = pdataF$ExDate                                                               #
###########################################################################



beep("coin")
pd1$X <-NULL
write.csv(pd1,file="AB2tempP11mR2.csv")

#summary info on 11 month reeturns
summary(pdataF$m11Return)



