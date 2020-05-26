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



Last1 = read.csv("BkTS_NAdj1Yd.csv",header = T)

Ebita1 = read.csv("EBITA_1Y.csv",header = T)


pd1$fROA <- NA
pd1$fROA <- NA
###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$fROA <- NA
  pd1$fROA <- NA
  #
  ###########################################################################
  
  
j = 1
for(j in 1L:nrow(pd1)){
  
  
    monthreturnDataA<-Last1
    monthreturnDataB<-Ebita1
    
   monthReturnA = monthreturnDataA[,codename]
  monthReturnDateA = as.character(monthreturnDataA[,datename])
  monthReturnDateA = ymd(as.Date(monthReturnDateA,format = "%m/%d/%Y"))
  
  monthReturnB = monthreturnDataB[,codename]
  monthReturnDateB = as.character(monthreturnDataB[,datename])
  monthReturnDateB = ymd(as.Date(monthReturnDateB,format = "%m/%d/%Y"))
  
  BeginDay = paste("1/1/",as.character(pd1[j,"YrP1"]),sep="")
  BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
  EndDay = paste("12/31/",as.character(pd1[j,"YrP1"]),sep="")
  EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))
  
  BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
  EndDayIndexA <- which(monthReturnDateA <= EndDay)
  BeginDayIndexB <- which(monthReturnDateB >= BeginDay)
  EndDayIndexB <- which(monthReturnDateB <= EndDay)
  
  if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 && length(BeginDayIndexB)>0 && length(EndDayIndexB)>0){
    dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
    dailyReturnB <- monthReturnB[BeginDayIndexB[1]:EndDayIndexB[length(EndDayIndexB)]]
    pd1[j,"pROA"] = dailyReturnB[length(dailyReturnB)] / dailyReturnA[length(dailyReturnA)]
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
  if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 && length(BeginDayIndexB)>0 && length(EndDayIndexB)>0){
    dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
    dailyReturnB <- monthReturnB[BeginDayIndexB[1]:EndDayIndexB[length(EndDayIndexB)]]
    pd1[j,"fROA"] = dailyReturnB[length(dailyReturnB)] / dailyReturnA[length(dailyReturnA)]
    
  }
  
}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$fROA
  pdataF2[,pd1Code]= pd1$pROA
  
  print(codename)
}

#
###########################################################################
beep("coin")

pdataF$X <-NULL
write.csv(pdataF,file="AB1tempFfROA9.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB1tempFfpROA9.csv")

###__________________________________ 2 F --------------------------------------------------
rm(list = ls())


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



Last1 = read.csv("BkTS_NAdj2Yd.csv",header = T)

Ebita1 = read.csv("EBITA_2Y.csv",header = T)


pd1$fROA <- NA
pd1$fROA <- NA
###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$fROA <- NA
  pd1$fROA <- NA
  #
  ###########################################################################
  
  
  j = 1
  for(j in 1L:nrow(pd1)){
    
    
    monthreturnDataA<-Last1
    monthreturnDataB<-Ebita1
    
    monthReturnA = monthreturnDataA[,codename]
    monthReturnDateA = as.character(monthreturnDataA[,datename])
    monthReturnDateA = ymd(as.Date(monthReturnDateA,format = "%m/%d/%Y"))
    
    monthReturnB = monthreturnDataB[,codename]
    monthReturnDateB = as.character(monthreturnDataB[,datename])
    monthReturnDateB = ymd(as.Date(monthReturnDateB,format = "%m/%d/%Y"))
    
    BeginDay = paste("1/1/",as.character(pd1[j,"YrP1"]),sep="")
    BeginDay = ymd(as.Date(BeginDay,format = "%m/%d/%Y"))
    EndDay = paste("12/31/",as.character(pd1[j,"YrP1"]),sep="")
    EndDay = ymd(as.Date(EndDay,format = "%m/%d/%Y"))
    
    BeginDayIndexA <- which(monthReturnDateA >= BeginDay)
    EndDayIndexA <- which(monthReturnDateA <= EndDay)
    BeginDayIndexB <- which(monthReturnDateB >= BeginDay)
    EndDayIndexB <- which(monthReturnDateB <= EndDay)
    
    if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 && length(BeginDayIndexB)>0 && length(EndDayIndexB)>0){
      dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
      dailyReturnB <- monthReturnB[BeginDayIndexB[1]:EndDayIndexB[length(EndDayIndexB)]]
      pd1[j,"pROA"] = dailyReturnB[length(dailyReturnB)] / dailyReturnA[length(dailyReturnA)]
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
    if(length(BeginDayIndexA)>0 && length(EndDayIndexA)>0 && length(BeginDayIndexB)>0 && length(EndDayIndexB)>0){
      dailyReturnA <- monthReturnA[BeginDayIndexA[1]:EndDayIndexA[length(EndDayIndexA)]]
      dailyReturnB <- monthReturnB[BeginDayIndexB[1]:EndDayIndexB[length(EndDayIndexB)]]
      pd1[j,"fROA"] = dailyReturnB[length(dailyReturnB)] / dailyReturnA[length(dailyReturnA)]
      
    }
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$fROA
  pdataF2[,pd1Code]= pd1$pROA
  
  print(codename)
}

#
###########################################################################
beep("coin")

pdataF$X <-NULL
write.csv(pdataF,file="AB2tempFfROA9.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB2tempFfpROA9.csv")


