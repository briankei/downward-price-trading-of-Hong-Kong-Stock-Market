# find the important days of RSS:
# BM0  - begin date of ExDate month                   :   ***market cap,***book2market
# EM0  - end date of ExDate month                     :   
# BMp12P11M - # presplit 11 month return
# EMp12P11M - # presplit 11 month return
# BQp1 - begin date of previous quarter               :   presplit change in NII
# EQp1 - end date of previous quarter                 :   presplit change in NII
# BQ0 - begin date of current quarter                 :   ***MVE
# EQ0 - end date of current quarter
# BQn1 - begin date of next quarter
# EQn1 - end date of next quarter
# BQp8 - begin date of previous 8th quarter           :   presplit change in NII
# EQp8 - end date of previous 8th quarter             :   presplit change in NII

# BYp1 - begin date of 1 year period before Ex-Date   : presplit - Risk, illiquid, spread, turnover
# EYp1 - end date of 1 year period before Ex-Date     : presplit - Risk, illiquid, spread, turnover
# BYp2 - begin date of 2nd year prior to Ex-Date      : validity of target stocks, presplit change
# EYp2 - end date of 2nd year prior to Ex-Date        : validity of target stocks, presplit change
# BYn1 - begin date of 1 year period after Ex-Date    : ***BH24 months return, postsplit - Risk, illiquid, spread, turnover
# EYn1 - end date of 1 year period after Ex-Date      : postsplit - Risk, illiquid, spread, turnover
# BYn2 - begin date of 2 year period after Ex-Date    :  delta: postsplit - Risk, illiquid, spread, turnover
# EYn2 - end date of 2 year period after Ex-Date      : ***BH24 months return, delta: postsplit - Risk, illiquid, spread, turnover
# BQ0Yp1 - begin date of 1 year prior to BQ0          : Mkt_Adj_Return
# EQ0Yp1 - end date of 1 year prior to BQ0            : Mkt_Adj_Return

# Yr - current year of Ex-Date
# YrP1 - previous fiscal year                         : previous fiscal year: presplit - ROA, Leverage, Profitablility, TobinQ
# YrN1 - next fiscal year                             : next fiscal year: postsplit - ROA, Leverage, Profitablility, TobinQ
# BQp5M - begin date of 5 months before begin date of current Q :***book2market(BEME)
# BMp12P11M                                           : ***previous 11 month compound return
# EMp12P11M                                           : ***previous 11 month compound return    
#PrePrice                                             : ***Closeing price before Ex-Date
#TargetPrice                                          :  ***PrePrice /  ChangeRatio
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

pd1$BQp1 <- ymd(as.Date(as.character(pd1$BQp1)))
pd1$EQp1 <- ymd(as.Date(as.character(pd1$EQp1 )))
pd1$ExDate <- ymd(as.Date(as.character(pd1$ExDate)))
#
##############################################################################





#pd1$Return24months <- NA
#daily , filled by Blank
Last1 = read.csv("BkPX_LAST_NAdj1Dd.csv",header = T)
#Last2 = read.csv("adjRLog2.csv",header = T)
###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 45L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$BQprice <- NA
  #
  ###########################################################################
  
j = 1
for(j in 1L:nrow(pd1)){ 
  BeginDay = pd1[j,"EQp1"]-30
  EndDay = pd1[j,"EQp1"]+1
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
    dailyReturn <-na.locf(dailyReturn,na.rm = TRUE)
    if(length(dailyReturn)>0)
      pd1[j,"BQprice"] = dailyReturn[length(dailyReturn)]
  }
  

}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$BQprice
  
  print(codename)
}

#
###########################################################################
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB1tempBQprice18.csv")
########-----------------------------------------2 file -----------------------------

rm(list = ls())
##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

pd1$BQp1 <- ymd(as.Date(as.character(pd1$BQp1)))
pd1$EQp1 <- ymd(as.Date(as.character(pd1$EQp1 )))
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

#daily , filled by Blank

Last1 = read.csv("BkPX_LAST_NAdj2Dd.csv",header = T)

#Last2 = read.csv("adjRLog2.csv",header = T)
###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$BQprice <- NA
  #
  ###########################################################################
  
  j = 1
  for(j in 1L:nrow(pd1)){ 
    BeginDay = pd1[j,"EQp1"]-30
    EndDay = pd1[j,"EQp1"]+1
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
      dailyReturn <- na.locf(dailyReturn,na.rm = TRUE)
      if(length(dailyReturn)>0)
        pd1[j,"BQprice"] = dailyReturn[length(dailyReturn)]
    }
    
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$BQprice
  print(codename)
}

#
###########################################################################
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="AB2tempBQprice18.csv")



