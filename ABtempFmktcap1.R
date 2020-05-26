

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
rSplits= read.csv("tempDates2.csv",header = T)
rSplits[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(rSplits$ExDate)))
rownames(rSplits)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

rSplits$ExDate <- ymd(as.Date(as.character(rSplits$ExDate)))
rSplits$BM0 <- ymd(as.Date(as.character(rSplits$BM0)))
rSplits$MarketCapM <- NA
#
##############################################################################



curMC1 = read.csv("CUR_MKT_CAP_1Dd.csv",header = T)
#curMC2 = read.csv("CUR_MKT_CAP_2Dd.csv",header = T)

Last1<-curMC1
#Last2<- curMC2


rownames(Last1)<-(c(as.character(as.Date(as.character(Last1$Date1),"%m/%d/%Y"))))

###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  ###########|
  rSplits$MarketCapM <- NA
  #
  ###########################################################################
  
j = 1L
for(j in 1L:nrow(rSplits)){
  
  

    last5price <- c(Last1[as.character(rSplits[j,"BM0"]-4) ,codename],
                    Last1[as.character(rSplits[j,"BM0"]-3) ,codename],
                    Last1[as.character(rSplits[j,"BM0"]-2) ,codename],
                    Last1[as.character(rSplits[j,"BM0"]-1) ,codename],
                    Last1[as.character(rSplits[j,"BM0"]) ,codename])
    last5price <- na.locf(last5price)
    if(length(last5price)>0)
      rSplits[j,"MarketCapM"] = last5price[length(last5price)]
    
    
}
  ###############################################################################
  #
  pdataF[,pd1Code]= rSplits$MarketCapM
  
  print(codename)
}

#
###########################################################################
pdataF$X <- NULL
write.csv(pdataF,"AB1tempFmktcap1.csv")

##------------------------------ 2 F------------------------------------

rm(list = ls())
library(beepr)
library(lubridate)
library(zoo)

##############################################################################
rSplits= read.csv("tempDates2.csv",header = T)
rSplits[,"ï.."]<-NULL
myrownames <- ymd(as.Date(as.character(rSplits$ExDate)))
rownames(rSplits)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

rSplits$ExDate <- ymd(as.Date(as.character(rSplits$ExDate)))
rSplits$BM0 <- ymd(as.Date(as.character(rSplits$BM0)))
rSplits$MarketCapM <- NA
#
##############################################################################

##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pdataF= read.csv("ABtemp2.csv",header = T)
pdataF$X<-NULL
myrownames <- ymd(as.character(pdataF[,1]))
rownames(pdataF)<-myrownames
############################rSplits#########################################
#
##############################################################################



curMC1 = read.csv("CUR_MKT_CAP_2Dd.csv",header = T)
#curMC2 = read.csv("CUR_MKT_CAP_2Dd.csv",header = T)

Last1<-curMC1
#Last2<- curMC2


rownames(Last1)<-(c(as.character(as.Date(as.character(Last1$Date2000),"%m/%d/%Y"))))

###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  ###########|
  rSplits$MarketCapM <- NA
  #
  ###########################################################################
  
  j = 1L
  for(j in 1L:nrow(rSplits)){
    
    
    
    last5price <- c(Last1[as.character(rSplits[j,"BM0"]-4) ,codename],
                    Last1[as.character(rSplits[j,"BM0"]-3) ,codename],
                    Last1[as.character(rSplits[j,"BM0"]-2) ,codename],
                    Last1[as.character(rSplits[j,"BM0"]-1) ,codename],
                    Last1[as.character(rSplits[j,"BM0"]) ,codename])
    last5price <- na.locf(last5price)
    if(length(last5price)>0)
      rSplits[j,"MarketCapM"] = last5price[length(last5price)]
    
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= rSplits$MarketCapM
  
  print(codename)
}

#
###########################################################################
pdataF$X <- NULL
write.csv(pdataF,"AB2tempFmktcap1.csv")

