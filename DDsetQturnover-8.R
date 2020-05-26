setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(beepr)
library(lubridate)
library(zoo)
rm(list = ls())
##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pdataF= read.csv("mAllDIDDeltaChinaC.csv",header = T)


pdataF$X<-NULL



#
pdataF$TOQ0 <- 0
pdataF$TOQp1 <- 0
pdataF$TOQp2 <- 0
pdataF$TOQp3 <- 0
pdataF$TOQp4 <- 0
pdataF$TOQp5 <- 0
pdataF$TOQp6 <- 0
pdataF$TOQp7 <- 0
pdataF$TOQp8 <- 0
pdataF$TOQn1 <- 0
pdataF$TOQn2 <- 0
pdataF$TOQn3 <- 0
pdataF$TOQn4 <- 0
pdataF$TOQn5 <- 0
pdataF$TOQn6 <- 0
pdataF$TOQn7 <- 0
pdataF$TOQn8 <- 0

pdataF$ARQ0 <- 0
pdataF$ARQp1 <- 0
pdataF$ARQp2 <- 0
pdataF$ARQp3 <- 0
pdataF$ARQp4 <- 0
pdataF$ARQp5 <- 0
pdataF$ARQp6 <- 0
pdataF$ARQp7 <- 0
pdataF$ARQp8 <- 0
pdataF$ARQn1 <- 0
pdataF$ARQn2 <- 0
pdataF$ARQn3 <- 0
pdataF$ARQn4 <- 0
pdataF$ARQn5 <- 0
pdataF$ARQn6 <- 0
pdataF$ARQn7 <- 0
pdataF$ARQn8 <- 0

pdataF$NSQ0 <- 0
pdataF$NSQp1 <- 0
pdataF$NSQp2 <- 0
pdataF$NSQp3 <- 0
pdataF$NSQp4 <- 0
pdataF$NSQp5 <- 0
pdataF$NSQp6 <- 0
pdataF$NSQp7 <- 0
pdataF$NSQp8 <- 0
pdataF$NSQn1 <- 0
pdataF$NSQn2 <- 0
pdataF$NSQn3 <- 0
pdataF$NSQn4 <- 0
pdataF$NSQn5 <- 0
pdataF$NSQn6 <- 0
pdataF$NSQn7 <- 0
pdataF$NSQn8 <- 0

pdataF$NIQ0 <- 0
pdataF$NIQp1 <- 0
pdataF$NIQp2 <- 0
pdataF$NIQp3 <- 0
pdataF$NIQp4 <- 0
pdataF$NIQp5 <- 0
pdataF$NIQp6 <- 0
pdataF$NIQp7 <- 0
pdataF$NIQp8 <- 0
pdataF$NIQn1 <- 0
pdataF$NIQn2 <- 0
pdataF$NIQn3 <- 0
pdataF$NIQn4 <- 0
pdataF$NIQn5 <- 0
pdataF$NIQn6 <- 0
pdataF$NIQn7 <- 0
pdataF$NIQn8 <- 0



pdataF$AZQ0 <- 0
pdataF$AZQp1 <- 0
pdataF$AZQp2 <- 0
pdataF$AZQp3 <- 0
pdataF$AZQp4 <- 0
pdataF$AZQp5 <- 0
pdataF$AZQp6 <- 0
pdataF$AZQp7 <- 0
pdataF$AZQp8 <- 0
pdataF$AZQn1 <- 0
pdataF$AZQn2 <- 0
pdataF$AZQn3 <- 0
pdataF$AZQn4 <- 0
pdataF$AZQn5 <- 0
pdataF$AZQn6 <- 0
pdataF$AZQn7 <- 0
pdataF$AZQn8 <- 0

pdataF$SHQ0 <- 0  #insider share holding pct
pdataF$SHQp1 <- 0
pdataF$SHQp2 <- 0
pdataF$SHQp3 <- 0
pdataF$SHQp4 <- 0
pdataF$SHQp5 <- 0
pdataF$SHQp6 <- 0
pdataF$SHQp7 <- 0
pdataF$SHQp8 <- 0
pdataF$SHQn1 <- 0
pdataF$SHQn2 <- 0
pdataF$SHQn3 <- 0
pdataF$SHQn4 <- 0
pdataF$SHQn5 <- 0
pdataF$SHQn6 <- 0
pdataF$SHQn7 <- 0
pdataF$SHQn8 <- 0


pdataF$IHQ0 <- 0  #insider share holding pct
pdataF$IHQp1 <- 0
pdataF$IHQp2 <- 0
pdataF$IHQp3 <- 0
pdataF$IHQp4 <- 0
pdataF$IHQp5 <- 0
pdataF$IHQp6 <- 0
pdataF$IHQp7 <- 0
pdataF$IHQp8 <- 0
pdataF$IHQn1 <- 0
pdataF$IHQn2 <- 0
pdataF$IHQn3 <- 0
pdataF$IHQn4 <- 0
pdataF$IHQn5 <- 0
pdataF$IHQn6 <- 0
pdataF$IHQn7 <- 0
pdataF$IHQn8 <- 0


pdataF$LVQ0 <- 0  #leverage
pdataF$LVQp1 <- 0
pdataF$LVQp2 <- 0
pdataF$LVQp3 <- 0
pdataF$LVQp4 <- 0
pdataF$LVQp5 <- 0
pdataF$LVQp6 <- 0
pdataF$LVQp7 <- 0
pdataF$LVQp8 <- 0
pdataF$LVQn1 <- 0
pdataF$LVQn2 <- 0
pdataF$LVQn3 <- 0
pdataF$LVQn4 <- 0
pdataF$LVQn5 <- 0
pdataF$LVQn6 <- 0
pdataF$LVQn7 <- 0
pdataF$LVQn8 <- 0

pdataF$IQQ0 <- 0  #
pdataF$IQQp1 <- 0
pdataF$IQQp2 <- 0
pdataF$IQQp3 <- 0
pdataF$IQQp4 <- 0
pdataF$IQQp5 <- 0
pdataF$IQQp6 <- 0
pdataF$IQQp7 <- 0
pdataF$IQQp8 <- 0
pdataF$IQQn1 <- 0
pdataF$IQQn2 <- 0
pdataF$IQQn3 <- 0
pdataF$IQQn4 <- 0
pdataF$IQQn5 <- 0
pdataF$IQQn6 <- 0
pdataF$IQQn7 <- 0
pdataF$IQQn8 <- 0

pdataF$RSQ0 <- 0  #
pdataF$RSQp1 <- 0
pdataF$RSQp2 <- 0
pdataF$RSQp3 <- 0
pdataF$RSQp4 <- 0
pdataF$RSQp5 <- 0
pdataF$RSQp6 <- 0
pdataF$RSQp7 <- 0
pdataF$RSQp8 <- 0
pdataF$RSQn1 <- 0
pdataF$RSQn2 <- 0
pdataF$RSQn3 <- 0
pdataF$RSQn4 <- 0
pdataF$RSQn5 <- 0
pdataF$RSQn6 <- 0
pdataF$RSQn7 <- 0
pdataF$RSQn8 <- 0


pdataF$RSYp1 <- 0
pdataF$RSYp2 <- 0
pdataF$TOYp1 <- 0
pdataF$TOYp2 <- 0
pdataF$SRYp1 <- 0
pdataF$SRYp2 <- 0
pdataF$IQYp1 <- 0
pdataF$IQYp2 <- 0

TO1 = read.csv("turnover1.csv",header = T)
TO2 = read.csv("turnover2.csv",header = T)
AR1 = read.csv("adjRLog1.csv",header = T)
AR2 = read.csv("adjRLog2.csv",header = T)
NS1 = read.csv("NUM_INSIDERS_OWNING_SHARES_Q1dFix.csv",header = T)
NS2 = read.csv("NUM_INSIDERS_OWNING_SHARES_Q2dFix.csv",header = T)
RS1 = read.csv("BkDAILYRETURN_NAdj1Dd.csv",header = T) #risk
RS2 = read.csv("BkDAILYRETURN_NAdj2Dd.csv",header = T)
SR1 = read.csv("spread1.csv",header = T)
SR2 = read.csv("spread2.csv",header = T)
IQ1 = read.csv("illiq1.csv",header = T)
IQ2 = read.csv("illiq2.csv",header = T)
NI1 = read.csv("INSTIT_ONWER_1WdFix.csv",header = T)
NI2 = read.csv("INSTIT_ONWER_2WdFix.csv",header = T)
AZ1 = read.csv("TOT_ANALYST_REC_Q1d.csv",header = T)
AZ2 = read.csv("TOT_ANALYST_REC_Q2d.csv",header = T)
LV1 = read.csv("leverage1.csv",header = T)
LV2 = read.csv("leverage2.csv",header = T)
SH1 = read.csv("PCT_INSIDER_SHARES_OUT_Q1dFix.csv",header = T)  #insider holding percentage
SH2 = read.csv("PCT_INSIDER_SHARES_OUT_Q2dFix.csv",header = T)
IH1 = read.csv("EQY_INST_PCT_SH_OUT_Q1dFix.csv",header = T)
IH2 = read.csv("EQY_INST_PCT_SH_OUT_Q2dFix.csv",header = T)

###############################################################################

j = 1L
for(j in 1L: nrow(pdataF)){
  print(as.character(j))
  codename = as.character(pdataF[j,"Code"])
  code = as.integer(gsub("X","",codename))
  ExDate = ymd(pdataF[j,"Date"])
  datename = paste("Date",as.character(code),sep="")
  
  # on calendar days 
  BQ0 <-floor_date(ExDate,"quarter") 
  EQ0 <-ceiling_date(ExDate,"quarter")-1
  BQp1 <-floor_date(BQ0-1,"quarter")    
  EQp1 <-ceiling_date(BQp1,"quarter")-1  
  BQp2 <-floor_date(BQp1-1,"quarter")    
  EQp2 <-ceiling_date(BQp2,"quarter")-1  
  BQp3 <-floor_date(BQp2-1,"quarter")    
  EQp3 <-ceiling_date(BQp3,"quarter")-1  
  BQp4 <-floor_date(BQp3-1,"quarter")    
  EQp4 <-ceiling_date(BQp4,"quarter")-1  
  BQp5 <-floor_date(BQp4-1,"quarter")    
  EQp5 <-ceiling_date(BQp5,"quarter")-1  
  BQp6 <-floor_date(BQp5-1,"quarter")    
  EQp6 <-ceiling_date(BQp6,"quarter")-1  
  BQp7 <-floor_date(BQp6-1,"quarter")    
  EQp7 <-ceiling_date(BQp7,"quarter")-1  
  BQp8 <-floor_date(BQp7-1,"quarter")    
  EQp8 <-ceiling_date(BQp8,"quarter")-1  
  
  BQn1 <-floor_date(EQ0+1,"quarter")
  EQn1 <-ceiling_date(BQn1,"quarter")-1
  BQn2 <-floor_date(EQn1+1,"quarter")
  EQn2 <-ceiling_date(BQn2,"quarter")-1
  BQn3 <-floor_date(EQn2+1,"quarter")
  EQn3 <-ceiling_date(BQn3,"quarter")-1
  BQn4 <-floor_date(EQn3+1,"quarter")
  EQn4 <-ceiling_date(BQn4,"quarter")-1
  BQn5 <-floor_date(EQn4+1,"quarter")
  EQn5 <-ceiling_date(BQn5,"quarter")-1
  BQn6 <-floor_date(EQn5+1,"quarter")
  EQn6 <-ceiling_date(BQn6,"quarter")-1
  BQn7 <-floor_date(EQn6+1,"quarter")
  EQn7 <-ceiling_date(BQn7,"quarter")-1
  BQn8 <-floor_date(EQn7+1,"quarter")
  EQn8 <-ceiling_date(BQn8,"quarter")-1
  
  #calculate in periods
  BYp1 <-ExDate-366 
  EYp1 <-ExDate-1   
  BYp2 <-BYp1-366   
  EYp2 <-BYp1-1    
  BYn1 <-ExDate+1   
  EYn1 <-ExDate+366 
  BYn2 <-EYn1+1     
  EYn2 <-EYn1+366  
  BQ0Yp1 <-BQ0-366 
  EQ0Yp1 <-BQ0-1   
  
  ##########################################
  if(code < 2000){
    TO=TO1
    AR=AR1
    NS=NS1
    RS=RS1
    SR=SR1
    IQ=IQ1
    NI=NI1
    AZ=AZ1
    LV=LV1
    SH=SH1
    IH=IH1
  }else{
    TO=TO2
    AR=AR2
    NS=NS2
    RS=RS2
    SR=SR2
    IQ=IQ2
    NI=NI2
    AZ=AZ2
    LV=LV2
    SH=SH2
    IH=IH2
  }
  
  
  #################  TO  ########################
  
  dataF<-TO
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOQn8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BYp1)
  EndDayIndex <- which(dataD <= EYp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOYp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BYp2)
  EndDayIndex <- which(dataD <= EYp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"TOYp2"] = median(value,na.rm = TRUE)
  }
  
  ###############################################
  #################  AR  ########################
  
  dataF<-AR
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQ0"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQp1"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQp2"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQp3"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQp4"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQp5"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQp6"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQp7"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQp8"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQn1"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQn2"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQn3"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQn4"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQn5"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQn6"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQn7"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"ARQn8"] = (exp(sum(value,na.rm = TRUE))-1)*100
  }
  ###############################################
  #################  NS  ########################
  
  dataF<-NS
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NSQn8"] = median(value,na.rm = TRUE)
  }
  #################  NI  ########################
  
  dataF<-NI
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"NIQn8"] = median(value,na.rm = TRUE)
  }
  #################  AZ  ########################
  
  dataF<-AZ
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"AZQn8"] = median(value,na.rm = TRUE)
  }
  ###############################################
  #################  LV  ########################
  
  dataF<-LV
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"LVQn8"] = median(value,na.rm = TRUE)
  }
  
  ################# IQ ########################
  
  dataF<-IQ
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQQn8"] = median(value,na.rm = TRUE)
  }
  
  
  ################# RS ########################
  
  dataF<-RS
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSQn8"] = median(value,na.rm = TRUE)
  }
  
  
  #################  SH  ########################
  
  dataF<-SH
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SHQn8"] = median(value,na.rm = TRUE)
  }
  
  
  #################  IH  ########################
  
  dataF<-IH
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BQ0)
  EndDayIndex <- which(dataD <= EQ0)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQ0"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp1)
  EndDayIndex <- which(dataD <= EQp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp2)
  EndDayIndex <- which(dataD <= EQp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQp2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp3)
  EndDayIndex <- which(dataD <= EQp3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQp3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp4)
  EndDayIndex <- which(dataD <= EQp4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQp4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp5)
  EndDayIndex <- which(dataD <= EQp5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQp5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp6)
  EndDayIndex <- which(dataD <= EQp6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQp6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp7)
  EndDayIndex <- which(dataD <= EQp7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQp7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQp8)
  EndDayIndex <- which(dataD <= EQp8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQp8"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn1)
  EndDayIndex <- which(dataD <= EQn1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQn1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn2)
  EndDayIndex <- which(dataD <= EQn2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQn2"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn3)
  EndDayIndex <- which(dataD <= EQn3)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQn3"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn4)
  EndDayIndex <- which(dataD <= EQn4)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQn4"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn5)
  EndDayIndex <- which(dataD <= EQn5)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQn5"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn6)
  EndDayIndex <- which(dataD <= EQn6)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQn6"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn7)
  EndDayIndex <- which(dataD <= EQn7)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQn7"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BQn8)
  EndDayIndex <- which(dataD <= EQn8)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IHQn8"] = median(value,na.rm = TRUE)
  }
  
  
  
  
  #################  RISK  ########################
  
  dataF<-RS
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BYp1)
  EndDayIndex <- which(dataD <= EYp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSYp1"] = sd(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BYp2)
  EndDayIndex <- which(dataD <= EYp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"RSYp2"] = sd(value,na.rm = TRUE)
  }
  #################  Spread  ########################
  
  dataF<-SR
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BYp1)
  EndDayIndex <- which(dataD <= EYp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SRYp1"] = median(value,na.rm = TRUE)*100
  }
  BeginDayIndex <- which(dataD >= BYp2)
  EndDayIndex <- which(dataD <= EYp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"SRYp2"] = median(value,na.rm = TRUE)*100
  }
  #################  Illiquid  ########################
  
  dataF<-IQ
  dataV = dataF[,codename]
  dataD = as.character(dataF[,datename])
  dataD = ymd(as.Date(dataD,format = "%m/%d/%Y"))
  
  BeginDayIndex <- which(dataD >= BYp1)
  EndDayIndex <- which(dataD <= EYp1)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQYp1"] = median(value,na.rm = TRUE)
  }
  BeginDayIndex <- which(dataD >= BYp2)
  EndDayIndex <- which(dataD <= EYp2)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    value <- dataV[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pdataF[j,"IQYp2"] = median(value,na.rm = TRUE)
  }
  
  

}


pdataF$pcNI <- NULL
pdataF$pcNS <- NULL
pdataF$pcPS <- NULL
pdataF$pcPI <- NULL
pdataF$pcRS <- NULL
pdataF$pcTO <- NULL
pdataF$pcSR <- NULL
pdataF$pcNI <- pdataF$NIQp1-pdataF$NIQp8
pdataF$pcNS <- pdataF$NSQp1-pdataF$NSQp8
pdataF$pcSH <- pdataF$SHQp1-pdataF$SHQp8
pdataF$pcIH <- pdataF$IHQp1-pdataF$IHQp8
pdataF$pcRS <- pdataF$RSYp1-pdataF$RSYp2
pdataF$pcTO <- pdataF$TOYp1-pdataF$TOYp2
pdataF$pcSR <- pdataF$SRYp1-pdataF$SRYp2
pdataF$pcIQ <- pdataF$IQYp1-pdataF$IQYp2
if(length(a<-which(pdataF$IHQ0>100))>0) pdataF[a,"IHQ0"]=99
if(length(a<-which(pdataF$IHQp1>100))>0) pdataF[a,"IHQp1"]=99
if(length(a<-which(pdataF$IHQp2>100))>0) pdataF[a,"IHQp2"]=99
if(length(a<-which(pdataF$IHQp3>100))>0) pdataF[a,"IHQp3"]=99
if(length(a<-which(pdataF$IHQp4>100))>0) pdataF[a,"IHQp4"]=99
if(length(a<-which(pdataF$IHQp5>100))>0) pdataF[a,"IHQp5"]=99
if(length(a<-which(pdataF$IHQp6>100))>0) pdataF[a,"IHQp6"]=99
if(length(a<-which(pdataF$IHQp7>100))>0) pdataF[a,"IHQp7"]=99
if(length(a<-which(pdataF$IHQp8>100))>0) pdataF[a,"IHQp8"]=99
if(length(a<-which(pdataF$IHQn1>100))>0) pdataF[a,"IHQn1"]=99
if(length(a<-which(pdataF$IHQn2>100))>0) pdataF[a,"IHQn2"]=99
if(length(a<-which(pdataF$IHQn3>100))>0) pdataF[a,"IHQn3"]=99
if(length(a<-which(pdataF$IHQn4>100))>0) pdataF[a,"IHQn4"]=99
if(length(a<-which(pdataF$IHQn5>100))>0) pdataF[a,"IHQn5"]=99
if(length(a<-which(pdataF$IHQn6>100))>0) pdataF[a,"IHQn6"]=99
if(length(a<-which(pdataF$IHQn7>100))>0) pdataF[a,"IHQn7"]=99
if(length(a<-which(pdataF$IHQn8>100))>0) pdataF[a,"IHQn8"]=99

if(length(a<-which(pdataF$SHQ0>100))>0) pdataF[a,"SHQ0"]=99
if(length(a<-which(pdataF$SHQp1>100))>0) pdataF[a,"SHQp1"]=99
if(length(a<-which(pdataF$SHQp2>100))>0) pdataF[a,"SHQp2"]=99
if(length(a<-which(pdataF$SHQp3>100))>0) pdataF[a,"SHQp3"]=99
if(length(a<-which(pdataF$SHQp4>100))>0) pdataF[a,"SHQp4"]=99
if(length(a<-which(pdataF$SHQp5>100))>0) pdataF[a,"SHQp5"]=99
if(length(a<-which(pdataF$SHQp6>100))>0) pdataF[a,"SHQp6"]=99
if(length(a<-which(pdataF$SHQp7>100))>0) pdataF[a,"SHQp7"]=99
if(length(a<-which(pdataF$SHQp8>100))>0) pdataF[a,"SHQp8"]=99
if(length(a<-which(pdataF$SHQn1>100))>0) pdataF[a,"SHQn1"]=99
if(length(a<-which(pdataF$SHQn2>100))>0) pdataF[a,"SHQn2"]=99
if(length(a<-which(pdataF$SHQn3>100))>0) pdataF[a,"SHQn3"]=99
if(length(a<-which(pdataF$SHQn4>100))>0) pdataF[a,"SHQn4"]=99
if(length(a<-which(pdataF$SHQn5>100))>0) pdataF[a,"SHQn5"]=99
if(length(a<-which(pdataF$SHQn6>100))>0) pdataF[a,"SHQn6"]=99
if(length(a<-which(pdataF$SHQn7>100))>0) pdataF[a,"SHQn7"]=99
if(length(a<-which(pdataF$SHQn8>100))>0) pdataF[a,"SHQn8"]=99


beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="DD_setQturnover-8.csv")

