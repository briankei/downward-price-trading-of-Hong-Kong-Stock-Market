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
pdataF3<- pdataF # pspread and fspread 
pdataF4<- pdataF # pspread and fspread 
pdataF5<- pdataF # pspread and fspread 
pdataF6<- pdataF # pspread and fspread 
pdataF7<- pdataF # pspread and fspread 
pdataF8<- pdataF # pspread and fspread 
pdataF9<- pdataF # pspread and fspread 
pdataF10<- pdataF # pspread and fspread 
pdataF11<- pdataF # pspread and fspread 

##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"?.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$ExDate <- ymd(as.character(pd1$ExDate))
pd1$BQ0 <- ymd(as.character(pd1$BQ0))
pd1$EQ0 <- ymd(as.character(pd1$EQ0))


pd1$BQp1 <-ymd(as.character(pd1$BQp1))   
pd1$EQp1 <-ymd(as.character(pd1$EQp1))
pd1$BQp2 <-floor_date(pd1$BQp1-1,"quarter")    
pd1$EQp2 <-ceiling_date(pd1$BQp2,"quarter")-1  
pd1$BQp3 <-floor_date(pd1$BQp2-1,"quarter")    
pd1$EQp3 <-ceiling_date(pd1$BQp3,"quarter")-1  
pd1$BQp4 <-floor_date(pd1$BQp3-1,"quarter")    
pd1$EQp4 <-ceiling_date(pd1$BQp4,"quarter")-1  
#pd1$BQp8 <-ymd(as.character(pd1$BQp8)) 
#pd1$EQp8 <-ymd(as.character(pd1$EQp8))


pd1$BQn1 <-ymd(as.character(pd1$BQn1))
pd1$EQn1 <-ymd(as.character(pd1$EQn1))
pd1$BQn2 <-floor_date(pd1$EQn1+1,"quarter")
pd1$EQn2 <-ceiling_date(pd1$BQn2,"quarter")-1
pd1$BQn3 <-floor_date(pd1$EQn2+1,"quarter")
pd1$EQn3 <-ceiling_date(pd1$BQn3,"quarter")-1
pd1$BQn4 <-floor_date(pd1$EQn3+1,"quarter")
pd1$EQn4 <-ceiling_date(pd1$BQn4,"quarter")-1
pd1$BQn5 <-floor_date(pd1$EQn4+1,"quarter")
pd1$EQn5 <-ceiling_date(pd1$BQn5,"quarter")-1
pd1$BQn6 <-floor_date(pd1$EQn5+1,"quarter")
pd1$EQn6 <-ceiling_date(pd1$BQn6,"quarter")-1
pd1$BQn7 <-floor_date(pd1$EQn6+1,"quarter")
pd1$EQn7 <-ceiling_date(pd1$BQn7,"quarter")-1
pd1$BQn8 <-floor_date(pd1$EQn7+1,"quarter")
pd1$EQn8 <-ceiling_date(pd1$BQn8,"quarter")-1





pd1$NISQp4 <- NA
pd1$NISQp3 <- NA
pd1$NISQp2 <- NA
pd1$NISQp1 <- NA
pd1$NISQ0 <- NA
pd1$NISQn5 <- NA
pd1$NISQn6 <- NA
pd1$NISQn7 <- NA
pd1$NISQn8 <- NA
pd1$preNIS <-NA
pd1$postNIS <-NA

Last1 = read.csv("NUM_INSIDERS_OWNING_SHARES_Q1dFix.csv",header = T)


###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$NISQp4 <- NA
  pd1$NISQp3 <- NA
  pd1$NISQp2 <- NA
  pd1$NISQp1 <- NA
  pd1$NISQ0 <- NA
  pd1$NISQn5 <- NA
  pd1$NISQn6 <- NA
  pd1$NISQn7 <- NA
  pd1$NISQn8 <- NA
  pd1$preNIS <-NA
  pd1$postNIS <-NA
  
  #
  ###########################################################################
  
  

j = 1L
for(j in 1L:nrow(pd1)){
  
    monthreturnData<-Last1
    
  monthReturn = monthreturnData[,codename]
  monthReturnDate = as.character(monthreturnData[,datename])
  monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
  
  BeginDay = pd1[j,"BQp4"]
  EndDay = pd1[j,"EQp4"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQp4"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQp3"]
  EndDay = pd1[j,"EQp3"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQp3"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQp2"]
  EndDay = pd1[j,"EQp2"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQp2"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQp1"]
  EndDay = pd1[j,"EQp1"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQp1"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQ0"]
  EndDay = pd1[j,"EQ0"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQ0"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQn5"]
  EndDay = pd1[j,"EQn5"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQn5"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQn6"]
  EndDay = pd1[j,"EQn6"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQn6"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQn7"]
  EndDay = pd1[j,"EQn7"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQn7"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQn8"]
  EndDay = pd1[j,"EQn8"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NISQn8"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  if(is.na(pd1[j,"NISQn8"])==FALSE &&  
     is.na(pd1[j,"NISQn7"])==FALSE &&  
     is.na(pd1[j,"NISQn6"])==FALSE && 
     is.na(pd1[j,"NISQn5"])==FALSE ){
    pd1[j,"postNIS"] = mean(c(pd1[j,"NISQn8"],pd1[j,"NISQn7"],pd1[j,"NISQn6"],pd1[j,"NISQn5"]),na.rm= TRUE)
  }
  
  if(is.na(pd1[j,"NISQp1"])==FALSE &&  
     is.na(pd1[j,"NISQp2"])==FALSE &&  
     is.na(pd1[j,"NISQp3"])==FALSE && 
     is.na(pd1[j,"NISQp4"])==FALSE ){
    pd1[j,"priNIS"] = mean(c(pd1[j,"NISQp1"],pd1[j,"NISQp2"],pd1[j,"NISQp3"],pd1[j,"NISQp4"]),na.rm= TRUE)
  }
  
  
  
  
}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$NISQp4
  pdataF2[,pd1Code]= pd1$NISQp3
  pdataF3[,pd1Code]= pd1$NISQp2
  pdataF4[,pd1Code]= pd1$NISQp1
  pdataF5[,pd1Code]= pd1$NISQ0
  pdataF6[,pd1Code]= pd1$NISQn5
  pdataF7[,pd1Code]= pd1$NISQn6
  pdataF8[,pd1Code]= pd1$NISQn7
  pdataF9[,pd1Code]= pd1$NISQn8
  pdataF10[,pd1Code]= pd1$priNIS
  pdataF11[,pd1Code]= pd1$postNIS
  
  print(codename)
}

#
###########################################################################

beep("coin")

pdataF$X <-NULL
write.csv(pdataF,file="AB1tempF2NISQp4-20.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB1tempF2NISQp3-20.csv")
pdataF3$X <-NULL
write.csv(pdataF3,file="AB1tempF2NISQp2-20.csv")
pdataF4$X <-NULL
write.csv(pdataF4,file="AB1tempF2NISQp1-20.csv")
pdataF5$X <-NULL
write.csv(pdataF5,file="AB1tempF2NISQ0-20.csv")
pdataF6$X <-NULL
write.csv(pdataF6,file="AB1tempF2NISQn5-20.csv")
pdataF7$X <-NULL
write.csv(pdataF7,file="AB1tempF2NISQn6-20.csv")
pdataF8$X <-NULL
write.csv(pdataF8,file="AB1tempF2NISQn7-20.csv")
pdataF9$X <-NULL
write.csv(pdataF9,file="AB1tempF2NISQn8-20.csv")
pdataF10$X <-NULL
write.csv(pdataF10,file="AB1tempF2priNIS-20.csv")
pdataF11$X <-NULL
write.csv(pdataF11,file="AB1tempF2postNIS-20.csv")

########################--------------------  2f  ----------------------------
rm(list = ls())

##############################################################################
#
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
pdataF= read.csv("ABtemp2.csv",header = T)
pdataF$X<-NULL
myrownames <- ymd(as.character(pdataF[,1]))
rownames(pdataF)<-myrownames

pdataF2<- pdataF # pspread and fspread 
pdataF3<- pdataF # pspread and fspread 
pdataF4<- pdataF # pspread and fspread 
pdataF5<- pdataF # pspread and fspread 
pdataF6<- pdataF # pspread and fspread 
pdataF7<- pdataF # pspread and fspread 
pdataF8<- pdataF # pspread and fspread 
pdataF9<- pdataF # pspread and fspread 
pdataF10<- pdataF # pspread and fspread 
pdataF11<- pdataF # pspread and fspread 
##############################################################################
pd1= read.csv("tempDates2.csv",header = T)
pd1[,"?.."]<-NULL
myrownames <- ymd(as.Date(as.character(pd1$ExDate)))
rownames(pd1)<-myrownames

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
#pd1= read.csv("rSplitsFBooktoMarket3Clean.csv",header = T)

#
##############################################################################


pd1$ExDate <- ymd(as.character(pd1$ExDate))
pd1$BQ0 <- ymd(as.character(pd1$BQ0))
pd1$EQ0 <- ymd(as.character(pd1$EQ0))


pd1$BQp1 <-ymd(as.character(pd1$BQp1))   
pd1$EQp1 <-ymd(as.character(pd1$EQp1))
pd1$BQp2 <-floor_date(pd1$BQp1-1,"quarter")    
pd1$EQp2 <-ceiling_date(pd1$BQp2,"quarter")-1  
pd1$BQp3 <-floor_date(pd1$BQp2-1,"quarter")    
pd1$EQp3 <-ceiling_date(pd1$BQp3,"quarter")-1  
pd1$BQp4 <-floor_date(pd1$BQp3-1,"quarter")    
pd1$EQp4 <-ceiling_date(pd1$BQp4,"quarter")-1  
#pd1$BQp8 <-ymd(as.character(pd1$BQp8)) 
#pd1$EQp8 <-ymd(as.character(pd1$EQp8))


pd1$BQn1 <-ymd(as.character(pd1$BQn1))
pd1$EQn1 <-ymd(as.character(pd1$EQn1))
pd1$BQn2 <-floor_date(pd1$EQn1+1,"quarter")
pd1$EQn2 <-ceiling_date(pd1$BQn2,"quarter")-1
pd1$BQn3 <-floor_date(pd1$EQn2+1,"quarter")
pd1$EQn3 <-ceiling_date(pd1$BQn3,"quarter")-1
pd1$BQn4 <-floor_date(pd1$EQn3+1,"quarter")
pd1$EQn4 <-ceiling_date(pd1$BQn4,"quarter")-1
pd1$BQn5 <-floor_date(pd1$EQn4+1,"quarter")
pd1$EQn5 <-ceiling_date(pd1$BQn5,"quarter")-1
pd1$BQn6 <-floor_date(pd1$EQn5+1,"quarter")
pd1$EQn6 <-ceiling_date(pd1$BQn6,"quarter")-1
pd1$BQn7 <-floor_date(pd1$EQn6+1,"quarter")
pd1$EQn7 <-ceiling_date(pd1$BQn7,"quarter")-1
pd1$BQn8 <-floor_date(pd1$EQn7+1,"quarter")
pd1$EQn8 <-ceiling_date(pd1$BQn8,"quarter")-1





pd1$NISQp4 <- NA
pd1$NISQp3 <- NA
pd1$NISQp2 <- NA
pd1$NISQp1 <- NA
pd1$NISQ0 <- NA
pd1$NISQn5 <- NA
pd1$NISQn6 <- NA
pd1$NISQn7 <- NA
pd1$NISQn8 <- NA
pd1$preNIS <-NA
pd1$postNIS <-NA

Last1 = read.csv("NUM_INSIDERS_OWNING_SHARES_Q2dFix.csv",header = T)


###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$NISQp4 <- NA
  pd1$NISQp3 <- NA
  pd1$NISQp2 <- NA
  pd1$NISQp1 <- NA
  pd1$NISQ0 <- NA
  pd1$NISQn5 <- NA
  pd1$NISQn6 <- NA
  pd1$NISQn7 <- NA
  pd1$NISQ8 <- NA
  pd1$preNIS <-NA
  pd1$postNIS <-NA
  
  #
  ###########################################################################
  
  
  
  j = 1L
  for(j in 1L:nrow(pd1)){
    
    monthreturnData<-Last1
    
    monthReturn = monthreturnData[,codename]
    monthReturnDate = as.character(monthreturnData[,datename])
    monthReturnDate = ymd(as.Date(monthReturnDate,format = "%m/%d/%Y"))
    
    BeginDay = pd1[j,"BQp4"]
    EndDay = pd1[j,"EQp4"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQp4"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQp3"]
    EndDay = pd1[j,"EQp3"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQp3"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQp2"]
    EndDay = pd1[j,"EQp2"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQp2"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQp1"]
    EndDay = pd1[j,"EQp1"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQp1"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQ0"]
    EndDay = pd1[j,"EQ0"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQ0"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQn5"]
    EndDay = pd1[j,"EQn5"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQn5"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQn6"]
    EndDay = pd1[j,"EQn6"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQn6"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQn7"]
    EndDay = pd1[j,"EQn7"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQn7"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQn8"]
    EndDay = pd1[j,"EQn8"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NISQn8"] = mean(dailyReturn, na.rm = TRUE)
    }
    
   if(is.na(pd1[j,"NISQn8"])==FALSE &&  
      is.na(pd1[j,"NISQn7"])==FALSE &&  
      is.na(pd1[j,"NISQn6"])==FALSE && 
      is.na(pd1[j,"NISQn5"])==FALSE ){
      pd1[j,"postNIS"] = mean(c(pd1[j,"NISQn8"],pd1[j,"NISQn7"],pd1[j,"NISQn6"],pd1[j,"NISQn5"]),na.rm= TRUE)
    }
    
    if(is.na(pd1[j,"NISQp1"])==FALSE &&  
       is.na(pd1[j,"NISQp2"])==FALSE &&  
       is.na(pd1[j,"NISQp3"])==FALSE && 
       is.na(pd1[j,"NISQp4"])==FALSE ){
      pd1[j,"priNIS"] = mean(c(pd1[j,"NISQp1"],pd1[j,"NISQp2"],pd1[j,"NISQp3"],pd1[j,"NISQp4"]),na.rm= TRUE)
    }
    
    
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$NISQp4
  pdataF2[,pd1Code]= pd1$NISQp3
  pdataF3[,pd1Code]= pd1$NISQp2
  pdataF4[,pd1Code]= pd1$NISQp1
  pdataF5[,pd1Code]= pd1$NISQ0
  pdataF6[,pd1Code]= pd1$NISQn5
  pdataF7[,pd1Code]= pd1$NISQn6
  pdataF8[,pd1Code]= pd1$NISQn7
  pdataF9[,pd1Code]= pd1$NISQn8
  pdataF10[,pd1Code]= pd1$priNIS
  pdataF11[,pd1Code]= pd1$postNIS
  
  print(codename)
}

#
###########################################################################

beep("coin")

pdataF$X <-NULL
write.csv(pdataF,file="AB2tempF2NISQp4-20.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB2tempF2NISQp3-20.csv")
pdataF3$X <-NULL
write.csv(pdataF3,file="AB2tempF2NISQp2-20.csv")
pdataF4$X <-NULL
write.csv(pdataF4,file="AB2tempF2NISQp1-20.csv")
pdataF5$X <-NULL
write.csv(pdataF5,file="AB2tempF2NISQ0-20.csv")
pdataF6$X <-NULL
write.csv(pdataF6,file="AB2tempF2NISQn5-20.csv")
pdataF7$X <-NULL
write.csv(pdataF7,file="AB2tempF2NISQn6-20.csv")
pdataF8$X <-NULL
write.csv(pdataF8,file="AB2tempF2NISQn7-20.csv")
pdataF9$X <-NULL
write.csv(pdataF9,file="AB2tempF2NISQn8-20.csv")
pdataF10$X <-NULL
write.csv(pdataF10,file="AB2tempF2priNIS-20.csv")
pdataF11$X <-NULL
write.csv(pdataF11,file="AB2tempF2postNIS-20.csv")
