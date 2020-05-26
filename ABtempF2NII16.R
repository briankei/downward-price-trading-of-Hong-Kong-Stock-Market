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





pd1$NIIQp4 <- NA
pd1$NIIQp3 <- NA
pd1$NIIQp2 <- NA
pd1$NIIQp1 <- NA
pd1$NIIQ0 <- NA
pd1$NIIQn5 <- NA
pd1$NIIQn6 <- NA
pd1$NIIQn7 <- NA
pd1$NIIQn8 <- NA
pd1$preNII <-NA
pd1$postNII <-NA

Last1 = read.csv("INSTIT_ONWER_1WdFix.csv",header = T)


###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$NIIQp4 <- NA
  pd1$NIIQp3 <- NA
  pd1$NIIQp2 <- NA
  pd1$NIIQp1 <- NA
  pd1$NIIQ0 <- NA
  pd1$NIIQn5 <- NA
  pd1$NIIQn6 <- NA
  pd1$NIIQn7 <- NA
  pd1$NIIQn8 <- NA
  pd1$preNII <-NA
  pd1$postNII <-NA
  
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
    pd1[j,"NIIQp4"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQp3"]
  EndDay = pd1[j,"EQp3"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NIIQp3"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQp2"]
  EndDay = pd1[j,"EQp2"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NIIQp2"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQp1"]
  EndDay = pd1[j,"EQp1"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NIIQp1"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQ0"]
  EndDay = pd1[j,"EQ0"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NIIQ0"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQn5"]
  EndDay = pd1[j,"EQn5"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NIIQn5"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQn6"]
  EndDay = pd1[j,"EQn6"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NIIQn6"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQn7"]
  EndDay = pd1[j,"EQn7"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NIIQn7"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  BeginDay = pd1[j,"BQn8"]
  EndDay = pd1[j,"EQn8"]
  BeginDayIndex <- which(monthReturnDate >= BeginDay)
  EndDayIndex <- which(monthReturnDate <= EndDay)
  if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
    dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
    pd1[j,"NIIQn8"] = mean(dailyReturn, na.rm = TRUE)
  }
  
  if(is.na(pd1[j,"NIIQn8"])==FALSE &&  
     is.na(pd1[j,"NIIQn7"])==FALSE &&  
     is.na(pd1[j,"NIIQn6"])==FALSE && 
     is.na(pd1[j,"NIIQn5"])==FALSE ){
    pd1[j,"postNII"] = mean(c(pd1[j,"NIIQn8"],pd1[j,"NIIQn7"],pd1[j,"NIIQn6"],pd1[j,"NIIQn5"]),na.rm= TRUE)
  }
  
  if(is.na(pd1[j,"NIIQp1"])==FALSE &&  
     is.na(pd1[j,"NIIQp2"])==FALSE &&  
     is.na(pd1[j,"NIIQp3"])==FALSE && 
     is.na(pd1[j,"NIIQp4"])==FALSE ){
    pd1[j,"priNII"] = mean(c(pd1[j,"NIIQp1"],pd1[j,"NIIQp2"],pd1[j,"NIIQp3"],pd1[j,"NIIQp4"]),na.rm= TRUE)
  }
  
  
  
  
}
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$NIIQp4
  pdataF2[,pd1Code]= pd1$NIIQp3
  pdataF3[,pd1Code]= pd1$NIIQp2
  pdataF4[,pd1Code]= pd1$NIIQp1
  pdataF5[,pd1Code]= pd1$NIIQ0
  pdataF6[,pd1Code]= pd1$NIIQn5
  pdataF7[,pd1Code]= pd1$NIIQn6
  pdataF8[,pd1Code]= pd1$NIIQn7
  pdataF9[,pd1Code]= pd1$NIIQn8
  pdataF10[,pd1Code]= pd1$priNII
  pdataF11[,pd1Code]= pd1$postNII
  
  print(codename)
}

#
###########################################################################

beep("coin")

pdataF$X <-NULL
write.csv(pdataF,file="AB1tempF2NIIQp4-16.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB1tempF2NIIQp3-16.csv")
pdataF3$X <-NULL
write.csv(pdataF3,file="AB1tempF2NIIQp2-16.csv")
pdataF4$X <-NULL
write.csv(pdataF4,file="AB1tempF2NIIQp1-16.csv")
pdataF5$X <-NULL
write.csv(pdataF5,file="AB1tempF2NIIQ0-16.csv")
pdataF6$X <-NULL
write.csv(pdataF6,file="AB1tempF2NIIQn5-16.csv")
pdataF7$X <-NULL
write.csv(pdataF7,file="AB1tempF2NIIQn6-16.csv")
pdataF8$X <-NULL
write.csv(pdataF8,file="AB1tempF2NIIQn7-16.csv")
pdataF9$X <-NULL
write.csv(pdataF9,file="AB1tempF2NIIQn8-16.csv")
pdataF10$X <-NULL
write.csv(pdataF10,file="AB1tempF2priNII-16.csv")
pdataF11$X <-NULL
write.csv(pdataF11,file="AB1tempF2postNII-16.csv")

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





pd1$NIIQp4 <- NA
pd1$NIIQp3 <- NA
pd1$NIIQp2 <- NA
pd1$NIIQp1 <- NA
pd1$NIIQ0 <- NA
pd1$NIIQn5 <- NA
pd1$NIIQn6 <- NA
pd1$NIIQn7 <- NA
pd1$NIIQn8 <- NA
pd1$preNII <-NA
pd1$postNII <-NA

Last1 = read.csv("INSTIT_ONWER_2WdFix.csv",header = T)


###############################################################################
#
CodeNames <- colnames(pdataF)
pd1Code = 2L
for(pd1Code in 2L: ncol(pdataF)){
  codename = CodeNames[pd1Code]
  code = as.integer(gsub("X","",codename))
  datename = paste("Date",as.character(code),sep="")
  pd1$NIIQp4 <- NA
  pd1$NIIQp3 <- NA
  pd1$NIIQp2 <- NA
  pd1$NIIQp1 <- NA
  pd1$NIIQ0 <- NA
  pd1$NIIQn5 <- NA
  pd1$NIIQn6 <- NA
  pd1$NIIQn7 <- NA
  pd1$NIIQ8 <- NA
  pd1$preNII <-NA
  pd1$postNII <-NA
  
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
      pd1[j,"NIIQp4"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQp3"]
    EndDay = pd1[j,"EQp3"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NIIQp3"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQp2"]
    EndDay = pd1[j,"EQp2"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NIIQp2"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQp1"]
    EndDay = pd1[j,"EQp1"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NIIQp1"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQ0"]
    EndDay = pd1[j,"EQ0"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NIIQ0"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQn5"]
    EndDay = pd1[j,"EQn5"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NIIQn5"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQn6"]
    EndDay = pd1[j,"EQn6"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NIIQn6"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQn7"]
    EndDay = pd1[j,"EQn7"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NIIQn7"] = mean(dailyReturn, na.rm = TRUE)
    }
    
    BeginDay = pd1[j,"BQn8"]
    EndDay = pd1[j,"EQn8"]
    BeginDayIndex <- which(monthReturnDate >= BeginDay)
    EndDayIndex <- which(monthReturnDate <= EndDay)
    if(length(BeginDayIndex)>0 && length(EndDayIndex)>0){
      dailyReturn <- monthReturn[BeginDayIndex[1]:EndDayIndex[length(EndDayIndex)]]
      pd1[j,"NIIQn8"] = mean(dailyReturn, na.rm = TRUE)
    }
    
   if(is.na(pd1[j,"NIIQn8"])==FALSE &&  
      is.na(pd1[j,"NIIQn7"])==FALSE &&  
      is.na(pd1[j,"NIIQn6"])==FALSE && 
      is.na(pd1[j,"NIIQn5"])==FALSE ){
      pd1[j,"postNII"] = mean(c(pd1[j,"NIIQn8"],pd1[j,"NIIQn7"],pd1[j,"NIIQn6"],pd1[j,"NIIQn5"]),na.rm= TRUE)
    }
    
    if(is.na(pd1[j,"NIIQp1"])==FALSE &&  
       is.na(pd1[j,"NIIQp2"])==FALSE &&  
       is.na(pd1[j,"NIIQp3"])==FALSE && 
       is.na(pd1[j,"NIIQp4"])==FALSE ){
      pd1[j,"priNII"] = mean(c(pd1[j,"NIIQp1"],pd1[j,"NIIQp2"],pd1[j,"NIIQp3"],pd1[j,"NIIQp4"]),na.rm= TRUE)
    }
    
    
    
  }
  ###############################################################################
  #
  pdataF[,pd1Code]= pd1$NIIQp4
  pdataF2[,pd1Code]= pd1$NIIQp3
  pdataF3[,pd1Code]= pd1$NIIQp2
  pdataF4[,pd1Code]= pd1$NIIQp1
  pdataF5[,pd1Code]= pd1$NIIQ0
  pdataF6[,pd1Code]= pd1$NIIQn5
  pdataF7[,pd1Code]= pd1$NIIQn6
  pdataF8[,pd1Code]= pd1$NIIQn7
  pdataF9[,pd1Code]= pd1$NIIQn8
  pdataF10[,pd1Code]= pd1$priNII
  pdataF11[,pd1Code]= pd1$postNII
  
  print(codename)
}

#
###########################################################################

beep("coin")

pdataF$X <-NULL
write.csv(pdataF,file="AB2tempF2NIIQp4-16.csv")
pdataF2$X <-NULL
write.csv(pdataF2,file="AB2tempF2NIIQp3-16.csv")
pdataF3$X <-NULL
write.csv(pdataF3,file="AB2tempF2NIIQp2-16.csv")
pdataF4$X <-NULL
write.csv(pdataF4,file="AB2tempF2NIIQp1-16.csv")
pdataF5$X <-NULL
write.csv(pdataF5,file="AB2tempF2NIIQ0-16.csv")
pdataF6$X <-NULL
write.csv(pdataF6,file="AB2tempF2NIIQn5-16.csv")
pdataF7$X <-NULL
write.csv(pdataF7,file="AB2tempF2NIIQn6-16.csv")
pdataF8$X <-NULL
write.csv(pdataF8,file="AB2tempF2NIIQn7-16.csv")
pdataF9$X <-NULL
write.csv(pdataF9,file="AB2tempF2NIIQn8-16.csv")
pdataF10$X <-NULL
write.csv(pdataF10,file="AB2tempF2priNII-16.csv")
pdataF11$X <-NULL
write.csv(pdataF11,file="AB2tempF2postNII-16.csv")
