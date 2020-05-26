#Get  
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp1.csv",header = T)

curMC = read.csv("BkAdjMktBV_NAdj1Dd.csv",header = T)
#curMC2 = read.csv("BkAdjMktBV_NAdj2Dd.csv",header = T)

last = (ncol(temp)+1)/2 - 1
mycolnames <- colnames(temp)
j = 1L
for(j in 1L:last){
  print(as.character(j))
  codename <- mycolnames[j*2+1]
  datename <- mycolnames[j*2]
  code = as.integer(gsub("X","",codename))
  ExDate = ymd(as.Date(as.character(temp[,j*2]),format="%m/%d/%Y"))
  
  
  #########################################################################################   
    i = 1L
    for(i in 1L: length(ExDate)){
    
    oneYearBefore <- ymd(ExDate[i] - 345)
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    
    a <- curMC[,codename]
    b <- as.Date(as.character(curMC[,datename]),format="%m/%d/%Y")
      
  
    nalist<- which(is.na(a)==TRUE)
    if(length(nalist)>0){
      a <- a[-nalist]
      b <- b[-nalist]
    
    c <- which(b <= ExDate[i]) # some stock has no value but have value on previous day
    
    if(length(c)>0 ) {
      
      temp[i,j*2+1] = 1/a[c[length(c)]]
    }

    
      
    }
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp1BooktoMarket.csv")


####################################### 2nd file ################################################# 

#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp2.csv",header = T)

curMC = read.csv("BkAdjMktBV_NAdj2Dd.csv",header = T)
#curMC2 = read.csv("BkAdjMktBV_NAdj2Dd.csv",header = T)

last = (ncol(temp)+1)/2 - 1
mycolnames <- colnames(temp)
j = 1L
for(j in 1L:last){
  print(as.character(j))
  codename <- mycolnames[j*2+1]
  datename <- mycolnames[j*2]
  code = as.integer(gsub("X","",codename))
  ExDate = ymd(as.Date(as.character(temp[,j*2]),format="%m/%d/%Y"))
  
  
  #########################################################################################   
  for(i in 1L: length(ExDate)){
    
    oneYearBefore <- ymd(ExDate[i] - 345)
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    
    a <- curMC[,codename]
    b <- as.Date(as.character(curMC[,datename]),format="%m/%d/%Y")
    
    
    nalist<- which(is.na(a)==TRUE)
    if(length(nalist)>0){
      a <- a[-nalist]
      b <- b[-nalist]
      
      c <- which(b <= ExDate[i]) # some stock has no value but have value on previous day
      
      if(length(c)>0 ) {
        
        temp[i,j*2+1] = 1/a[c[length(c)]]
      }
      
      
      
    }
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp2BooktoMarket.csv")

