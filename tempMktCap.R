#Get  
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp1.csv",header = T)

curMC = read.csv("CUR_MKT_CAP_1Dd.csv",header = T)
#curMC2 = read.csv("CUR_MKT_CAP_2Dd.csv",header = T)

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
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
     
    
    a <- curMC[,codename]
    b <- format(as.Date(as.character(curMC[,datename]),format="%m/%d/%Y"),format="%m/%y")
   
    c <- which(b == monthyr)
    if(length(c)>0 ) {
      d <- a[c]
      temp[i,j*2+1] = mean(d,na.rm=TRUE)
    }
    
     
  }#for ExDate loop
  
############################################################################################ 

  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp1MktCap.csv")

####################################### 2 f ##################################### 
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp2.csv",header = T)

curMC = read.csv("CUR_MKT_CAP_2Dd.csv",header = T)
#curMC2 = read.csv("CUR_MKT_CAP_2Dd.csv",header = T)

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
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    
    
    a <- curMC[,codename]
    b <- format(as.Date(as.character(curMC[,datename]),format="%m/%d/%Y"),format="%m/%y")
    
    c <- which(b == monthyr)
    if(length(c)>0 ) {
      d <- a[c]
      temp[i,j*2+1] = mean(d,na.rm=TRUE)
    }
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp2MktCap.csv")
