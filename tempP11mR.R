#Get  
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp1.csv",header = T)


monthreturn = read.csv("CUMULATIVE_TOT_RETURN_1Md.csv",header = T) 
#monthreturn2 = read.csv("CUMULATIVE_TOT_RETURN_2Md.csv",header = T)

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
    
    a <- monthreturn[,codename]
    b <- ymd(as.Date(as.character(monthreturn[,datename]),format="%m/%d/%Y"))
   
    c <- which(b <= oneYearBefore)  #monthly return
    if(length(c)>0){
      len = length(c)
      d <- a[c[len]:(c[len]+10)]
      
      d <- 1+d/100 #bloomberg return was in percentage. change it to 1+ratio
      
      temp[i,j*2+1] = d[1]*d[2]*d[3]*d[4]*d[5]*d[6]*d[7]*d[8]*d[9]*d[10]*d[11] #ratio
      
      
    }
 
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp1P11mR.csv")


####################################### 2nd file ################################################# 


#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp2.csv",header = T)


monthreturn = read.csv("CUMULATIVE_TOT_RETURN_2Md.csv",header = T) 
#monthreturn2 = read.csv("CUMULATIVE_TOT_RETURN_2Md.csv",header = T)

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
    
    a <- monthreturn[,codename]
    b <- ymd(as.Date(as.character(monthreturn[,datename]),format="%m/%d/%Y"))
    
    c <- which(b <= oneYearBefore)  #monthly return
    if(length(c)>0){
      len = length(c)
      d <- a[c[len]:(c[len]+10)]
      
      d <- 1+d/100 #bloomberg return was in percentage. change it to 1+ratio
      
      temp[i,j*2+1] = d[1]*d[2]*d[3]*d[4]*d[5]*d[6]*d[7]*d[8]*d[9]*d[10]*d[11] #ratio
      
      
    }
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp2P11mR.csv")


