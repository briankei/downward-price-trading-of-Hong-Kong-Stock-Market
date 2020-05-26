#Get  not run yet
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
tempProfit= read.csv("temp1.csv",header = T)
ptempProfit <- tempProfit
tempLev <- tempProfit
ptempLev <- tempProfit
tempTobinQ <- tempProfit
ptempTobinQ <- tempProfit


tobinQ = read.csv("tobinQ_1Yd.csv",header = T)
profit = read.csv("profitability1.csv",header = T)
leverage = read.csv("leverage1.csv",header = T)

myday =365 # 1 year
j = 1L

last = (ncol(tempProfit)+1)/2 - 1
mycolnames <- colnames(tempProfit)

for(j in 1L:last){
  print(as.character(j))
  codename <- mycolnames[j*2+1]
  datename <- mycolnames[j*2]
  code = as.integer(gsub("X","",codename))
  ExDate = ymd(as.Date(as.character(tempProfit[,j*2]),format="%m/%d/%Y"))
  
  i = 1L
  #########################################################################################   
  for(i in 1L: length(ExDate)){
    
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    f366d = year(ExDate[i])+ 1
    p366d = year(ExDate[i])- 1
    
    a <- profit[,codename]
    b <- year(ymd(as.Date(as.character(profit[,datename]))))
    
    c <- which(b == p366d)
    if(length(c)>0) {
      tempProfit[i,j*2+1]= a[c[length(c)]]
    } 
    
    c <- which(b == f366d)
    if(length(c)>0) {
      ptempProfit[i,j*2+1]= a[c[length(c)]]
    } 
    
    a <- leverage[,codename]
    b <- year(ymd(as.Date(as.character(leverage[,datename]),format="%m/%d/%Y")))
    
    c <- which(b == p366d)
    if(length(c)>0) {
      tempLev[i,j*2+1]= a[c[length(c)]]
    } 
    
    c <- which(b == f366d)
    if(length(c)>0) {
      ptempLev[i,j*2+1]= a[c[length(c)]]
    } 
    
    a <- tobinQ[,codename]
    b <- year(ymd(as.Date(as.character(tobinQ[,datename]),format="%m/%d/%Y")))
    c <- which(b == p366d)
    if(length(c)>0) {
      tempTobinQ[i,j*2+1]= a[c[length(c)]]
    } 
    
    c <- which(b == f366d)
    if(length(c)>0) {
      ptempTobinQ[i,j*2+1]= a[c[length(c)]]
    } 
    
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

tempProfit$X<-NULL
write.csv(tempProfit,"temp1FProfit.csv")
ptempProfit$X<-NULL
write.csv(ptempProfit,"temp1FpPRofit.csv")
tempLev$X<-NULL
write.csv(tempLev,"temp1FLev.csv")
ptempLev$X<-NULL
write.csv(ptempLev,"temp1FpLev.csv")
tempTobinQ$X<-NULL
write.csv(tempTobinQ,"temp1FtobinQ.csv")
ptempTobinQ$X<-NULL
write.csv(ptempTobinQ,"temp1FptobinQ.csv")

############################  2 f  #####################################
tempProfit= read.csv("temp2.csv",header = T)
ptempProfit <- tempProfit
tempLev <- tempProfit
ptempLev <- tempProfit
tempTobinQ <- tempProfit
ptempTobinQ <- tempProfit


tobinQ = read.csv("tobinQ_2Yd.csv",header = T)
profit = read.csv("profitability2.csv",header = T)
leverage = read.csv("leverage2.csv",header = T)

myday =365 # 1 year
j = 1L

last = (ncol(tempProfit)+1)/2 - 1
mycolnames <- colnames(tempProfit)

for(j in 1L:last){
  print(as.character(j))
  codename <- mycolnames[j*2+1]
  datename <- mycolnames[j*2]
  code = as.integer(gsub("X","",codename))
  ExDate = ymd(as.Date(as.character(tempProfit[,j*2]),format="%m/%d/%Y"))
  
  i = 1L
  #########################################################################################   
  for(i in 1L: length(ExDate)){
    
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    f366d = year(ExDate[i])+ 1
    p366d = year(ExDate[i])- 1
    
    a <- profit[,codename]
    b <- year(ymd(as.Date(as.character(profit[,datename]))))
    
    c <- which(b == p366d)
    if(length(c)>0) {
      tempProfit[i,j*2+1]= a[c[length(c)]]
    } 
    
    c <- which(b == f366d)
    if(length(c)>0) {
      ptempProfit[i,j*2+1]= a[c[length(c)]]
    } 
    
    a <- leverage[,codename]
    b <- year(ymd(as.Date(as.character(leverage[,datename]),format="%m/%d/%Y")))
    
    c <- which(b == p366d)
    if(length(c)>0) {
      tempLev[i,j*2+1]= a[c[length(c)]]
    } 
    
    c <- which(b == f366d)
    if(length(c)>0) {
      ptempLev[i,j*2+1]= a[c[length(c)]]
    } 
    
    a <- tobinQ[,codename]
    b <- year(ymd(as.Date(as.character(tobinQ[,datename]),format="%m/%d/%Y")))
    c <- which(b == p366d)
    if(length(c)>0) {
      tempTobinQ[i,j*2+1]= a[c[length(c)]]
    } 
    
    c <- which(b == f366d)
    if(length(c)>0) {
      ptempTobinQ[i,j*2+1]= a[c[length(c)]]
    } 
    
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

tempProfit$X<-NULL
write.csv(tempProfit,"temp2FProfit.csv")
ptempProfit$X<-NULL
write.csv(ptempProfit,"temp2FpPRofit.csv")
tempLev$X<-NULL
write.csv(tempLev,"temp2FLev.csv")
ptempLev$X<-NULL
write.csv(ptempLev,"temp2FpLev.csv")
tempTobinQ$X<-NULL
write.csv(tempTobinQ,"temp2FtobinQ.csv")
ptempTobinQ$X<-NULL
write.csv(ptempTobinQ,"temp2FptobinQ.csv")
