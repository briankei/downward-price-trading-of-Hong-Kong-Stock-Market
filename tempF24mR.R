#Get  
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp1.csv",header = T)
adjRLog = read.csv("adjRLog1.csv",header = T)
#adjRLog2 = read.csv("adjRLog2.csv",header = T)
last = (ncol(temp)+1)/2 - 1
mycolnames <- colnames(temp)
j = 1L
for(j in 1L:last){
  print(as.character(j))
  codename <- mycolnames[j*2+1]
  datename <- mycolnames[j*2]
  code = as.integer(gsub("X","",codename))
  ExDate = ymd(as.Date(as.character(temp[,j*2]),format="%m/%d/%Y"))
  
  i = 1L
  #########################################################################################   
  for(i in 1L: length(ExDate)){
    Year2D <- ymd(ExDate[i] + 731)
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    
    a <- adjRLog[,codename]
    b <- ymd(as.Date(as.character(adjRLog[,datename]),format="%m/%d/%Y"))
    
    nalist<-which(is.na(a)==TRUE)
    if(length(nalist)>0){
      a<-a[-nalist]
      b<-b[-nalist]
    }
    c1 <- which(b >= Year2D )
    if(length(c1)>0) {
      c2 <- which(b > ExDate[i])
      if(length(c2)>0) {
        
        d<-a[(c2[1]):c1[1]]
        temp[i,j*2+1]= exp(sum(d))
        
      }
      
    }
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp1F24mR.csv")


####################################### 2nd file ################################################# 
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp2.csv",header = T)
adjRLog = read.csv("adjRLog2.csv",header = T)
#adjRLog2 = read.csv("adjRLog2.csv",header = T)
last = (ncol(temp)+1)/2 - 1
mycolnames <- colnames(temp)
j = 1L
for(j in 1L:last){
  print(as.character(j))
  codename <- mycolnames[j*2+1]
  datename <- mycolnames[j*2]
  code = as.integer(gsub("X","",codename))
  ExDate = ymd(as.Date(as.character(temp[,j*2]),format="%m/%d/%Y"))
  
  i = 1L
  #########################################################################################   
  for(i in 1L: length(ExDate)){
    Year2D <- ymd(ExDate[i] + 731)
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    
    a <- adjRLog[,codename]
    b <- ymd(as.Date(as.character(adjRLog[,datename]),format="%m/%d/%Y"))
    
    nalist<-which(is.na(a)==TRUE)
    if(length(nalist)>0){
      a<-a[-nalist]
      b<-b[-nalist]
    }
    c1 <- which(b >= Year2D )
    if(length(c1)>0) {
      c2 <- which(b > ExDate[i])
      if(length(c2)>0) {
        
        d<-a[(c2[1]):c1[1]]
        temp[i,j*2+1]= exp(sum(d))
        
      }
      
    }
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp2F24mR.csv")


