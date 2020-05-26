#Get  not run yet
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp1.csv",header = T)

ptemp <- temp
turnover = read.csv("turnover1.csv",header = T)

myday =365 # 1 year
j = 1L

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
    
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    
    a <- turnover[,codename]
    b <- as.Date(as.character(turnover[,datename]),format="%m/%d/%Y")
    
    nalist <- which(is.na(a)==TRUE)
    if(length(nalist)>0){
      a <- a[-nalist]
      b <- b[-nalist]
    }
    f366d = ExDate[i]+ myday
    p366d = ExDate[i]- myday
      
    c1 <- which(b > ExDate[i])
    if(length(c1)>0) {
      c2 <- which(b >= f366d)
      if(length(c2)>0) {
          
        d <- a[(c1[1]):(c2[1])]
        temp[i,j*2+1]= mean(d)
      }
    }    
    c1 <- which(b < ExDate[i])
    if(length(c1)>0) {
      c2 <- which(b >= p366d)
      if(length(c2)>0) {
        d<- a[(c2[1]):(c1[length(c1)])]
        ptemp[i,j*2+1]= mean(d)
            
      }
    }
   
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp1Fturnover.csv")
ptemp$X<-NULL
write.csv(ptemp,"temp1FpTurnover.csv")

###############################################  2nd file ###################################### 

temp= read.csv("temp2.csv",header = T)

ptemp <- temp
turnover = read.csv("turnover2.csv",header = T)

myday =365 # 1 year
j = 1L

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
    
    Year <- year(ExDate[i])
    monthyr <- format(ExDate[i],"%m/%y")
    
    a <- turnover[,codename]
    b <- as.Date(as.character(turnover[,datename]),format="%m/%d/%Y")
    
    nalist <- which(is.na(a)==TRUE)
    if(length(nalist)>0){
      a <- a[-nalist]
      b <- b[-nalist]
    }
    f366d = ExDate[i]+ myday
    p366d = ExDate[i]- myday
    c1 <- which(b > ExDate[i])
    if(length(c1)>0) {
      c2 <- which(b >= f366d)
      if(length(c2)>0) {
        
        d <- a[(c1[1]):(c2[1])]
        temp[i,j*2+1]= mean(d)
      }
    }    
    c1 <- which(b < ExDate[i])
    if(length(c1)>0) {
      c2 <- which(b >= p366d)
      if(length(c2)>0) {
        d<- a[(c2[1]):(c1[length(c1)])]
        ptemp[i,j*2+1]= mean(d)
        
      }
    }
    
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp2Fturnover.csv")
ptemp$X<-NULL
write.csv(ptemp,"temp2FpTurnover.csv")
