#Get  not run yet
setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")
library(lubridate)
#split Ex-date betwwen Jan 1st 2008 and Dec 31st 2017
temp= read.csv("temp1.csv",header = T)

ptemp <- temp
Roa = read.csv("BkROA_NAdj1S.csv",header = T)


myday =365 # 1 year
j = 1L

last = (ncol(temp)+1)/2 - 1
mycolnames <- colnames(temp)

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
    
    a <- Roa[,codename]
    b <- year(ymd(as.Date(as.character(Roa[,datename]),format="%m/%d/%Y")))
    
    f366d = year(ExDate[i])+ 1
    p366d = year(ExDate[i])- 1
    
    c <- which(b == p366d)
    if(length(c)>0) {
      temp[i,j*2+1]= a[c[length(c)]]
    } 
    
    c <- which(b == f366d)
    if(length(c)>0) {
      ptemp[i,j*2+1]= a[c[length(c)]]
    } 
    
    
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp1Froa.csv")
ptemp$X<-NULL
write.csv(ptemp,"temp1FpRoa.csv")

############################  2 f  #####################################
temp= read.csv("temp2.csv",header = T)

ptemp <- temp
Roa = read.csv("BkROA_NAdj2S.csv",header = T)


myday =365 # 1 year
j = 1L

last = (ncol(temp)+1)/2 - 1
mycolnames <- colnames(temp)

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
    
    a <- Roa[,codename]
    b <- year(ymd(as.Date(as.character(Roa[,datename]),format="%m/%d/%Y")))
    
    f366d = year(ExDate[i])+ 1
    p366d = year(ExDate[i])- 1
    
    c <- which(b == p366d)
    if(length(c)>0) {
      temp[i,j*2+1]= a[c[length(c)]]
    } 
    
    c <- which(b == f366d)
    if(length(c)>0) {
      ptemp[i,j*2+1]= a[c[length(c)]]
    } 
    
    
    
    
  }#for ExDate loop
  
  ############################################################################################ 
  
  
}#for column loop

temp$X<-NULL
write.csv(temp,"temp2Froa.csv")
ptemp$X<-NULL
write.csv(ptemp,"temp2FpRoa.csv")