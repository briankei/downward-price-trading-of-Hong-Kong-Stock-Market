split <-read.csv("rSplitsFmktReturn11.csv", header = T)
pdata1 <-read.csv("pdataAllClean.csv", header = T)
pdata2  <-read.csv("pdataAllClean2.csv", header = T)

pdata3 <- rbind(pdata1,pdata2)
######################  fix scale per seed paper , change everything in percentage##
pdata3$fspread = pdata3$fspread *100
pdata3$pspread = pdata3$pspread *100
pdata3$f2spread = pdata3$f2spread *100
pdata3$p2spread = pdata3$p2spread *100

pdata3$fProfit = pdata3$fProfit *100
pdata3$pProfit = pdata3$pProfit *100

#pdata3$fTurnover = pdata3$fTurnover *100
#pdata3$pTurnover = pdata3$pTurnover *100
#pdata3$f2Turnover = pdata3$f2Turnover *100
#pdata3$p2Turover = pdata3$p2Turover *100

#pdata3$Return24months = (pdata3$Return24months - 1)*100
#pdata3$m11Return = (pdata3$m11Return - 1)*100


#pdata3$filliquid<- pdata3$filliquid
#pdata3$pilliquid<- pdata3$pilliquid
#pdata3$f2illiquid<- pdata3$f2illiquid
#pdata3$p2illiquid<- pdata3$p2illiquid


#pdata3$fLev<- pdata3$fLev*100
#pdata3$pLev<- pdata3$pLev*100

pdata3$fROA<- pdata3$fROA*100  
pdata3$pROA<- pdata3$pROA*100  
#change the data from R to r
pdata3$Return24months <- (pdata3$Return24months - 1)*100
pdata3$pm11Return <- (pdata3$pm11Return -1)*100
pdata3$mktReturn <- (pdata3$mktReturn -1)*100
pdata3$m12Return <- (pdata3$m12Return -1)*100
#########################################################################
pdata3$RSS <- 0
pdata3$L1L1 <- 0
pdata3$L1H1 <- 0
pdata3$H1H1 <- 0
pdata3$PrePrice<-0
pdata3$TargetPrice<-0
pdata3$SplitRatio <- 1
SplitCodeDate <- as.Date(as.character(split$ExDate),format = "%m/%d/%Y")
SplitCodeDate <- paste(as.character(split$Symbol),as.character(SplitCodeDate),sep=":")
SplitCodeDate <- paste("X",SplitCodeDate,sep="")
pDate <-as.character(pdata3$Date)
pDate <- as.Date(pDate)
pDate <- as.character(pDate)
pDate <- paste(pdata3$Code,pDate,sep=":")
j = 2L
for(j in 1L:nrow(split)){
  a<- which(pDate == SplitCodeDate[j])
  if(length(a)>0){
    pdata3[a,"PrePrice"]<-split[j,"PrePrice"]
    pdata3[a,"TargetPrice"]<-split[j,"TargetPrice"]
    
    if(split[j,"PrePrice"]<1 && split[j,"TargetPrice"]<1) pdata3[a,"L1L1"]=1
    if(split[j,"PrePrice"]<1 && split[j,"TargetPrice"]>=1) pdata3[a,"L1H1"]=1
    if(split[j,"PrePrice"]>=1 && split[j,"TargetPrice"]>=1) pdata3[a,"H1H1"]=1
    pdata3[a,"RSS"]<- 1
    pdata3[a,"SplitRatio"]= split[j,"ChangeRatio"]
  }
}
pdata3$X <- NULL
write.csv(pdata3,"pdata3.csv")

write.csv(pdata3,"pdata3Clean.csv")

