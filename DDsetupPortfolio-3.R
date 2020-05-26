library(lubridate)
library(beepr)
library(MatchIt)
library(optmatch)
library(kableExtra)
library(compare)
library(compareDF)

pd= read.csv("pdata3Clean.csv",header= T)

pdataF4 <- pd

pdataF4$Date <- as.Date(as.character(pdataF4$Date))

d <- which(pdataF4$RSS == 1)
pdataFrss <- pdataF4[d,]
RSSdate <- as.Date(as.character(pdataFrss$Date))
RSSdate <- unique(RSSdate)
#m.out$nn
#summary(m.out)

#plot(m.out, type = "jitter", interactive = FALSE)
#plot(m.out, type = "hist")
pdataF5<-pdataF4[,c("Code","Date","RSS","SplitRatio",
                    "Return24months","MarketCapM","mktReturn","pm11Return", "Book2Market" ,
                    "frisk",  "prisk","fspread" , "pspread" ,"fProfit","pProfit" ,"fROA","pROA" ,
                    "fLev","pLev","filliquid", "pilliquid", "fTurnover", "pTurnover","fTobinQ","pTobinQ","BQprice"
                    )]


######################
#checking missing value
for (j in 1L: ncol(pdataF5)){
  a<- which(is.na(pdataF5[,j])==TRUE)
  if(length(a)>0) print(as.character(j))
}


#########################
#pdataF5<-pdataF4

j = 1L
mAll <-NULL



pdataF5$sizeQ<-NA
pdataF5$BookMQ<-NA
pdataF5$ReturnP11mQ<-NA
pdataF5$PortfolioIndex<- NA
#split portfolio
for(j in 1L: length(RSSdate)){
  
  print(as.character(j))
  e <- which((pdataF5$Date %in% RSSdate[j])==TRUE)
  
  pdataF5[e,]$sizeQ <- with(pdataF5[e,], cut(MarketCapM, 
                                  breaks=quantile(MarketCapM, probs=seq(0,1, by=0.2), na.rm=TRUE), 
                                  include.lowest=TRUE))
  for(k in 1L: 5){
    f<- which((pdataF5$sizeQ==k)==TRUE)
    pdataF5[f,]$BookMQ <- with(pdataF5[f,], cut(Book2Market, 
                                                breaks=quantile(Book2Market, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                                include.lowest=TRUE))
    for(m in 1L:5){
      g<- which((pdataF5[f,]$BookMQ==m)==TRUE)
      pdataF5[f,][g,]$ReturnP11mQ <- with(pdataF5[f,][g,], cut(pm11Return, 
                                                  breaks=quantile(pm11Return, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                                  include.lowest=TRUE))
      
    }
  }
  #all set
  #pdataF6<-pdataF5[e,]
  a<-which((pdataF5[e,]$RSS==1)==TRUE)
  for(m in 1L:length(a)){
    mySize=pdataF5[e,][a[m],"sizeQ"]
    myBookM=pdataF5[e,][a[m],"BookMQ"]
    myReturnP11m=pdataF5[e,][a[m],"ReturnP11mQ"]
    b<-which(pdataF5[e,]$sizeQ==mySize)
    c<-which(pdataF5[e,][b,]$BookMQ==myBookM)
    d<-which(pdataF5[e,][b,][c,]$ReturnP11mQ==myReturnP11m)
    pdataF5[e,][b,][c,][d,"PortfolioIndex"]=paste(as.character(pdataF5[e,][a[m],"Code"]),as.character(RSSdate[j]),sep=":")
    
  }
}

write.csv(pdataF5,"pdataF5.csv")

myPorfolio<-pdataF5$PortfolioIndex
a<-which(is.na(myPorfolio)==FALSE)
myPorfolio<-myPorfolio[a]
myPorfolio<-unique(myPorfolio)
j = 1L
mAll <-NULL
#calculate perpensity score

for(j in 1L: length(myPorfolio)){
  print(as.character(j))
  #clear 1% and 99% of all values
  
  
  
  
  #####################################
  
  if(j !=70 && j!=173){
    e <- which((pdataF5$PortfolioIndex ==myPorfolio[j])==TRUE)
    
    pdataFmatching <- pdataF5[e,]
      
      
      #####################################
    
    #pdataFmatching<-pdataF5
    # attach(pdataFmatching)
   #ps<-glm(RSS ~ pm11Return + Book2Market + MarketCapM + 
   #          prisk + pspread +pProfit +pROA +pLev+pilliquid+
  #           pTurnover+mktReturn,data=pdataFmatching,family=binomial,
  #         control = list(maxit = 100))
   #summary(ps)
    #pdataFmatching$psvalue<-predict(ps, type="response")
    m.out = matchit(RSS ~ pm11Return + Book2Market + MarketCapM + BQprice +
                      prisk + pspread +pProfit +pROA +pLev+pilliquid+
                      pTurnover+mktReturn,
                    data = pdataFmatching, method = "nearest",
                    ratio = 5)
    m1 <-m.out$match.matrix
    m<-match.data(m.out)
    
    m$treated <- NA
    m$treatedD <- NA
    m$treatedID <- NA
    l=1L
    for(l in 1L : nrow(m1)){
      a <- m1[l,]
      
      myNA<- which(is.na(a)==TRUE)
      if(length(myNA)==0){
        m[a,"treated"]= as.character(m[rownames(m1)[l],"Code"])
        m[rownames(m1)[l],"treated"]= as.character(m[rownames(m1)[l],"Code"])
        
        m[a,"treatedD"]= myPorfolio[j]
        m[rownames(m1)[l],"treatedD"]= myPorfolio[j]
        
        m[a,"treatedID"]= paste(as.character(m[rownames(m1)[l],"Code"]),
                                myPorfolio[j],sep=":")
        m[rownames(m1)[l],"treatedID"]= paste(as.character(m[rownames(m1)[l],"Code"]),
                                              myPorfolio[j],sep=":")
        
      }
      
      
    }
    
    if(is.null(mAll)== TRUE){
      mAll <- m
    }else {
      mAll <- rbind(mAll,m)
    }
    
  }
}






a<-which(is.na(mAll$treatedID)==TRUE)
if(length(a)>0)
  mAll<-mAll[-a,]
############################################################################################################################
rownames(pdataF4)<-paste(pdataF5$Code,as.character(pdataF5$Date),sep=":")
rownames(mAll)<-paste(mAll$Code,as.character(mAll$Date),sep=":")
myrownames <- rownames(mAll)


a<-c("m12Return","PIHQ0",
     "L1L1","L1H1","H1H1","PrePrice","TargetPrice","NIIQ0","NIIQp1","NIIQp2",
     "NIIQp3","NIIQp4","NIIQn5","NIIQn6","NIIQn7","NIIQn8",
"PIHQn5",
"PIHQn6",
"PIHQn7",
"PIHQn8",
"PIHQp1",
"PIHQp2",
"PIHQp3",
"PIHQp4",
"NISQ0",
"NISQn5",
"NISQn6",
"NISQn7",
"NISQn8",
"NISQp1",
"NISQp2",
"NISQp3",
"NISQp4",
"PSHQ0",
"PSHQn5",
"PSHQn6",
"PSHQn7",
"PSHQn8",
"PSHQp1",
"PSHQp2",
"PSHQp3",
"PSHQp4",
"NAZQ0",
"NAZQn5",
"NAZQn6",
"NAZQn7",
"NAZQn8",
"NAZQp1",
"NAZQp2",
"NAZQp3",
"NAZQp4"
)
mAll[myrownames,a]<-pdataF4[myrownames,a]

write.csv(mAll,"mAll.csv")









################### don't execute - no more clean up required #############################################
summary(mAll)
summary(pdataF5)
#ctable=compare_df(mAll,pdataF5,c("Code","Date"),exclude=c("treated","treatedD","treatedID"))
mAll$rowindex<-paste(as.character(mAll$Code),as.character(mAll$Date),sep=":")
duplicatelist<-which(duplicated(mAll$rowindex)==TRUE)

if(length(duplicatelist)>0) mAll2<- mAll[-duplicatelist,] else
  mAll2<-mAll
write.csv(mAll2,"mAllClean.csv")

rownames(mAll2)<-c(mAll2$rowindex)
pdataF5$rowindex<-paste(as.character(pdataF5$Code),as.character(pdataF5$Date),sep=":")

rownames(pdataF5)<-c(paste(as.character(pdataF5$Code),as.character(pdataF5$Date),sep=":"))
mAll2$treated<-NULL
mAll2$treatedD<-NULL
mAll2$treatedID<-NULL
mAll2$weights<-NULL
mAll2$distance<-NULL
#for(i in 1L: nrow(mAll2)){
  
#  if(compare(mAll2[i,],pdataF5[mAll2[i,"rowindex"],])$result==FALSE)
#    print(as.character(i))
#}
a<-which(is.na(mAll$PIHQ0)==TRUE)
if(length(a)>0) mAll[a,"PIHQ0"]=0
a<-which(is.na(mAll$PIHQn5)==TRUE)
if(length(a)>0) mAll[a,"PIHQn5"]=0
a<-which(is.na(mAll$PIHQn6)==TRUE)
if(length(a)>0) mAll[a,"PIHQn6"]=0
a<-which(is.na(mAll$PIHQn7)==TRUE)
if(length(a)>0) mAll[a,"PIHQn7"]=0
a<-which(is.na(mAll$PIHQn8)==TRUE)
if(length(a)>0) mAll[a,"PIHQn8"]=0
a<-which(is.na(mAll$PIHQp1)==TRUE)
if(length(a)>0) mAll[a,"PIHQp1"]=0
a<-which(is.na(mAll$PIHQp2)==TRUE)
if(length(a)>0) mAll[a,"PIHQp2"]=0
a<-which(is.na(mAll$PIHQp3)==TRUE)
if(length(a)>0) mAll[a,"PIHQp3"]=0
a<-which(is.na(mAll$PIHQp4)==TRUE)
if(length(a)>0) mAll[a,"PIHQp4"]=0
a<-which(is.na(mAll$NISQ0)==TRUE)
if(length(a)>0) mAll[a,"NISQ0"]=0
a<-which(is.na(mAll$NISQn5)==TRUE)
if(length(a)>0) mAll[a,"NISQn5"]=0
a<-which(is.na(mAll$NISQn6)==TRUE)
if(length(a)>0) mAll[a,"NISQn6"]=0
a<-which(is.na(mAll$NISQn7)==TRUE)
if(length(a)>0) mAll[a,"NISQn7"]=0
a<-which(is.na(mAll$NISQn8)==TRUE)
if(length(a)>0) mAll[a,"NISQn8"]=0
a<-which(is.na(mAll$NISQp1)==TRUE)
if(length(a)>0) mAll[a,"NISQp1"]=0
a<-which(is.na(mAll$NISQp2)==TRUE)
if(length(a)>0) mAll[a,"NISQp2"]=0
a<-which(is.na(mAll$NISQp3)==TRUE)
if(length(a)>0) mAll[a,"NISQp3"]=0
a<-which(is.na(mAll$NISQp4)==TRUE)
if(length(a)>0) mAll[a,"NISQp4"]=0
a<-which(is.na(mAll$PSHQ0)==TRUE)
if(length(a)>0) mAll[a,"PSHQ0"]=0
a<-which(is.na(mAll$PSHQn5)==TRUE)
if(length(a)>0) mAll[a,"PSHQn5"]=0
a<-which(is.na(mAll$PSHQn6)==TRUE)
if(length(a)>0) mAll[a,"PSHQn6"]=0
a<-which(is.na(mAll$PSHQn7)==TRUE)
if(length(a)>0) mAll[a,"PSHQn7"]=0
a<-which(is.na(mAll$PSHQn8)==TRUE)
if(length(a)>0) mAll[a,"PSHQn8"]=0
a<-which(is.na(mAll$PSHQp1)==TRUE)
if(length(a)>0) mAll[a,"PSHQp1"]=0
a<-which(is.na(mAll$PSHQp2)==TRUE)
if(length(a)>0) mAll[a,"PSHQp2"]=0
a<-which(is.na(mAll$PSHQp3)==TRUE)
if(length(a)>0) mAll[a,"PSHQp3"]=0
a<-which(is.na(mAll$PSHQp4)==TRUE)
if(length(a)>0) mAll[a,"PSHQp4"]=0
a<-which(is.na(mAll$NAZQ0)==TRUE)
if(length(a)>0) mAll[a,"NAZQ0"]=0
a<-which(is.na(mAll$NAZQn5)==TRUE)
if(length(a)>0) mAll[a,"NAZQn5"]=0
a<-which(is.na(mAll$NAZQn6)==TRUE)
if(length(a)>0) mAll[a,"NAZQn6"]=0
a<-which(is.na(mAll$NAZQn7)==TRUE)
if(length(a)>0) mAll[a,"NAZQn7"]=0
a<-which(is.na(mAll$NAZQn8)==TRUE)
if(length(a)>0) mAll[a,"NAZQn8"]=0
a<-which(is.na(mAll$NAZQp1)==TRUE)
if(length(a)>0) mAll[a,"NAZQp1"]=0
a<-which(is.na(mAll$NAZQp2)==TRUE)
if(length(a)>0) mAll[a,"NAZQp2"]=0
a<-which(is.na(mAll$NAZQp3)==TRUE)
if(length(a)>0) mAll[a,"NAZQp3"]=0
a<-which(is.na(mAll$NAZQp4)==TRUE)
if(length(a)>0) mAll[a,"NAZQp4"]=0
