#setwd("d:/SharedFolder/myR/Bloomberg")
#setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/Bloomberg")
#install.packages("plm" )
library(kableExtra)
library(plm)
myData= read.csv("mAllDIDDeltaChinaC.csv",header = T)

myData$SplitRatio=1/myData$SplitRatio

myData$X.1<-NULL
myData$X<-NULL
myData$weights<-NULL
myData$distance<-NULL
#myData$NIIdelta<-myData$NIIQp4-myData$NIIQp1

myData$L5L5=0
myData$L5H5=0
myData$H5H5=0
myData$L4L4=0
myData$L4H4=0
myData$H4H4=0
myData$L6L6=0
myData$L6H6=0
myData$H6H6=0
myData$L1L1=0
myData$L1H1=0
myData$H1H1=0
myData$L3L3=0
myData$L3H3=0
myData$H3H3=0

j=1
RSSlist<-which(myData$RSS==1)
for(j in 1L:length(RSSlist)){
  
  if(myData[RSSlist[j],"PrePrice"]<0.3 && myData[RSSlist[j],"TargetPrice"]<0.3) myData[RSSlist[j],"L3L3"]=1
  if(myData[RSSlist[j],"PrePrice"]<0.3 && myData[RSSlist[j],"TargetPrice"]>=0.3) myData[RSSlist[j],"L3H3"]=1
  if(myData[RSSlist[j],"PrePrice"]>=0.3 && myData[RSSlist[j],"TargetPrice"]>=0.3) myData[RSSlist[j],"H3H3"]=1
  
  
  
}
for(j in 1L:length(RSSlist)){
  
    if(myData[RSSlist[j],"PrePrice"]<0.6 && myData[RSSlist[j],"TargetPrice"]<0.6) myData[RSSlist[j],"L6L6"]=1
    if(myData[RSSlist[j],"PrePrice"]<0.6 && myData[RSSlist[j],"TargetPrice"]>=0.6) myData[RSSlist[j],"L6H6"]=1
    if(myData[RSSlist[j],"PrePrice"]>=0.6 && myData[RSSlist[j],"TargetPrice"]>=0.6) myData[RSSlist[j],"H6H6"]=1
    
 
  
}
for(j in 1L:length(RSSlist)){

    if(myData[RSSlist[j],"PrePrice"]<0.4 && myData[RSSlist[j],"TargetPrice"]<0.4) myData[RSSlist[j],"L4L4"]=1
    if(myData[RSSlist[j],"PrePrice"]<0.4 && myData[RSSlist[j],"TargetPrice"]>=0.4) myData[RSSlist[j],"L4H4"]=1
    if(myData[RSSlist[j],"PrePrice"]>=0.4 && myData[RSSlist[j],"TargetPrice"]>=0.4) myData[RSSlist[j],"H4H4"]=1
    

  
}

for(j in 1L:length(RSSlist)){

    if(myData[RSSlist[j],"PrePrice"]<0.5 && myData[RSSlist[j],"TargetPrice"]<0.5) myData[RSSlist[j],"L5L5"]=1
    if(myData[RSSlist[j],"PrePrice"]<0.5 && myData[RSSlist[j],"TargetPrice"]>=0.5) myData[RSSlist[j],"L5H5"]=1
    if(myData[RSSlist[j],"PrePrice"]>=0.5 && myData[RSSlist[j],"TargetPrice"]>=0.5) myData[RSSlist[j],"H5H5"]=1
    

  
}

for(j in 1L:length(RSSlist)){

    if(myData[RSSlist[j],"PrePrice"]<1.0 && myData[RSSlist[j],"TargetPrice"]<1.0) myData[RSSlist[j],"L1L1"]=1
    if(myData[RSSlist[j],"PrePrice"]<1.0 && myData[RSSlist[j],"TargetPrice"]>=1.0) myData[RSSlist[j],"L1H1"]=1
    if(myData[RSSlist[j],"PrePrice"]>=1.0 && myData[RSSlist[j],"TargetPrice"]>=1.0) myData[RSSlist[j],"H1H1"]=1
    

  
}
#
####################################################################
#Bloomberg Help desk said they might have  double counting on the 

if(length(a<-which(myData$PIHQ0>100))>0) myData[a,"PIHQ0"]=99
if(length(a<-which(myData$PIHQn5>100))>0) myData[a,"PIHQn5"]=99
if(length(a<-which(myData$PIHQn6>100))>0) myData[a,"PIHQn6"]=99
if(length(a<-which(myData$PIHQn7>100))>0) myData[a,"PIHQn7"]=99
if(length(a<-which(myData$PIHQn8>100))>0) myData[a,"PIHQn8"]=99
if(length(a<-which(myData$PIHQp1>100))>0) myData[a,"PIHQp1"]=99
if(length(a<-which(myData$PIHQp2>100))>0) myData[a,"PIHQp2"]=99
if(length(a<-which(myData$PIHQp3>100))>0) myData[a,"PIHQp3"]=99
if(length(a<-which(myData$PIHQp4>100))>0) myData[a,"PIHQp4"]=99

if(length(a<-which(myData$PSHQ0>100))>0) myData[a,"PSHQ0"]=99
if(length(a<-which(myData$PSHQn5>100))>0) myData[a,"PSHQn5"]=99
if(length(a<-which(myData$PSHQn6>100))>0) myData[a,"PSHQn6"]=99
if(length(a<-which(myData$PSHQn7>100))>0) myData[a,"PSHQn7"]=99
if(length(a<-which(myData$PSHQn8>100))>0) myData[a,"PSHQn8"]=99
if(length(a<-which(myData$PSHQp1>100))>0) myData[a,"PSHQp1"]=99
if(length(a<-which(myData$PSHQp2>100))>0) myData[a,"PSHQp2"]=99
if(length(a<-which(myData$PSHQp3>100))>0) myData[a,"PSHQp3"]=99
if(length(a<-which(myData$PSHQp4>100))>0) myData[a,"PSHQp4"]=99



##################################################################
myData$Date <- as.character(myData$Date)
myData$Date <- as.Date(myData$Date)

PmyData <-pdata.frame(myData,index=c("Code","Date"))
################# get info ##########################
#number of records
nrow(myData)

#numeber of unique reverse stock splits dates
length(unique(myData$Date))

#unique stock code
length(unique(myData$Code))


#####################################
myData$Code <- as.character(myData$Code)
uniqueStockCode <- unique(myData$Code)
uniqueStockCodeRSS = rep(0,length(uniqueStockCode))
uniqueStockCodeControl = rep(0, length(uniqueStockCode))
for(i in 1L:length(uniqueStockCode)){
  a<-which(myData$Code == uniqueStockCode[i])
  if(length(a)>0){
    b<-which(myData[a,"RSS"]== 1)
    if(length(b)>0){
      uniqueStockCodeRSS[i] = length(b)
      uniqueStockCodeControl[i] = length(a) - length(b)
    }else {
      uniqueStockCodeControl[i] = length(a)
    }
  }
  
}
myStock<-as.data.frame(NULL)
myStock <- cbind(uniqueStockCode,uniqueStockCodeControl)
myStock <- cbind(myStock,uniqueStockCodeRSS)

write.csv(myStock,"myStock.csv")
totalRepeat = uniqueStockCodeControl+uniqueStockCodeRSS
repeat1 <- which(totalRepeat == 1)
repeat2 <- which(totalRepeat == 2)
repeat3 <- which(totalRepeat == 3)
repeat4 <- which(totalRepeat == 4)
repeat5 <- which(totalRepeat == 5)
repeat6 <- which(totalRepeat == 6)
repeat7 <- which(totalRepeat == 7)
repeat8 <- which(totalRepeat == 8)
repeat9 <- which(totalRepeat == 9)
repeat10 <- which(totalRepeat == 10)
repeat11 <- which(totalRepeat == 11)
repeat12 <- which(totalRepeat == 12)

totalcount = length(repeat1)+ length(repeat2)+ length(repeat3)+ length(repeat4)+ 
  length(repeat5)+ length(repeat6)+ length(repeat7)+ length(repeat8)+ 
  length(repeat9)+ length(repeat10)+ length(repeat11)+ length(repeat12)


################# get RSS info ##########################
#Reverse Stock Splits only
a<-which(myData$RSS==1)
myDataRSS<- myData[a,]      #RSS only
myDataControl<-myData[-a,]   #control group only

#number of Reverse Stock Splits and each one has a control group of 5 stocks
nrow(myDataRSS)

#RSS stocks is L1L1
length(b<-which(myDataRSS$L1L1==1))

#RSS stocks is L1H1
length(b<-which(myDataRSS$L1H1==1))

#RSS stocks is H1H1 (1 dollar)
length(b<-which(myDataRSS$H1H1==1))

#RSS stocks is L5L5 (50 cents)
length(b<-which(myDataRSS$L5L5==1))

#RSS stocks is L5H5
length(b<-which(myDataRSS$L5H5==1))

#RSS stocks is H5H5
length(b<-which(myDataRSS$H5H5==1))

#RSS split Ratio
SplitRatio=1/myDataRSS$SplitRatio
summary(SplitRatio)
sd(SplitRatio)
quantile(SplitRatio)
############################################ get unique RSS code info

duplicatedCodelist<-which(duplicated(myDataRSS$Code)==TRUE)
myUniqDataRSS<-myDataRSS[-duplicatedCodelist,]


#number of unique RSS stock code
nrow(myUniqDataRSS)
#number of split after HK-SZ connect launch
length(b<-which(myUniqDataRSS$SZC==1))
#number of split after HK-SH connect launch
length(b<-which(myUniqDataRSS$SHC==1))
#number of split after corporat goverance effective
length(b<-which(myUniqDataRSS$BDPC==1))

#RSS stocks is qualified SZ HK connect
length(b<-which(myUniqDataRSS$szcStk==1))
#RSS stocks is qualified SH HK connect
length(b<-which(myUniqDataRSS$shcStk==1))

#RSS stocks is GEM board
length(b<-which(myUniqDataRSS$GEM==1))

#RSS stocks is MAIN board
length(b<-which(myUniqDataRSS$GEM==0))



############################### descrptive statistics ##################
#output summary

#Control group only myDataControl
myDataControl2<-myDataControl[,-c(1,2,28:36)]
mySummary<-(summary(myDataControl2))

mySummary[1,]<- gsub("Min.   :","",mySummary[1,])
mySummary[2,]<- gsub("1st Qu.:","",mySummary[2,])
mySummary[3,]<- gsub("Median :","",mySummary[3,])
mySummary[4,]<- gsub("Mean   :","",mySummary[4,])
mySummary[5,]<- gsub("3rd Qu.:","",mySummary[5,])
mySummary[6,]<- gsub("Max.   :","",mySummary[6,])


mySD<-lapply(myDataControl2,sd)

#colnames(mySD)<-c("Standard Deviation")
mySummary<-rbind(mySummary,mySD)

myQuantile<-lapply(myDataControl2,quantile,na.rm=T)
mySummary<-rbind(mySummary,myQuantile)
mySummary<-t(mySummary)
#colnames(mySummary)<-c("Minimum","1stQ","Median","Mean","3rdQ","Maximum",)

write.csv(mySummary,"Control Summary.csv")

#RSS  group only myDataRSS
myDataRSS2<-myDataRSS[,-c(1,2,28:36)]
#myDataRSS2$SplitRatio=1/myDataRSS2$SplitRatio
mySummary<-(summary(myDataRSS2))

mySummary[1,]<- gsub("Min.   :","",mySummary[1,])
mySummary[2,]<- gsub("1st Qu.:","",mySummary[2,])
mySummary[3,]<- gsub("Median :","",mySummary[3,])
mySummary[4,]<- gsub("Mean   :","",mySummary[4,])
mySummary[5,]<- gsub("3rd Qu.:","",mySummary[5,])
mySummary[6,]<- gsub("Max.   :","",mySummary[6,])

mySD<-(lapply(myDataRSS2,sd))
mySummary<-rbind(mySummary,mySD)
myQuantile<-(lapply(myDataRSS2,quantile,na.rm=T))
mySummary<-rbind(mySummary,myQuantile)
mySummary<-t(mySummary)
#colnames(mySummary)<-c("Minimum","1stQ","Median","Mean","3rdQ","Maximum","Std Dev","0th","25th","50th","75th","100th")
write.csv(mySummary,"RSS Summary.csv")






###########################  24month returns with RSS only####################################################





###############################DV: 24 months return on  Difference value regression

pooling <- plm(D_Return24months ~ RSS+ # no result
                 RSS*X_turnover+
                 X_ROA+X_lev+X_profit+ X_tobinq+ X_risk+X_illiquid+X_turnover +X_spread+SplitRatio, 
               data=PmyData,  model="pooling")
summary(pooling)


pooling <- plm(D_Return24months ~ L5L5+L5L5*X_turnover+  # L5H5 has stronger reverse effect result *** Best result
                 L5H5+L5H5*X_turnover+
                 H5H5+H5H5*X_turnover+
                 X_ROA+X_lev+X_profit+ X_tobinq+ X_risk+X_illiquid+X_turnover +X_spread+SplitRatio, 
               data=PmyData,  model="pooling")
summary(pooling)

pooling <- plm(D_Return24months ~ RSS+ # no result
                 RSS*X_risk+
                 X_ROA+X_lev+X_profit+ X_tobinq+ X_risk+X_illiquid+X_turnover +X_spread+SplitRatio, 
               data=PmyData,  model="pooling")
summary(pooling)

pooling <- plm(D_Return24months ~ L5L5+L5L5*X_risk+  # L5H5 has stronger reverse effect result *** Best result
                 L5H5+L5H5*X_risk+
                 H5H5+H5H5*X_risk+
                 X_ROA+X_lev+X_profit+ X_tobinq+ X_risk+X_illiquid+X_turnover +X_spread+SplitRatio, 
               data=PmyData,  model="pooling")
summary(pooling)

#pooling <- plm(D_Return24months ~ L5L5+L5L5*D_pilliquid+  # has result not lining up with my story
#                 L5H5+L5H5*D_pilliquid+
#                 H5H5+H5H5*D_pilliquid+
#                 D_pROA+D_pLev+D_pProfit+ D_pTobinQ+ D_prisk+D_pilliquid+D_pTurnover +D_pspread+SplitRatio, 
#               data=PmyData,  model="pooling")
#summary(pooling)

#pooling <- plm(D_Return24months ~ L4L4+L4L4*D_pTurnover+  # has no result
#                 L4H4+L4H4*D_pTurnover+
#                 H4H4+H4H4*D_pTurnover+
#                 D_pROA+D_pLev+D_pProfit+ D_pTobinQ+ D_prisk+D_pilliquid+D_pTurnover +D_pspread+SplitRatio, 
#               data=PmyData,  model="pooling")
#summary(pooling)
#pooling <- plm(D_Return24months ~ L6L6+L6L6*D_pTurnover+  # has stronger effect than L5H5 but may due to unbalance data
#                 L6H6+L6H6*D_pTurnover+
#                 H6H6+H6H6*D_pTurnover+
#                 D_pROA+D_pLev+D_pProfit+ D_pTobinQ+ D_prisk+D_pilliquid+D_pTurnover +D_pspread+SplitRatio, 
#               data=PmyData,  model="pooling")
#summary(pooling)

#pooling <- plm(D_Return24months ~ RSS+ # no result
#                 D_pNII+D_pNII*RSS+
#                 D_pROA+D_pLev+D_pProfit+ D_pTobinQ+ D_prisk+D_pilliquid+D_pTurnover +D_pspread+SplitRatio, 
#               data=PmyData,  model="pooling")
#summary(pooling)
#pooling <- plm(D_Return24months ~ L5H5+L5L5+H5H5+ # no result
#                 D_pNII+D_pNII*L5H5+D_pPIH*L5L5+D_pNIS*H5H5+
#                 D_pROA+D_pLev+D_pProfit+ D_pTobinQ+ D_prisk+D_pilliquid+D_pTurnover +D_pspread+SplitRatio, 
#               data=PmyData,  model="pooling")
#summary(pooling)



###############################DV: 24 months return Difference in Differencevalue regression
##############################################  pooling ###########################
#pooling <- plm(D_Return24months ~ RSS+  #no result
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)
pooling <- plm(DD_Return24months ~ RSS+RSS*DD_turnover+  #has stronger reverse effect
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)
pooling <- plm(DD_Return24months ~ L5L5+L5L5*DD_turnover+  #has stronger reverse effect
                 L5L5+L5L5*DD_turnover+
                 L5H5+L5H5*DD_turnover+
                 H5H5+H5H5*DD_turnover+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)
#pooling <- plm(D_Return24months ~ RSS+RSS*DD_risk+  #has no resutl
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)
pooling <- plm(DD_Return24months ~ RSS+RSS*DD_risk+  #has stronger reverse effect
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)

pooling <- plm(DD_Return24months ~ L5L5+L5L5*DD_risk+  #has stronger reverse effect
                 L5L5+L5L5*DD_risk+
                 L5H5+L5H5*DD_risk+
                 H5H5+H5H5*DD_risk+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)



################# this records is not balanced in portfolio in missing info on NII
# need to clean up prior looking into NII effect
pooling <- plm(DD_Return24months ~ RSS+ DD_NII2Q+   #has stronger reverse effect
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")

summary(pooling)


######################################## NA in records  ##################################################
a<-which(is.na(myData$DD_NII2Q)==TRUE)
a1<-which(is.na(myData$DD_NIS2Q)==TRUE)
a2<-which(is.na(myData$DD_PIH2Q)==TRUE)
a3<-which(is.na(myData$DD_PSH2Q)== TRUE)
a4<-c(a,a1,a2,a2)
a5<-unique(a4)
a6<-as.character(myData[a5,"treatedID"])
a7<-unique(a6)
a8<-which((as.character(myData$treatedID) %in% a7)==TRUE)
myData2<- myData[-a8,]
#######################################################################
################# get info ##########################
#number of records
nrow(myData2)

#numeber of unique reverse stock splits dates
length(unique(myData2$Date))

#unique stock code
length(unique(myData2$Code))


################# get RSS info ##########################
#Reverse Stock Splits only
a<-which(myData2$RSS==1)
myData2RSS<- myData2[a,]      #RSS only
myData2Control<-myData2[-a,]   #control group only

#number of Reverse Stock Splits and each one has a control group of 5 stocks
nrow(myData2RSS)

#RSS stocks is L1L1
length(b<-which(myData2RSS$L1L1==1))

#RSS stocks is L1H1
length(b<-which(myData2RSS$L1H1==1))

#RSS stocks is H1H1 (1 dollar)
length(b<-which(myData2RSS$H1H1==1))

#RSS stocks is L5L5 (50 cents)
length(b<-which(myData2RSS$L5L5==1))

#RSS stocks is L5H5
length(b<-which(myData2RSS$L5H5==1))

#RSS stocks is H5H5
length(b<-which(myData2RSS$H5H5==1))

#RSS split Ratio
SplitRatio=myData2RSS$SplitRatio
summary(SplitRatio)
sd(SplitRatio)
quantile(SplitRatio)
############################################ get unique RSS code info

duplicatedCodelist<-which(duplicated(myData2RSS$Code)==TRUE)
myUniqDataRSS<-myData2RSS[-duplicatedCodelist,]


#number of unique RSS stock code
nrow(myUniqDataRSS)
#number of split after HK-SZ connect launch
length(b<-which(myUniqDataRSS$SZC==1))
#number of split after HK-SH connect launch
length(b<-which(myUniqDataRSS$SHC==1))
#number of split after corporat goverance effective
length(b<-which(myUniqDataRSS$BDPC==1))

#RSS stocks is qualified SZ HK connect
length(b<-which(myUniqDataRSS$szcStk==1))
#RSS stocks is qualified SH HK connect
length(b<-which(myUniqDataRSS$shcStk==1))

#RSS stocks is GEM board
length(b<-which(myUniqDataRSS$GEM==1))

#RSS stocks is MAIN board
length(b<-which(myUniqDataRSS$GEM==0))



############################### descrptive statistics ##################
#output summary

#Control group only myData2Control
myData2Control2<-myData2Control[,-c(1,2,28:36)]
mySummary<-(summary(myData2Control2))

mySummary[1,]<- gsub("Min.   :","",mySummary[1,])
mySummary[2,]<- gsub("1st Qu.:","",mySummary[2,])
mySummary[3,]<- gsub("Median :","",mySummary[3,])
mySummary[4,]<- gsub("Mean   :","",mySummary[4,])
mySummary[5,]<- gsub("3rd Qu.:","",mySummary[5,])
mySummary[6,]<- gsub("Max.   :","",mySummary[6,])


mySD<-lapply(myData2Control2,sd)

#colnames(mySD)<-c("Standard Deviation")
mySummary<-rbind(mySummary,mySD)

myQuantile<-lapply(myData2Control2,quantile,na.rm=T)
mySummary<-rbind(mySummary,myQuantile)
mySummary<-t(mySummary)
#colnames(mySummary)<-c("Minimum","1stQ","Median","Mean","3rdQ","Maximum",)

write.csv(mySummary,"Control NII2Q Summary.csv")

#RSS  group only myData2RSS
myData2RSS2<-myData2RSS[,-c(1,2,28:36)]
#myData2RSS2$SplitRatio=1/myData2RSS2$SplitRatio
mySummary<-(summary(myData2RSS2))

mySummary[1,]<- gsub("Min.   :","",mySummary[1,])
mySummary[2,]<- gsub("1st Qu.:","",mySummary[2,])
mySummary[3,]<- gsub("Median :","",mySummary[3,])
mySummary[4,]<- gsub("Mean   :","",mySummary[4,])
mySummary[5,]<- gsub("3rd Qu.:","",mySummary[5,])
mySummary[6,]<- gsub("Max.   :","",mySummary[6,])

mySD<-(lapply(myData2RSS2,sd))
mySummary<-rbind(mySummary,mySD)
myQuantile<-(lapply(myData2RSS2,quantile,na.rm=T))
mySummary<-rbind(mySummary,myQuantile)
mySummary<-t(mySummary)
#colnames(mySummary)<-c("Minimum","1stQ","Median","Mean","3rdQ","Maximum","Std Dev","0th","25th","50th","75th","100th")
write.csv(mySummary,"RSS NII2Q Summary.csv")






#######################################################################
PmyData <-pdata.frame(myData2,index=c("Code","Date"))
pooling <- plm(D_Return24months ~ RSS+ # no result
                 RSS*X_turnover+
                 X_ROA+X_lev+X_profit+ X_tobinq+ X_risk+X_illiquid+X_turnover +X_spread+SplitRatio, 
               data=PmyData,  model="pooling")
summary(pooling)

pooling <- plm(DD_Return24months ~ RSS+RSS*DD_turnover+  #has stronger reverse effect
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)


pooling <- plm(D_Return24months ~ RSS+ # no result
                 RSS*X_risk+
                 X_ROA+X_lev+X_profit+ X_tobinq+ X_risk+X_illiquid+X_turnover +X_spread+SplitRatio, 
               data=PmyData,  model="pooling")
summary(pooling)


pooling <- plm(DD_Return24months ~ RSS+RSS*DD_risk+  #has stronger reverse effect
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)


pooling <- plm(D_Return24months ~ RSS+X_LNII2Q+ RSS*X_LNII2Q+  #has stronger reverse effect
                 X_ROA+X_lev+X_profit+X_tobinq+X_risk+X_illiquid+X_turnover+ X_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)


pooling <- plm(DD_Return24months ~ RSS+ DD_LNII2Q+   #has stronger reverse effect
                 RSS*DD_LNII2Q+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")

summary(pooling)



pooling <- plm(D_Return24months ~ RSS+X_LPSH2Q+ RSS*X_LPSH2Q+  #has stronger reverse effect
                 X_ROA+X_lev+X_profit+X_tobinq+X_risk+X_illiquid+X_turnover+ X_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)


pooling <- plm(DD_Return24months ~ RSS+ DD_LPSH2Q+   #has stronger reverse effect
                 RSS*DD_LPSH2Q+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")

summary(pooling)
####################################  consol equation ##################################################
pooling <- plm(D_Return24months ~ RSS+X_LPSH2Q+ X_LNII2Q+ RSS*X_LNII2Q+  
                 RSS*X_turnover+RSS*X_risk+
                 X_ROA+X_lev+X_profit+X_tobinq+X_risk+X_illiquid+X_turnover+ X_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)


pooling <- plm(DD_Return24months ~ RSS+ DD_LPSH2Q+  
                 DD_LNII2Q+ RSS*DD_LNII2Q+
                 RSS*DD_turnover+RSS*DD_risk+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")

summary(pooling)
library(lmtest)

plmtest(pooling, c("time"),type="bp") #test for random effects. null is variances acrss entitis is zero, result show  time-fixed
plmtest(pooling, type="bp") #test random effects. null is variance across entities is zero. result show fail to reject null. no signifcant differences acrss stock, can run simple ols regression
coeftest(pooling, vcovHC)
bptest(pooling)  #testing for heteroskedasticity.  the null is homoskedasticity. p-value < 0.05 to reject null, result shows homoskedasticity
pbgtest(pooling)  #testing for serial correlation . The null is that there is not serial correlation. p-value < 0.05 to reject null, rsults show no correlation
pcdtest(pooling, test=c("lm"))  #Testing for cross-sectional dependence/contemporaneous correlation. the null is not correlated.

fixed <- plm(DD_Return24months ~ RSS+ DD_LPSH2Q+  
                 DD_LNII2Q+ RSS*DD_LNII2Q+
                 RSS*DD_turnover+RSS*DD_risk+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="within")

summary(fixed)

lm <- lm(DD_Return24months ~ RSS+ DD_LPSH2Q+  
               DD_LNII2Q+ RSS*DD_LNII2Q+
               RSS*DD_turnover+RSS*DD_risk+
               DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
             data=PmyData)

summary(lm)
lm <- lm(DD_Return24months ~ RSS+ DD_LPSH2Q+  
                  RSS*DD_LPSH2Q+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData)

summary(lm)

################################# L4L4 L6L6 #########################################################

pooling <- plm(D_Return24months ~ L4L4+L4L4*DD_LNIS+  #has stronger reverse effect
                 L4L4+L4L4*DD_LNIS+
                 L4H4+L4H4*DD_LNIS+
                 H4H4+H4H4*DD_LNIS+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)
pooling <- plm(D_Return24months ~ L6L6+L6L6*DD_LPSH+  #has stronger reverse effect
                 L6L6+L6L6*DD_LPSH+
                 L6H6+L6H6*DD_LPSH+
                 H6H6+H6H6*DD_LPSH+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
pooling <- plm(D_Return24months ~ L6L6+L6L6*DD_LPSH+  #has stronger reverse effect
                 L6L6+L6L6*DD_LPSH+
                 L6H6+L6H6*DD_LPSH+
                 H6H6+H6H6*DD_LPSH+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)


################################# no result ###########################################
#pooling <- plm(D_Return24months ~ RSS+RSS*DD_profit+  #has no result
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)
#pooling <- plm(D_Return24months ~ RSS+RSS*DD_spread+  #has no result on illiquid and spread
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)

#pooling <- plm(D_Return24months ~ L5L5+L5L5*DD_turnover+  #no resutl
#                 L5H5+L5H5*DD_turnover+
#                 H5H5+H5H5*DD_turnover+
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)
#pooling <- plm(D_Return24months ~ L5L5+L5L5*DD_lev+  #has strong reverse reslut
#                 L5H5+L5H5*DD_lev+
#                 H5H5+H5H5*DD_lev+
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)

#pooling <- plm(D_Return24months ~ RSS+ #slight reverse effect only
#                 DD_NIS+DD_NIS*RSS+
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)

#pooling <- plm(D_Return24months ~ RSS+ #slight reverse effect only
#                 DD_LNIS+DD_LNIS*RSS+
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)

#pooling <- plm(D_Return24months ~ RSS+ #strong enhancement effect to negative, others no result
#                 DD_LNII+DD_LNII*RSS+
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)



#pooling <- plm(D_Return24months ~ RSS+ #little revese result
#                 DD_LPSH+DD_LPSH*RSS+
#                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
#               data=PmyData,  model="pooling")
#summary(pooling)


################## 12 month return mostly no result#####################################
pooling <- plm(D_m12Return ~ RSS+  #no result
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)
pooling <- plm(D_m12Return ~ RSS+ RSS* DD_turnover #no result
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)
pooling <- plm(D_m12Return ~ RSS+ RSS* DD_risk #no result
               DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)

pooling <- plm(D_m12Return ~ L5L5+L5L5*DD_turnover+
                 L5H5+L5H5*DD_turnover+
                 H5H5+H5H5*DD_turnover+
                 DD_LNII+DD_NII*L5H5+
                 DD_LPIH+DD_PIH*L5H5+
                 DD_LNIS+DD_NIS*L5H5+
                 DD_LPSH+DD_PSH*L5H5+
                 DD_LNAZ+DD_NAZ*L5H5+
                 DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio,  
               data=PmyData,  model="pooling")
summary(pooling)


############# lm #################################################################
fit2 = lm(Return24months ~ L5L5+L5L5*X_turnover+
            L5H5+L5H5*X_turnover+
            H5H5+H5H5*X_turnover+
            X_NII+X_NII*X_turnover+
            X_PIH+X_PIH*X_turnover+
            X_NIS+X_NIS*X_turnover+
            X_PSH+X_PSH*X_turnover+
            X_NAZ+X_NAZ*X_turnover+
            X_ROA+X_lev+X_profit+X_tobinq+X_risk+X_illiquid+X_turnover+X_spread+SplitRatio, data=PmyData)
summary(fit2)

################difference in lm ##################################################

fit2 = lm(D_Return24months ~ L5L5+L5L5*D_pTurnover+
            L5H5+L5H5*D_pTurnover+
            H5H5+H5H5*D_pTurnover+
            D_pNII+D_pNII*D_pTurnover+
            D_pPIH+D_pPIH*D_pTurnover+
            D_pNIS+D_pNIS*D_pTurnover+
            D_pPSH+D_pPSH*D_pTurnover+
            D_pNAZ+D_pNAZ*D_pTurnover+
            D_pROA+D_pLev+D_pProfit+ D_pTobinQ+ D_prisk+D_pilliquid+D_pTurnover +D_pspread+SplitRatio, data=PmyData)
summary(fit2)

##################difference in difference in lm

fit2 = lm(D_Return24months ~ L5L5+L5L5*DD_turnover+
            L5H5+L5H5*DD_turnover+
            H5H5+H5H5*DD_turnover+
            DD_NII+DD_NII*DD_turnover+
            DD_PIH+DD_PIH*DD_turnover+
            DD_NIS+DD_NIS*DD_turnover+
            DD_PSH+DD_PSH*DD_turnover+
            DD_NAZ+DD_NAZ*DD_turnover+
            DD_ROA+DD_lev+DD_profit+DD_tobinq+DD_risk+DD_illiquid+DD_turnover+ DD_spread+SplitRatio, data=PmyData)
summary(fit2)


###-----------------         12 months return                ----------------
###############################DV: 12 months return on  Difference value regression #######

pooling <- plm(D_m12Return ~ SZC+BDPC+L5L5+L5L5*D_pTurnover+
                 L5H5+L5H5*D_pTurnover+H5H5+H5H5*D_pTurnover+
                 D_pROA+D_pLev+D_pProfit+
                 D_pTobinQ+D_prisk+D_pilliquid+D_pTurnover+D_pspread +
                 SplitRatio, data=PmyData,  model="pooling")
summary(pooling)



###############################DV: 12 months return Difference in Differencevalue regression


pooling <- plm(D_m12Return ~ SZC+BDPC+L5L5+L5L5*DD_turnover+
                 L5H5+L5H5*DD_turnover+H5H5+H5H5*DD_turnover+
                 DD_ROA+DD_lev+DD_profit+
                 DD_tobinq+DD_risk+DD_illiquid+DD_turnover+DD_spread +
                 SplitRatio, data=PmyData,  model="pooling")
summary(pooling)



###------------------fspread is lower for RSS stocks after Reverse Stock Splits----------######

fit2 = lm(fspread ~ SZC+BDPC+L5L5+L5L5*pTurnover+
            L5H5+L5H5*pTurnover+H5H5+H5H5*pTurnover+
            pROA+pLev+pProfit+pTurnover*pTurnover+                #test forced linear?
            pTobinQ+prisk+pilliquid+pTurnover+pspread +
            SplitRatio, data=PmyData)
summary(fit2)

pooling <- plm(fspread ~ SZC+BDPC+L5L5+L5L5*pTurnover+
                 L5H5+L5H5*pTurnover+H5H5+H5H5*pTurnover+
                 pROA+pLev+pProfit+
                 pTobinQ+prisk+pilliquid+pTurnover+pspread +
                 SplitRatio, data=PmyData,  model="between")
summary(pooling)


between4 <- plm(fspread ~ SZC+BDPC+L5L5+L5L5*pTurnover+
                  L5H5+L5H5*pTurnover+H5H5+H5H5*pTurnover+
                  pROA+pLev+pProfit+
                  pTobinQ+prisk+pilliquid+pTurnover+pspread +
                  SplitRatio, data=PmyData,  model="between")
summary(between4)


############################### Difference value regression
fit2 = lm(D_fspread ~ SZC+BDPC+L5L5+L5L5*D_pTurnover+
            L5H5+L5H5*D_pTurnover+H5H5+H5H5*D_pTurnover+
            D_pROA+D_pLev+D_pProfit+D_pTurnover*D_pTurnover+                #test forced linear?
            D_pTobinQ+D_prisk+D_pilliquid+D_pTurnover+
            SplitRatio, data=PmyData)
summary(fit2)
pooling <- plm(D_fspread ~ SZC+BDPC+L5L5+L5L5*D_pTurnover+
                 L5H5+L5H5*D_pTurnover+H5H5+H5H5*D_pTurnover+
                 D_pROA+D_pLev+D_pProfit+
                 D_pTobinQ+D_prisk+D_pilliquid+D_pTurnover+
                 SplitRatio, data=PmyData,  model="pooling")
summary(pooling)
between4 <- plm(D_fspread ~ SZC+BDPC+L5L5+L5L5*D_pTurnover+
                  L5H5+L5H5*D_pTurnover+H5H5+H5H5*D_pTurnover+
                  D_pROA+D_pLev+D_pProfit+
                  D_pTobinQ+D_prisk+D_pilliquid+D_pTurnover+
                  SplitRatio, data=PmyData,  model="between")
summary(between4)



############################### Difference in Differencevalue regression

fit2 = lm(DD_spread ~ SZC+BDPC+L5L5+L5L5*DD_turnover+
            L5H5+L5H5*DD_turnover+H5H5+H5H5*DD_turnover+
            DD_ROA+DD_lev+DD_profit+DD_turnover*DD_turnover+                #test forced linear?
            DD_tobinq+DD_risk+DD_illiquid+DD_turnover+
            SplitRatio, data=PmyData)
summary(fit2)



pooling <- plm(DD_spread ~ SZC+BDPC+L5L5+L5L5*DD_turnover+
                 L5H5+L5H5*DD_turnover+H5H5+H5H5*DD_turnover+
                 DD_ROA+DD_lev+DD_profit+
                 DD_tobinq+DD_risk+DD_illiquid+DD_turnover+
                 SplitRatio, data=PmyData,  model="pooling")
summary(pooling)


between4 <- plm(DD_spread ~ SZC+BDPC+L5L5+L5L5*DD_turnover+
                  L5H5+L5H5*DD_turnover+H5H5+H5H5*DD_turnover+
                  DD_ROA+DD_lev+DD_profit+
                  DD_tobinq+DD_risk+DD_illiquid+DD_turnover+
                  SplitRatio, data=PmyData,  model="between")
summary(between4)


###########################  12month returns with RSS only ####################################################


between12 <- plm(m12Return ~ SZC+BDPC+RSS+pROA+pLev+pProfit+
                 pTobinQ+prisk+pilliquid+pTurnover+pspread +
                 SplitRatio, data=PmyData,  model="between")
summary(between12)

between122 <- plm(m12Return ~ SZC+BDPC+RSS+RSS*pTurnover+pROA+pLev+pProfit+
                  pTobinQ+prisk+pilliquid+pTurnover+pspread +
                  SplitRatio, data=PmyData,  model="between")
summary(between122)
between123 <- plm(m12Return ~ SZC+BDPC+L1L1+L1L1*pTurnover+
                  L1H1+L1H1*pTurnover+H1H1+H1H1*pTurnover+
                  pROA+pLev+pProfit+
                  pTobinQ+prisk+pilliquid+pTurnover+pspread +
                  SplitRatio, data=PmyData,  model="between")
summary(between123)

between124 <- plm(m12Return ~ SZC+BDPC+L5L5+L5L5*pTurnover+
                  L5H5+L5H5*pTurnover+H5H5+H5H5*pTurnover+
                  pROA+pLev+pProfit+
                  pTobinQ+prisk+pilliquid+pTurnover+pspread +
                  SplitRatio, data=PmyData,  model="between")
summary(between124)

plmtest(between124)


fit2 = lm(m12Return ~ SZC+BDPC+L5L5+L5L5*pTurnover+
            L5H5+L5H5*pTurnover+H5H5+H5H5*pTurnover+
            pROA+pLev+pProfit+
            pTobinQ+prisk+pilliquid+pTurnover+pspread +
            SplitRatio, data=PmyData)
summary(fit2)


###########################  24month returns with L1L1 L1H1 HiHi only####################################################

pooling2 <- plm(Return24months ~ SZC+BDPC+L1L1+L1H1+H1H1+pROA+pLev+pProfit+
                 pTobinQ+prisk+pilliquid+pTurnover+pspread +
                 SplitRatio, data=PmyData, model="pooling")
summary(pooling2)

between2 <- plm(Return24months ~ SZC+BDPC+L1L1+L1H1+H1H1+pROA+pLev+pProfit+
                 pTobinQ+prisk+pilliquid+pTurnover+pspread +
                 SplitRatio, data=PmyData,  model="between")
summary(between2)



fixed2 <- plm(Return24months ~ SZC+BDPC+L1L1+L1H1+H1H1+pROA+pLev+pProfit+
               pTobinQ+prisk+pilliquid+pTurnover+pspread +
               SplitRatio, data=PmyData,  model="within")
summary(fixed2)
random2 <- plm(Return24months ~ SZC+BDPC+L1L1+L1H1+H1H1+pROA+pLev+pProfit+
                pTobinQ+prisk+pilliquid+pTurnover+pspread +
                SplitRatio, data=PmyData, model="random")
summary(random2)

plmtest(pooling2)



fit2 = lm(Return24months ~ SZC+BDPC+L1L1+L1H1+H1H1+pROA+pLev+pProfit+
           pTobinQ+prisk+pilliquid+pTurnover+pspread +
           SplitRatio, data=PmyData)
summary(fit2)
###########################  24month returns on pre and post change ####################################################

pooling <- plm(Return24months ~ SZC+SHC+BDPC+RSS+RSS*chinaC+GEM+Dprofit+Dturnover+Dlev+Dspread +
                 Dilliquid+DtobinQ+Drisk+Droa+SplitRatio, data=PmyData, model="pooling")
summary(pooling)

between <- plm(Return24months ~ SZC+SHC+BDPC+RSS+RSS*chinaC+GEM+Dprofit+Dturnover+Dlev+Dspread +
                 Dilliquid+DtobinQ+Drisk+Droa+SplitRatio, data=PmyData,  model="between")
summary(between)



fixed <- plm(Return24months ~ SZC+SHC+BDPC+RSS+RSS*chinaC+GEM+Dprofit+Dturnover+Dlev+Dspread +
               Dilliquid+DtobinQ+Drisk+Droa+SplitRatio, data=PmyData,  model="within")
summary(fixed)
random <- plm(Return24months ~ SZC+SHC+BDPC+RSS+RSS*chinaC+GEM+Dprofit+Dturnover+Dlev+Dspread +
                Dilliquid+DtobinQ+Drisk+Droa+SplitRatio, data=PmyData, model="random")
summary(random)

plmtest(pooling)



fit = lm(Return24months ~ SZC+SHC+BDPC+RSS+RSS*chinaC+GEM+Dprofit+Dturnover+Dlev+Dspread +
           Dilliquid+DtobinQ+Drisk+Droa+SplitRatio, data=PmyData)
summary(fit)


###########################  delta parameters ####################################################

between2 <- plm(Return24months ~ SZC+SHC+BDPC+RSS+GEM+DeltaProfit+Deltaturnover+DeltaLev+DeltaAvgSpread +
                  Deltailliquid+DeltatobinQ +Deltarisk+Deltaroa +SplitRatio, data=PmyData,  model="between")
summary(between2)


###########################  china stocks connect parameters ####################################################

between3 <- plm(Return24months ~ SZC+SHC+BDPC+RSS+GEM+RSS*shcStk+RSS*szcStk+RSS*chinaC+
                  pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData,  model="between")
summary(between3)

############################# previous spread ##############################
between4 <- plm(Return24months ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
                  pilliquid+pTobinQ+prisk+pROA+SplitRatio+preAvgSpread + RSS*preAvgSpread +
                 Fpilliquid+FptobinQ+Fprisk+FpRoa+SplitRatio, data=PmyData,  model="between")
summary(between4)

############################# previous 11m return ##############################
between5 <- plm(Return24months ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
                  pilliquid+pTobinQ+prisk+pROA+SplitRatio +P11mR + RSS*P11mR +
                  Fpilliquid+FptobinQ+Fprisk+FpRoa+SplitRatio, data=PmyData,  model="between")
summary(between5)

###########################  DDNII ####################################################
a<-which(is.na(myData$DDNII)==TRUE)
myData2<-myData[-a,]
PmyData <-pdata.frame(myData2,index=c("Code","Date"))


pooling <- plm(DDNII ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
                 pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData, model="pooling")
summary(pooling)

between <- plm(DDNII ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
                 pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData,  model="between")
summary(between)



fixed <- plm(DDNII ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
               pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData,  model="within")
summary(fixed)
random <- plm(DDNII ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
                pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData, model="random")
summary(random)

plmtest(pooling)



fit = lm(DDNII ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
           pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData)
summary(fit)




summary(myData)
###########################  12 month returns ####################################################

pooling <- plm(m12Return ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
                 pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData, model="pooling")
summary(pooling)

between <- plm(m12Return ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
                 pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData,  model="between")
summary(between)



fixed <- plm(m12Return ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
               pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData,  model="within")
summary(fixed)
random <- plm(m12Return ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
                pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData, model="random")
summary(random)

plmtest(pooling)



fit = lm(m12Return ~ SZC+SHC+BDPC+RSS+GEM+pProfit+pTurnover+pLev+pspread +
           pilliquid+pTobinQ+prisk+pROA+SplitRatio, data=PmyData)
summary(fit)

############################## old code ###################################
myData2<-myData[,-c(1,2)]
myData2<-myData[,-c(1,2,36:40)]
mySummary<-summary(myData2)

mySD<-lapply(myData2,sd)
#df <- data.frame(matrix(unlist(mySD), nrow=length(mySD), byrow=T),stringsAsFactors=FALSE)
#mySummary2<-cbind(mySummary,df)
myQuantile<-lapply(myData2,quantile)

write.csv(mySummary,"0mySummary.csv")

write.csv(myQuantile,"0myQuant.csv")

write.csv(mySD,"0mySD.csv")

myDataRSSlist<-which(myData2$RSS==1) 
myDataRSS<-myData2[myDataRSSlist,]
myDataRSS$SplitRatio<-1/myDataRSS$SplitRatio
summary(myDataRSS$SplitRatio)
quantile(myDataRSS$SplitRatio)
sd(myDataRSS$SplitRatio)

summary(myDataRSS$PrePrice)
quantile(myDataRSS$PrePrice)
sd(myDataRSS$PrePrice)

summary(myDataRSS$TargetPrice)
quantile(myDataRSS$TargetPrice)
sd(myDataRSS$TargetPrice)

library(dbplyr)
data<-myDataRSS$SplitRatio
dataname = "Split Ratio"
mainL1<-paste("Histogram of ",dataname,sep=": ")
mainL2<-paste("QQ plot of ",dataname,sep=": ")

hist(data,probability=T, main=mainL1,xlab="Approximately normally distributed data")
lines(density(data),col=2)

qqnorm(data,main=mainL2,pch=19)
qqline(data)


data<-myDataRSS$PrePrice
dataname = "Pre-Split Price"
mainL1<-paste("Histogram of ",dataname,sep=": ")
mainL2<-paste("QQ plot of ",dataname,sep=": ")

hist(data,probability=T, main=mainL1,xlab="Approximately normally distributed data")
lines(density(data),col=2)

qqnorm(data,main=mainL2,pch=19)
qqline(data)

data<-myData$Return24months
dataname = "2 Yrs Abnormal Return"
mainL1<-paste("Histogram of ",dataname,sep=": ")
mainL2<-paste("QQ plot of ",dataname,sep=": ")

hist(data,probability=T, main=mainL1,xlab="Approximately normally distributed data")
lines(density(data),col=2)

qqnorm(data,main=mainL2,pch=19)
qqline(data)

data<-myData$pTurnover
dataname = "1 year avg trunover prior split"
mainL1<-paste("Histogram of ",dataname,sep=": ")
mainL2<-paste("QQ plot of ",dataname,sep=": ")

hist(data,probability=T, main=mainL1,xlab="Approximately normally distributed data")
lines(density(data),col=2)

qqnorm(data,main=mainL2,pch=19)
qqline(data)

#RSS stocks only
data<-myDataRSS$Return24months
dataname = "2 Yrs Abnormal Return for Splits only"
mainL1<-paste("Histogram of ",dataname,sep=": ")
mainL2<-paste("QQ plot of ",dataname,sep=": ")

hist(data,probability=T, main=mainL1,xlab="Approximately normally distributed data")
lines(density(data),col=2)

qqnorm(data,main=mainL2,pch=19)
qqline(data)

data<-myDataRSS$pTurnover
dataname = "1 year avg trunover prior split for Splits only "
mainL1<-paste("Histogram of ",dataname,sep=": ")
mainL2<-paste("QQ plot of ",dataname,sep=": ")

hist(data,probability=T, main=mainL1,xlab="Approximately normally distributed data")
lines(density(data),col=2)

qqnorm(data,main=mainL2,pch=19)
qqline(data)





