#setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/Bloomberg")
#setwd("d:/SharedFolder/myR/Bloomberg")
library(lubridate)
library(beepr)
library(ggplot2)

pd1= read.csv("DD_setQturnover-7.csv",header = T)
pd1$X<-NULL
pd1$Date<-as.character(pd1$Date)
pd1$Date<-ymd(as.Date(pd1$Date))
pd1$L5L5=0
pd1$L5H5=0
pd1$H5H5=0
j=1
RSSlist<-which(pd1$RSS==1)


for(j in 1L:length(RSSlist)){
  
  if(pd1[RSSlist[j],"PrePrice"]<0.5 && pd1[RSSlist[j],"TargetPrice"]<0.5) pd1[RSSlist[j],"L5L5"]=1
  if(pd1[RSSlist[j],"PrePrice"]<0.5 && pd1[RSSlist[j],"TargetPrice"]>=0.5) pd1[RSSlist[j],"L5H5"]=1
  if(pd1[RSSlist[j],"PrePrice"]>=0.5 && pd1[RSSlist[j],"TargetPrice"]>=0.5) pd1[RSSlist[j],"H5H5"]=1
  
  
  
}

################# get RSS info ##########################
#Reverse Stock Splits only

myRSS<- pd1[RSSlist,]      #RSS only
myCTL<-pd1[-RSSlist,]   #control group only
L5L5list<- which(myRSS$L5L5==1)
L5H5list<- which(myRSS$L5H5==1)
H5H5list<- which(myRSS$H5H5==1)
myL5L5<-myRSS[L5L5list,]
myL5H5<-myRSS[L5H5list,]
myH5H5<-myRSS[H5H5list,]
myCTL$RSStype<-NA
for(j in 1L: nrow(myL5L5)){
   myID= myL5L5[j,"treatedID"]
   a<-which(myCTL$treatedID==myID)
   myCTL[a,"RSStype"] = "L5L5"
}
for(j in 1L: nrow(myL5H5)){
  myID= myL5H5[j,"treatedID"]
  a<-which(myCTL$treatedID==myID)
  myCTL[a,"RSStype"] = "L5H5"
}
for(j in 1L: nrow(myH5H5)){
  myID= myL5L5[j,"treatedID"]
  a<-which(myCTL$treatedID==myID)
  myCTL[a,"RSStype"] = "H5H5"
}
a<-which(myCTL$RSStype == "L5L5")
myCTL_L5L5 <- myCTL[a,]
a<-which(myCTL$RSStype == "L5H5")
myCTL_L5H5 <- myCTL[a,]
a<-which(myCTL$RSStype == "H5H5")
myCTL_H5H5 <- myCTL[a,]

quarterID<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
dfRSS<- data.frame("quarterID" = matrix(unlist(quarterID), nrow=length(quarterID), byrow=T))
dfL5H5<-dfRSS
dfL5L5<-dfRSS
dfControl<-dfRSS

#################################Quarteryly Turnover

myControl <- myCTL
myRSStype <- myRSS


################################ RSS Turnover ################
control <- c(mean(myControl$TOQp8,na.rm=T),mean(myControl$TOQp7,na.rm=T),mean(myControl$TOQp6,na.rm=T),mean(myControl$TOQp5,na.rm=T),
            mean(myControl$TOQp4,na.rm=T),mean(myControl$TOQp3,na.rm=T),mean(myControl$TOQp2,na.rm=T),mean(myControl$TOQp1,na.rm=T),
            mean(myControl$TOQ0,na.rm=T),mean(myControl$TOQn1,na.rm=T),mean(myControl$TOQn2,na.rm=T),mean(myControl$TOQn3,na.rm=T),
            mean(myControl$TOQn4,na.rm=T),mean(myControl$TOQn5,na.rm=T),mean(myControl$TOQn6,na.rm=T),mean(myControl$TOQn7,na.rm=T),
            mean(myControl$TOQn8,na.rm=T))
rss <- c(mean(myRSStype$TOQp8,na.rm=T),mean(myRSStype$TOQp7,na.rm=T),mean(myRSStype$TOQp6,na.rm=T),mean(myRSStype$TOQp5,na.rm=T),
             mean(myRSStype$TOQp4,na.rm=T),mean(myRSStype$TOQp3,na.rm=T),mean(myRSStype$TOQp2,na.rm=T),mean(myRSStype$TOQp1,na.rm=T),
             mean(myRSStype$TOQ0,na.rm=T),mean(myRSStype$TOQn1,na.rm=T),mean(myRSStype$TOQn2,na.rm=T),mean(myRSStype$TOQn3,na.rm=T),
             mean(myRSStype$TOQn4,na.rm=T),mean(myRSStype$TOQn5,na.rm=T),mean(myRSStype$TOQn6,na.rm=T),mean(myRSStype$TOQn7,na.rm=T),
             mean(myRSStype$TOQn8,na.rm=T))
names(control)<- c("turnover")
names(rss)<-c("turnover")
dfControl<-cbind(dfControl,control)
dfRSS<-cbind(dfRSS,rss)

x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Turnover",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "Turnover")
p + theme_classic()  



myControl <- myL5L5
myRSStype <- myL5H5


################################ RSS Turnover ################
control <- c(mean(myControl$TOQp8,na.rm=T),mean(myControl$TOQp7,na.rm=T),mean(myControl$TOQp6,na.rm=T),mean(myControl$TOQp5,na.rm=T),
             mean(myControl$TOQp4,na.rm=T),mean(myControl$TOQp3,na.rm=T),mean(myControl$TOQp2,na.rm=T),mean(myControl$TOQp1,na.rm=T),
             mean(myControl$TOQ0,na.rm=T),mean(myControl$TOQn1,na.rm=T),mean(myControl$TOQn2,na.rm=T),mean(myControl$TOQn3,na.rm=T),
             mean(myControl$TOQn4,na.rm=T),mean(myControl$TOQn5,na.rm=T),mean(myControl$TOQn6,na.rm=T),mean(myControl$TOQn7,na.rm=T),
             mean(myControl$TOQn8,na.rm=T))
rss <- c(mean(myRSStype$TOQp8,na.rm=T),mean(myRSStype$TOQp7,na.rm=T),mean(myRSStype$TOQp6,na.rm=T),mean(myRSStype$TOQp5,na.rm=T),
         mean(myRSStype$TOQp4,na.rm=T),mean(myRSStype$TOQp3,na.rm=T),mean(myRSStype$TOQp2,na.rm=T),mean(myRSStype$TOQp1,na.rm=T),
         mean(myRSStype$TOQ0,na.rm=T),mean(myRSStype$TOQn1,na.rm=T),mean(myRSStype$TOQn2,na.rm=T),mean(myRSStype$TOQn3,na.rm=T),
         mean(myRSStype$TOQn4,na.rm=T),mean(myRSStype$TOQn5,na.rm=T),mean(myRSStype$TOQn6,na.rm=T),mean(myRSStype$TOQn7,na.rm=T),
         mean(myRSStype$TOQn8,na.rm=T))

names(control)<- c("turnover")
names(rss)<-c("turnover")
dfL5L5<-cbind(dfL5L5,control)
dfL5H5<-cbind(dfL5H5,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Turnover",
       x="Quarters around Rervese Split Quarter ( L5L5 in dotted line )", y = "Turnover")
p + theme_classic()  

################################### number of leverage ##################################
myControl <- myCTL
myRSStype <- myRSS

############################### RSS  ################
control <- c(mean(myControl$LVQp8,na.rm=T),mean(myControl$LVQp7,na.rm=T),mean(myControl$LVQp6,na.rm=T),mean(myControl$LVQp5,na.rm=T),
             mean(myControl$LVQp4,na.rm=T),mean(myControl$LVQp3,na.rm=T),mean(myControl$LVQp2,na.rm=T),mean(myControl$LVQp1,na.rm=T),
             mean(myControl$LVQ0,na.rm=T),mean(myControl$LVQn1,na.rm=T),mean(myControl$LVQn2,na.rm=T),mean(myControl$LVQn3,na.rm=T),
             mean(myControl$LVQn4,na.rm=T),mean(myControl$LVQn5,na.rm=T),mean(myControl$LVQn6,na.rm=T),mean(myControl$LVQn7,na.rm=T),
             mean(myControl$LVQn8,na.rm=T))
rss <- c(mean(myRSStype$LVQp8,na.rm=T),mean(myRSStype$LVQp7,na.rm=T),mean(myRSStype$LVQp6,na.rm=T),mean(myRSStype$LVQp5,na.rm=T),
         mean(myRSStype$LVQp4,na.rm=T),mean(myRSStype$LVQp3,na.rm=T),mean(myRSStype$LVQp2,na.rm=T),mean(myRSStype$LVQp1,na.rm=T),
         mean(myRSStype$LVQ0,na.rm=T),mean(myRSStype$LVQn1,na.rm=T),mean(myRSStype$LVQn2,na.rm=T),mean(myRSStype$LVQn3,na.rm=T),
         mean(myRSStype$LVQn4,na.rm=T),mean(myRSStype$LVQn5,na.rm=T),mean(myRSStype$LVQn6,na.rm=T),mean(myRSStype$LVQn7,na.rm=T),
         mean(myRSStype$LVQn8,na.rm=T))

names(control)<- c("leverage")
names(rss)<-c("leverage")
dfControl<-cbind(dfControl,control)
dfRSS<-cbind(dfRSS,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Leverage",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "Leverage")
p + theme_classic()  


myControl <- myL5L5
myRSStype <- myL5H5

############################### RSS  ################
control <- c(mean(myControl$LVQp8,na.rm=T),mean(myControl$LVQp7,na.rm=T),mean(myControl$LVQp6,na.rm=T),mean(myControl$LVQp5,na.rm=T),
             mean(myControl$LVQp4,na.rm=T),mean(myControl$LVQp3,na.rm=T),mean(myControl$LVQp2,na.rm=T),mean(myControl$LVQp1,na.rm=T),
             mean(myControl$LVQ0,na.rm=T),mean(myControl$LVQn1,na.rm=T),mean(myControl$LVQn2,na.rm=T),mean(myControl$LVQn3,na.rm=T),
             mean(myControl$LVQn4,na.rm=T),mean(myControl$LVQn5,na.rm=T),mean(myControl$LVQn6,na.rm=T),mean(myControl$LVQn7,na.rm=T),
             mean(myControl$LVQn8,na.rm=T))
rss <- c(mean(myRSStype$LVQp8,na.rm=T),mean(myRSStype$LVQp7,na.rm=T),mean(myRSStype$LVQp6,na.rm=T),mean(myRSStype$LVQp5,na.rm=T),
         mean(myRSStype$LVQp4,na.rm=T),mean(myRSStype$LVQp3,na.rm=T),mean(myRSStype$LVQp2,na.rm=T),mean(myRSStype$LVQp1,na.rm=T),
         mean(myRSStype$LVQ0,na.rm=T),mean(myRSStype$LVQn1,na.rm=T),mean(myRSStype$LVQn2,na.rm=T),mean(myRSStype$LVQn3,na.rm=T),
         mean(myRSStype$LVQn4,na.rm=T),mean(myRSStype$LVQn5,na.rm=T),mean(myRSStype$LVQn6,na.rm=T),mean(myRSStype$LVQn7,na.rm=T),
         mean(myRSStype$LVQn8,na.rm=T))

names(control)<- c("leverage")
names(rss)<-c("leverage")
dfL5L5<-cbind(dfL5L5,control)
dfL5H5<-cbind(dfL5H5,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Leverage",
       x="Quarters around Rervese Split Quarter ( L5L5 in dotted line )", y = "Leverage")
p + theme_classic()  



############################# Quarterly return ############################################
myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(mean(myControl$ARQp8,na.rm=T),mean(myControl$ARQp7,na.rm=T),mean(myControl$ARQp6,na.rm=T),mean(myControl$ARQp5,na.rm=T),
             mean(myControl$ARQp4,na.rm=T),mean(myControl$ARQp3,na.rm=T),mean(myControl$ARQp2,na.rm=T),mean(myControl$ARQp1,na.rm=T),
             mean(myControl$ARQ0,na.rm=T),mean(myControl$ARQn1,na.rm=T),mean(myControl$ARQn2,na.rm=T),mean(myControl$ARQn3,na.rm=T),
             mean(myControl$ARQn4,na.rm=T),mean(myControl$ARQn5,na.rm=T),mean(myControl$ARQn6,na.rm=T),mean(myControl$ARQn7,na.rm=T),
             mean(myControl$ARQn8,na.rm=T))
rss <- c(mean(myRSStype$ARQp8,na.rm=T),mean(myRSStype$ARQp7,na.rm=T),mean(myRSStype$ARQp6,na.rm=T),mean(myRSStype$ARQp5,na.rm=T),
         mean(myRSStype$ARQp4,na.rm=T),mean(myRSStype$ARQp3,na.rm=T),mean(myRSStype$ARQp2,na.rm=T),mean(myRSStype$ARQp1,na.rm=T),
         mean(myRSStype$ARQ0,na.rm=T),mean(myRSStype$ARQn1,na.rm=T),mean(myRSStype$ARQn2,na.rm=T),mean(myRSStype$ARQn3,na.rm=T),
         mean(myRSStype$ARQn4,na.rm=T),mean(myRSStype$ARQn5,na.rm=T),mean(myRSStype$ARQn6,na.rm=T),mean(myRSStype$ARQn7,na.rm=T),
         mean(myRSStype$ARQn8,na.rm=T))

names(control)<- c("AR")
names(rss)<-c("AR")
dfControl<-cbind(dfControl,control)
dfRSS<-cbind(dfRSS,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Abnormal Return",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "Abnormal Return")
p + theme_classic()  


myControl <- myL5L5
myRSStype <- myL5H5

control <- c(mean(myControl$ARQp8,na.rm=T),mean(myControl$ARQp7,na.rm=T),mean(myControl$ARQp6,na.rm=T),mean(myControl$ARQp5,na.rm=T),
             mean(myControl$ARQp4,na.rm=T),mean(myControl$ARQp3,na.rm=T),mean(myControl$ARQp2,na.rm=T),mean(myControl$ARQp1,na.rm=T),
             mean(myControl$ARQ0,na.rm=T),mean(myControl$ARQn1,na.rm=T),mean(myControl$ARQn2,na.rm=T),mean(myControl$ARQn3,na.rm=T),
             mean(myControl$ARQn4,na.rm=T),mean(myControl$ARQn5,na.rm=T),mean(myControl$ARQn6,na.rm=T),mean(myControl$ARQn7,na.rm=T),
             mean(myControl$ARQn8,na.rm=T))
rss <- c(mean(myRSStype$ARQp8,na.rm=T),mean(myRSStype$ARQp7,na.rm=T),mean(myRSStype$ARQp6,na.rm=T),mean(myRSStype$ARQp5,na.rm=T),
         mean(myRSStype$ARQp4,na.rm=T),mean(myRSStype$ARQp3,na.rm=T),mean(myRSStype$ARQp2,na.rm=T),mean(myRSStype$ARQp1,na.rm=T),
         mean(myRSStype$ARQ0,na.rm=T),mean(myRSStype$ARQn1,na.rm=T),mean(myRSStype$ARQn2,na.rm=T),mean(myRSStype$ARQn3,na.rm=T),
         mean(myRSStype$ARQn4,na.rm=T),mean(myRSStype$ARQn5,na.rm=T),mean(myRSStype$ARQn6,na.rm=T),mean(myRSStype$ARQn7,na.rm=T),
         mean(myRSStype$ARQn8,na.rm=T))

names(control)<- c("AR")
names(rss)<-c("AR")
dfL5L5<-cbind(dfL5L5,control)
dfL5H5<-cbind(dfL5H5,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Abnormal Return",
       x="Quarters around Rervese Split Quarter ( L5L5 in dotted line )", y = "Abnormal Return")
p + theme_classic()  


################################### number of Institutional Investors ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(mean(myControl$NIQp8,na.rm=T),mean(myControl$NIQp7,na.rm=T),mean(myControl$NIQp6,na.rm=T),mean(myControl$NIQp5,na.rm=T),
             mean(myControl$NIQp4,na.rm=T),mean(myControl$NIQp3,na.rm=T),mean(myControl$NIQp2,na.rm=T),mean(myControl$NIQp1,na.rm=T),
             mean(myControl$NIQ0,na.rm=T),mean(myControl$NIQn1,na.rm=T),mean(myControl$NIQn2,na.rm=T),mean(myControl$NIQn3,na.rm=T),
             mean(myControl$NIQn4,na.rm=T),mean(myControl$NIQn5,na.rm=T),mean(myControl$NIQn6,na.rm=T),mean(myControl$NIQn7,na.rm=T),
             mean(myControl$NIQn8,na.rm=T))
rss <- c(mean(myRSStype$NIQp8,na.rm=T),mean(myRSStype$NIQp7,na.rm=T),mean(myRSStype$NIQp6,na.rm=T),mean(myRSStype$NIQp5,na.rm=T),
         mean(myRSStype$NIQp4,na.rm=T),mean(myRSStype$NIQp3,na.rm=T),mean(myRSStype$NIQp2,na.rm=T),mean(myRSStype$NIQp1,na.rm=T),
         mean(myRSStype$NIQ0,na.rm=T),mean(myRSStype$NIQn1,na.rm=T),mean(myRSStype$NIQn2,na.rm=T),mean(myRSStype$NIQn3,na.rm=T),
         mean(myRSStype$NIQn4,na.rm=T),mean(myRSStype$NIQn5,na.rm=T),mean(myRSStype$NIQn6,na.rm=T),mean(myRSStype$NIQn7,na.rm=T),
         mean(myRSStype$NIQn8,na.rm=T))

names(control)<- c("NII")
names(rss)<-c("NII")
dfControl<-cbind(dfControl,control)
dfRSS<-cbind(dfRSS,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Number of Institutional Investors",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "Number of Institutional Investors")
p + theme_classic()  

################################### percentage of Institutional holdings ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(mean(myControl$IHQp8,na.rm=T),mean(myControl$IHQp7,na.rm=T),mean(myControl$IHQp6,na.rm=T),mean(myControl$IHQp5,na.rm=T),
             mean(myControl$IHQp4,na.rm=T),mean(myControl$IHQp3,na.rm=T),mean(myControl$IHQp2,na.rm=T),mean(myControl$IHQp1,na.rm=T),
             mean(myControl$IHQ0,na.rm=T),mean(myControl$IHQn1,na.rm=T),mean(myControl$IHQn2,na.rm=T),mean(myControl$IHQn3,na.rm=T),
             mean(myControl$IHQn4,na.rm=T),mean(myControl$IHQn5,na.rm=T),mean(myControl$IHQn6,na.rm=T),mean(myControl$IHQn7,na.rm=T),
             mean(myControl$IHQn8,na.rm=T))
rss <- c(mean(myRSStype$IHQp8,na.rm=T),mean(myRSStype$IHQp7,na.rm=T),mean(myRSStype$IHQp6,na.rm=T),mean(myRSStype$IHQp5,na.rm=T),
         mean(myRSStype$IHQp4,na.rm=T),mean(myRSStype$IHQp3,na.rm=T),mean(myRSStype$IHQp2,na.rm=T),mean(myRSStype$IHQp1,na.rm=T),
         mean(myRSStype$IHQ0,na.rm=T),mean(myRSStype$IHQn1,na.rm=T),mean(myRSStype$IHQn2,na.rm=T),mean(myRSStype$IHQn3,na.rm=T),
         mean(myRSStype$IHQn4,na.rm=T),mean(myRSStype$IHQn5,na.rm=T),mean(myRSStype$IHQn6,na.rm=T),mean(myRSStype$IHQn7,na.rm=T),
         mean(myRSStype$IHQn8,na.rm=T))

names(control)<- c("PIH")
names(rss)<-c("PIH")
dfControl<-cbind(dfControl,control)
dfRSS<-cbind(dfRSS,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - percentage of Institutional holdings",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "percentage of Institutional holdings")
p + theme_classic()  





################################### number of insiders ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(mean(myControl$NSQp8,na.rm=T),mean(myControl$NSQp7,na.rm=T),mean(myControl$NSQp6,na.rm=T),mean(myControl$NSQp5,na.rm=T),
             mean(myControl$NSQp4,na.rm=T),mean(myControl$NSQp3,na.rm=T),mean(myControl$NSQp2,na.rm=T),mean(myControl$NSQp1,na.rm=T),
             mean(myControl$NSQ0,na.rm=T),mean(myControl$NSQn1,na.rm=T),mean(myControl$NSQn2,na.rm=T),mean(myControl$NSQn3,na.rm=T),
             mean(myControl$NSQn4,na.rm=T),mean(myControl$NSQn5,na.rm=T),mean(myControl$NSQn6,na.rm=T),mean(myControl$NSQn7,na.rm=T),
             mean(myControl$NSQn8,na.rm=T))
rss <- c(mean(myRSStype$NSQp8,na.rm=T),mean(myRSStype$NSQp7,na.rm=T),mean(myRSStype$NSQp6,na.rm=T),mean(myRSStype$NSQp5,na.rm=T),
         mean(myRSStype$NSQp4,na.rm=T),mean(myRSStype$NSQp3,na.rm=T),mean(myRSStype$NSQp2,na.rm=T),mean(myRSStype$NSQp1,na.rm=T),
         mean(myRSStype$NSQ0,na.rm=T),mean(myRSStype$NSQn1,na.rm=T),mean(myRSStype$NSQn2,na.rm=T),mean(myRSStype$NSQn3,na.rm=T),
         mean(myRSStype$NSQn4,na.rm=T),mean(myRSStype$NSQn5,na.rm=T),mean(myRSStype$NSQn6,na.rm=T),mean(myRSStype$NSQn7,na.rm=T),
         mean(myRSStype$NSQn8,na.rm=T))

names(control)<- c("NIS")
names(rss)<-c("NIS")
dfControl<-cbind(dfControl,control)
dfRSS<-cbind(dfRSS,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Number of Insiders",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "Number of Insiders")
p + theme_classic()  


################################### number of insiders percentage holding ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(mean(myControl$SHQp8,na.rm=T),mean(myControl$SHQp7,na.rm=T),mean(myControl$SHQp6,na.rm=T),mean(myControl$SHQp5,na.rm=T),
             mean(myControl$SHQp4,na.rm=T),mean(myControl$SHQp3,na.rm=T),mean(myControl$SHQp2,na.rm=T),mean(myControl$SHQp1,na.rm=T),
             mean(myControl$SHQ0,na.rm=T),mean(myControl$SHQn1,na.rm=T),mean(myControl$SHQn2,na.rm=T),mean(myControl$SHQn3,na.rm=T),
             mean(myControl$SHQn4,na.rm=T),mean(myControl$SHQn5,na.rm=T),mean(myControl$SHQn6,na.rm=T),mean(myControl$SHQn7,na.rm=T),
             mean(myControl$SHQn8,na.rm=T))
rss <- c(mean(myRSStype$SHQp8,na.rm=T),mean(myRSStype$SHQp7,na.rm=T),mean(myRSStype$SHQp6,na.rm=T),mean(myRSStype$SHQp5,na.rm=T),
         mean(myRSStype$SHQp4,na.rm=T),mean(myRSStype$SHQp3,na.rm=T),mean(myRSStype$SHQp2,na.rm=T),mean(myRSStype$SHQp1,na.rm=T),
         mean(myRSStype$SHQ0,na.rm=T),mean(myRSStype$SHQn1,na.rm=T),mean(myRSStype$SHQn2,na.rm=T),mean(myRSStype$SHQn3,na.rm=T),
         mean(myRSStype$SHQn4,na.rm=T),mean(myRSStype$SHQn5,na.rm=T),mean(myRSStype$SHQn6,na.rm=T),mean(myRSStype$SHQn7,na.rm=T),
         mean(myRSStype$SHQn8,na.rm=T))

names(control)<- c("PSH")
names(rss)<-c("PSH")
dfControl<-cbind(dfControl,control)
dfRSS<-cbind(dfRSS,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Number of Insiders percentage holding",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "Number of Insiders Holding %")
p + theme_classic()  

################################### Risk ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(mean(myControl$RSQp8,na.rm=T),mean(myControl$RSQp7,na.rm=T),mean(myControl$RSQp6,na.rm=T),mean(myControl$RSQp5,na.rm=T),
             mean(myControl$RSQp4,na.rm=T),mean(myControl$RSQp3,na.rm=T),mean(myControl$RSQp2,na.rm=T),mean(myControl$RSQp1,na.rm=T),
             mean(myControl$RSQ0,na.rm=T),mean(myControl$RSQn1,na.rm=T),mean(myControl$RSQn2,na.rm=T),mean(myControl$RSQn3,na.rm=T),
             mean(myControl$RSQn4,na.rm=T),mean(myControl$RSQn5,na.rm=T),mean(myControl$RSQn6,na.rm=T),mean(myControl$RSQn7,na.rm=T),
             mean(myControl$RSQn8,na.rm=T))
rss <- c(mean(myRSStype$RSQp8,na.rm=T),mean(myRSStype$RSQp7,na.rm=T),mean(myRSStype$RSQp6,na.rm=T),mean(myRSStype$RSQp5,na.rm=T),
         mean(myRSStype$RSQp4,na.rm=T),mean(myRSStype$RSQp3,na.rm=T),mean(myRSStype$RSQp2,na.rm=T),mean(myRSStype$RSQp1,na.rm=T),
         mean(myRSStype$RSQ0,na.rm=T),mean(myRSStype$RSQn1,na.rm=T),mean(myRSStype$RSQn2,na.rm=T),mean(myRSStype$RSQn3,na.rm=T),
         mean(myRSStype$RSQn4,na.rm=T),mean(myRSStype$RSQn5,na.rm=T),mean(myRSStype$RSQn6,na.rm=T),mean(myRSStype$RSQn7,na.rm=T),
         mean(myRSStype$RSQn8,na.rm=T))

names(control)<- c("risk")
names(rss)<-c("risk")
dfControl<-cbind(dfControl,control)
dfRSS<-cbind(dfRSS,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Risk",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "Risk")
p + theme_classic()  

myControl <- myL5L5
myRSStype <- myL5H5
############################### RSS  ################
control <- c(mean(myControl$RSQp8,na.rm=T),mean(myControl$RSQp7,na.rm=T),mean(myControl$RSQp6,na.rm=T),mean(myControl$RSQp5,na.rm=T),
             mean(myControl$RSQp4,na.rm=T),mean(myControl$RSQp3,na.rm=T),mean(myControl$RSQp2,na.rm=T),mean(myControl$RSQp1,na.rm=T),
             mean(myControl$RSQ0,na.rm=T),mean(myControl$RSQn1,na.rm=T),mean(myControl$RSQn2,na.rm=T),mean(myControl$RSQn3,na.rm=T),
             mean(myControl$RSQn4,na.rm=T),mean(myControl$RSQn5,na.rm=T),mean(myControl$RSQn6,na.rm=T),mean(myControl$RSQn7,na.rm=T),
             mean(myControl$RSQn8,na.rm=T))
rss <- c(mean(myRSStype$RSQp8,na.rm=T),mean(myRSStype$RSQp7,na.rm=T),mean(myRSStype$RSQp6,na.rm=T),mean(myRSStype$RSQp5,na.rm=T),
         mean(myRSStype$RSQp4,na.rm=T),mean(myRSStype$RSQp3,na.rm=T),mean(myRSStype$RSQp2,na.rm=T),mean(myRSStype$RSQp1,na.rm=T),
         mean(myRSStype$RSQ0,na.rm=T),mean(myRSStype$RSQn1,na.rm=T),mean(myRSStype$RSQn2,na.rm=T),mean(myRSStype$RSQn3,na.rm=T),
         mean(myRSStype$RSQn4,na.rm=T),mean(myRSStype$RSQn5,na.rm=T),mean(myRSStype$RSQn6,na.rm=T),mean(myRSStype$RSQn7,na.rm=T),
         mean(myRSStype$RSQn8,na.rm=T))

names(control)<- c("risk")
names(rss)<-c("risk")
dfL5L5<-cbind(dfL5L5,control)
dfL5H5<-cbind(dfL5H5,rss)


x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Risk",
       x="Quarters around Rervese Split Quarter ( L5L5 in dotted line )", y = "Risk")
p + theme_classic()  

################################### Illiquidity ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(mean(myControl$IQQp8,na.rm=T),mean(myControl$IQQp7,na.rm=T),mean(myControl$IQQp6,na.rm=T),mean(myControl$IQQp5,na.rm=T),
             mean(myControl$IQQp4,na.rm=T),mean(myControl$IQQp3,na.rm=T),mean(myControl$IQQp2,na.rm=T),mean(myControl$IQQp1,na.rm=T),
             mean(myControl$IQQ0,na.rm=T),mean(myControl$IQQn1,na.rm=T),mean(myControl$IQQn2,na.rm=T),mean(myControl$IQQn3,na.rm=T),
             mean(myControl$IQQn4,na.rm=T),mean(myControl$IQQn5,na.rm=T),mean(myControl$IQQn6,na.rm=T),mean(myControl$IQQn7,na.rm=T),
             mean(myControl$IQQn8,na.rm=T))
rss <- c(mean(myRSStype$IQQp8,na.rm=T),mean(myRSStype$IQQp7,na.rm=T),mean(myRSStype$IQQp6,na.rm=T),mean(myRSStype$IQQp5,na.rm=T),
         mean(myRSStype$IQQp4,na.rm=T),mean(myRSStype$IQQp3,na.rm=T),mean(myRSStype$IQQp2,na.rm=T),mean(myRSStype$IQQp1,na.rm=T),
         mean(myRSStype$IQQ0,na.rm=T),mean(myRSStype$IQQn1,na.rm=T),mean(myRSStype$IQQn2,na.rm=T),mean(myRSStype$IQQn3,na.rm=T),
         mean(myRSStype$IQQn4,na.rm=T),mean(myRSStype$IQQn5,na.rm=T),mean(myRSStype$IQQn6,na.rm=T),mean(myRSStype$IQQn7,na.rm=T),
         mean(myRSStype$IQQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Illiquidity",
       x="Quarters around Rervese Split Quarter ( control in dotted line )", y = "Illiquidity")
p + theme_classic()  

myControl <- myL5L5
myRSStype <- myL5H5
############################### RSS  ################
control <- c(mean(myControl$IQQp8,na.rm=T),mean(myControl$IQQp7,na.rm=T),mean(myControl$IQQp6,na.rm=T),mean(myControl$IQQp5,na.rm=T),
             mean(myControl$IQQp4,na.rm=T),mean(myControl$IQQp3,na.rm=T),mean(myControl$IQQp2,na.rm=T),mean(myControl$IQQp1,na.rm=T),
             mean(myControl$IQQ0,na.rm=T),mean(myControl$IQQn1,na.rm=T),mean(myControl$IQQn2,na.rm=T),mean(myControl$IQQn3,na.rm=T),
             mean(myControl$IQQn4,na.rm=T),mean(myControl$IQQn5,na.rm=T),mean(myControl$IQQn6,na.rm=T),mean(myControl$IQQn7,na.rm=T),
             mean(myControl$IQQn8,na.rm=T))
rss <- c(mean(myRSStype$IQQp8,na.rm=T),mean(myRSStype$IQQp7,na.rm=T),mean(myRSStype$IQQp6,na.rm=T),mean(myRSStype$IQQp5,na.rm=T),
         mean(myRSStype$IQQp4,na.rm=T),mean(myRSStype$IQQp3,na.rm=T),mean(myRSStype$IQQp2,na.rm=T),mean(myRSStype$IQQp1,na.rm=T),
         mean(myRSStype$IQQ0,na.rm=T),mean(myRSStype$IQQn1,na.rm=T),mean(myRSStype$IQQn2,na.rm=T),mean(myRSStype$IQQn3,na.rm=T),
         mean(myRSStype$IQQn4,na.rm=T),mean(myRSStype$IQQn5,na.rm=T),mean(myRSStype$IQQn6,na.rm=T),mean(myRSStype$IQQn7,na.rm=T),
         mean(myRSStype$IQQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Illiquidity",
       x="Quarters around Rervese Split Quarter ( L5L5 in dotted line )", y = "Illiquidity")
p + theme_classic()  
########################## plot multiple lines on different scale ####################

myControl <- myRSS
myRSStype <- myRSS
############################### percentage insiders holding  ################

control <- c(mean(myRSStype$TOQp8,na.rm=T),mean(myRSStype$TOQp7,na.rm=T),mean(myRSStype$TOQp6,na.rm=T),mean(myRSStype$TOQp5,na.rm=T),
         mean(myRSStype$TOQp4,na.rm=T),mean(myRSStype$TOQp3,na.rm=T),mean(myRSStype$TOQp2,na.rm=T),mean(myRSStype$TOQp1,na.rm=T),
         mean(myRSStype$TOQ0,na.rm=T),mean(myRSStype$TOQn1,na.rm=T),mean(myRSStype$TOQn2,na.rm=T),mean(myRSStype$TOQn3,na.rm=T),
         mean(myRSStype$TOQn4,na.rm=T),mean(myRSStype$TOQn5,na.rm=T),mean(myRSStype$TOQn6,na.rm=T),mean(myRSStype$TOQn7,na.rm=T),
         mean(myRSStype$TOQn8,na.rm=T))

rss <- c(mean(myRSStype$SHQp8,na.rm=T),mean(myRSStype$SHQp7,na.rm=T),mean(myRSStype$SHQp6,na.rm=T),mean(myRSStype$SHQp5,na.rm=T),
         mean(myRSStype$SHQp4,na.rm=T),mean(myRSStype$SHQp3,na.rm=T),mean(myRSStype$SHQp2,na.rm=T),mean(myRSStype$SHQp1,na.rm=T),
         mean(myRSStype$SHQ0,na.rm=T),mean(myRSStype$SHQn1,na.rm=T),mean(myRSStype$SHQn2,na.rm=T),mean(myRSStype$SHQn3,na.rm=T),
         mean(myRSStype$SHQn4,na.rm=T),mean(myRSStype$SHQn5,na.rm=T),mean(myRSStype$SHQn6,na.rm=T),mean(myRSStype$SHQn7,na.rm=T),
         mean(myRSStype$SHQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)


p <- ggplot(gdata, aes(x = x1))
p <- p + geom_line(aes(y = rss, colour = "insiders") )

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = control/8, colour = "turnover"),linetype="dotdash")

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
p <- p + scale_y_continuous( sec.axis = sec_axis(~.*8, name = "turnover"))

# modifying colours and theme options
p <- p + scale_colour_manual(values = c("blue", "red"))
p <- p + labs(y = "insiders holdings[%]",
              x = "Quarters",
              colour = "Parameter")
p <- p + theme(legend.position = c(0.8, 0.9))
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))

#################################
colnames(dfRSS)<-c("QID","TO","LEV","AR","NII","PIH","NIS","PSH","RSK")
colnames(dfControl)<-c("QID","TO","LEV","AR","NII","PIH","NIS","PSH","RSK")
colnames(dfL5L5)<-c("QID","TO","LEV","AR","RSK")
colnames(dfL5H5)<-c("QID","TO","LEV","AR","RSK")
write.csv(dfRSS,"dfRSSmean.csv")
write.csv(dfControl,"dfControlmean.csv")
write.csv(dfL5L5,"dfL5L5mean.csv")
write.csv(dfL5H5,"dfL5H5mean.csv")

################# get RSS info ##########################
#Reverse Stock Splits only

#myRSS     #RSS only
#myCTL   #control group only
#myL5L5
#myL5H5
#myH5H5
#myCTL$RSStype<-NA
#myCTL_L5L5 
#myCTL_L5H5 
#myCTL_H5H5 


############################### descrptive statistics ##################
#output summary




#RSS  group only myDataRSS
myDataRSS2<-myRSS[,-c(1,2,28:36)]
myDataRSS2$RSStype<-NULL
myDataRSS2$SplitRatio=1/myDataRSS2$SplitRatio
mySummary<-(summary(myDataRSS2))

mySummary[1,]<- gsub("Min.   :","",mySummary[1,])
mySummary[2,]<- gsub("1st Qu.:","",mySummary[2,])
mySummary[3,]<- gsub("Median :","",mySummary[3,])
mySummary[4,]<- gsub("Mean   :","",mySummary[4,])
mySummary[5,]<- gsub("3rd Qu.:","",mySummary[5,])
mySummary[6,]<- gsub("Max.   :","",mySummary[6,])

mySD<-(lapply(myDataRSS2,sd,na.rm = T))
mySummary<-rbind(mySummary,mySD)
myQuantile<-(lapply(myDataRSS2,quantile,na.rm=T))
for(j in 1L: length(myQuantile)){
  names(myQuantile[[j]])<-NULL
}

mySummary<-rbind(mySummary,myQuantile)
mySummary<-t(mySummary)
#colnames(mySummary)<-c("Minimum","1stQ","Median","Mean","3rdQ","Maximum","Std Dev","0th","25th","50th","75th","100th")
write.csv(mySummary,"RSS Summary 2.csv")


#control group
myDataRSS2<-myCTL[,-c(1,2,28:36)]
myDataRSS2$RSStype<-NULL
myDataRSS2$SplitRatio=1/myDataRSS2$SplitRatio
mySummary<-(summary(myDataRSS2))

mySummary[1,]<- gsub("Min.   :","",mySummary[1,])
mySummary[2,]<- gsub("1st Qu.:","",mySummary[2,])
mySummary[3,]<- gsub("Median :","",mySummary[3,])
mySummary[4,]<- gsub("Mean   :","",mySummary[4,])
mySummary[5,]<- gsub("3rd Qu.:","",mySummary[5,])
mySummary[6,]<- gsub("Max.   :","",mySummary[6,])

mySD<-(lapply(myDataRSS2,sd,na.rm = T))
mySummary<-rbind(mySummary,mySD)
myQuantile<-(lapply(myDataRSS2,quantile,na.rm=T))

for(j in 1L: length(myQuantile)){
  names(myQuantile[[j]])<-NULL
}

mySummary<-rbind(mySummary,myQuantile)
mySummary<-t(mySummary)
#colnames(mySummary)<-c("Minimum","1stQ","Median","Mean","3rdQ","Maximum","Std Dev","0th","25th","50th","75th","100th")
write.csv(mySummary,"Control Summary 2.csv")

