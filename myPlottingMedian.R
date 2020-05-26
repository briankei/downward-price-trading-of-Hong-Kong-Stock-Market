#setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/Bloomberg")
#setwd("d:/SharedFolder/myR/Bloomberg")
library(lubridate)
library(beepr)
library(ggplot2)
library(DescTools)

pd1= read.csv("DD_setQturnover-9.csv",header = T)
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

#################################Quarteryly Turnover

myControl <- myCTL
myRSStype <- myRSS


################################ RSS Turnover ################
control <- c(median(myControl$TOQp8,na.rm=T),median(myControl$TOQp7,na.rm=T),median(myControl$TOQp6,na.rm=T),median(myControl$TOQp5,na.rm=T),
            median(myControl$TOQp4,na.rm=T),median(myControl$TOQp3,na.rm=T),median(myControl$TOQp2,na.rm=T),median(myControl$TOQp1,na.rm=T),
            median(myControl$TOQ0,na.rm=T),median(myControl$TOQn1,na.rm=T),median(myControl$TOQn2,na.rm=T),median(myControl$TOQn3,na.rm=T),
            median(myControl$TOQn4,na.rm=T),median(myControl$TOQn5,na.rm=T),median(myControl$TOQn6,na.rm=T),median(myControl$TOQn7,na.rm=T),
            median(myControl$TOQn8,na.rm=T))
rss <- c(median(myRSStype$TOQp8,na.rm=T),median(myRSStype$TOQp7,na.rm=T),median(myRSStype$TOQp6,na.rm=T),median(myRSStype$TOQp5,na.rm=T),
             median(myRSStype$TOQp4,na.rm=T),median(myRSStype$TOQp3,na.rm=T),median(myRSStype$TOQp2,na.rm=T),median(myRSStype$TOQp1,na.rm=T),
             median(myRSStype$TOQ0,na.rm=T),median(myRSStype$TOQn1,na.rm=T),median(myRSStype$TOQn2,na.rm=T),median(myRSStype$TOQn3,na.rm=T),
             median(myRSStype$TOQn4,na.rm=T),median(myRSStype$TOQn5,na.rm=T),median(myRSStype$TOQn6,na.rm=T),median(myRSStype$TOQn7,na.rm=T),
             median(myRSStype$TOQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Turnover (Median)",
       x="Quarters  (Q(0) is the RSS quarter,   control firms in dotted line )", y = "Turnover")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)) 



myControl <- myL5L5
myRSStype <- myL5H5


################################ RSS Turnover ################
control <- c(median(myControl$TOQp8,na.rm=T),median(myControl$TOQp7,na.rm=T),median(myControl$TOQp6,na.rm=T),median(myControl$TOQp5,na.rm=T),
             median(myControl$TOQp4,na.rm=T),median(myControl$TOQp3,na.rm=T),median(myControl$TOQp2,na.rm=T),median(myControl$TOQp1,na.rm=T),
             median(myControl$TOQ0,na.rm=T),median(myControl$TOQn1,na.rm=T),median(myControl$TOQn2,na.rm=T),median(myControl$TOQn3,na.rm=T),
             median(myControl$TOQn4,na.rm=T),median(myControl$TOQn5,na.rm=T),median(myControl$TOQn6,na.rm=T),median(myControl$TOQn7,na.rm=T),
             median(myControl$TOQn8,na.rm=T))
rss <- c(median(myRSStype$TOQp8,na.rm=T),median(myRSStype$TOQp7,na.rm=T),median(myRSStype$TOQp6,na.rm=T),median(myRSStype$TOQp5,na.rm=T),
         median(myRSStype$TOQp4,na.rm=T),median(myRSStype$TOQp3,na.rm=T),median(myRSStype$TOQp2,na.rm=T),median(myRSStype$TOQp1,na.rm=T),
         median(myRSStype$TOQ0,na.rm=T),median(myRSStype$TOQn1,na.rm=T),median(myRSStype$TOQn2,na.rm=T),median(myRSStype$TOQn3,na.rm=T),
         median(myRSStype$TOQn4,na.rm=T),median(myRSStype$TOQn5,na.rm=T),median(myRSStype$TOQn6,na.rm=T),median(myRSStype$TOQn7,na.rm=T),
         median(myRSStype$TOQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Turnover (Median)",
       x="Quarters  (Q(0) is the RSS quarter,   L5L5 firms in dotted line )", y = "Turnover")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)) 

################################### number of leverage ##################################
myControl <- myCTL
myRSStype <- myRSS

############################### RSS  ################
control <- c(median(myControl$LVQp8,na.rm=T),median(myControl$LVQp7,na.rm=T),median(myControl$LVQp6,na.rm=T),median(myControl$LVQp5,na.rm=T),
             median(myControl$LVQp4,na.rm=T),median(myControl$LVQp3,na.rm=T),median(myControl$LVQp2,na.rm=T),median(myControl$LVQp1,na.rm=T),
             median(myControl$LVQ0,na.rm=T),median(myControl$LVQn1,na.rm=T),median(myControl$LVQn2,na.rm=T),median(myControl$LVQn3,na.rm=T),
             median(myControl$LVQn4,na.rm=T),median(myControl$LVQn5,na.rm=T),median(myControl$LVQn6,na.rm=T),median(myControl$LVQn7,na.rm=T),
             median(myControl$LVQn8,na.rm=T))
rss <- c(median(myRSStype$LVQp8,na.rm=T),median(myRSStype$LVQp7,na.rm=T),median(myRSStype$LVQp6,na.rm=T),median(myRSStype$LVQp5,na.rm=T),
         median(myRSStype$LVQp4,na.rm=T),median(myRSStype$LVQp3,na.rm=T),median(myRSStype$LVQp2,na.rm=T),median(myRSStype$LVQp1,na.rm=T),
         median(myRSStype$LVQ0,na.rm=T),median(myRSStype$LVQn1,na.rm=T),median(myRSStype$LVQn2,na.rm=T),median(myRSStype$LVQn3,na.rm=T),
         median(myRSStype$LVQn4,na.rm=T),median(myRSStype$LVQn5,na.rm=T),median(myRSStype$LVQn6,na.rm=T),median(myRSStype$LVQn7,na.rm=T),
         median(myRSStype$LVQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Leverage (Median)",
       x="Quarters  (Q(0) is the RSS quarter,   control firms in dotted line )", y = "Leverage")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))


myControl <- myL5L5
myRSStype <- myL5H5

############################### RSS  ################
control <- c(median(myControl$LVQp8,na.rm=T),median(myControl$LVQp7,na.rm=T),median(myControl$LVQp6,na.rm=T),median(myControl$LVQp5,na.rm=T),
             median(myControl$LVQp4,na.rm=T),median(myControl$LVQp3,na.rm=T),median(myControl$LVQp2,na.rm=T),median(myControl$LVQp1,na.rm=T),
             median(myControl$LVQ0,na.rm=T),median(myControl$LVQn1,na.rm=T),median(myControl$LVQn2,na.rm=T),median(myControl$LVQn3,na.rm=T),
             median(myControl$LVQn4,na.rm=T),median(myControl$LVQn5,na.rm=T),median(myControl$LVQn6,na.rm=T),median(myControl$LVQn7,na.rm=T),
             median(myControl$LVQn8,na.rm=T))
rss <- c(median(myRSStype$LVQp8,na.rm=T),median(myRSStype$LVQp7,na.rm=T),median(myRSStype$LVQp6,na.rm=T),median(myRSStype$LVQp5,na.rm=T),
         median(myRSStype$LVQp4,na.rm=T),median(myRSStype$LVQp3,na.rm=T),median(myRSStype$LVQp2,na.rm=T),median(myRSStype$LVQp1,na.rm=T),
         median(myRSStype$LVQ0,na.rm=T),median(myRSStype$LVQn1,na.rm=T),median(myRSStype$LVQn2,na.rm=T),median(myRSStype$LVQn3,na.rm=T),
         median(myRSStype$LVQn4,na.rm=T),median(myRSStype$LVQn5,na.rm=T),median(myRSStype$LVQn6,na.rm=T),median(myRSStype$LVQn7,na.rm=T),
         median(myRSStype$LVQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Leverage (Median)",
       x="Quarters (Q(0) is the RSS quarter,   L5L5firms in dotted line )", y = "Leverage")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))



############################# Quarterly return ############################################
myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(median(myControl$ARQp8,na.rm=T),median(myControl$ARQp7,na.rm=T),median(myControl$ARQp6,na.rm=T),median(myControl$ARQp5,na.rm=T),
             median(myControl$ARQp4,na.rm=T),median(myControl$ARQp3,na.rm=T),median(myControl$ARQp2,na.rm=T),median(myControl$ARQp1,na.rm=T),
             median(myControl$ARQ0,na.rm=T),median(myControl$ARQn1,na.rm=T),median(myControl$ARQn2,na.rm=T),median(myControl$ARQn3,na.rm=T),
             median(myControl$ARQn4,na.rm=T),median(myControl$ARQn5,na.rm=T),median(myControl$ARQn6,na.rm=T),median(myControl$ARQn7,na.rm=T),
             median(myControl$ARQn8,na.rm=T))
rss <- c(median(myRSStype$ARQp8,na.rm=T),median(myRSStype$ARQp7,na.rm=T),median(myRSStype$ARQp6,na.rm=T),median(myRSStype$ARQp5,na.rm=T),
         median(myRSStype$ARQp4,na.rm=T),median(myRSStype$ARQp3,na.rm=T),median(myRSStype$ARQp2,na.rm=T),median(myRSStype$ARQp1,na.rm=T),
         median(myRSStype$ARQ0,na.rm=T),median(myRSStype$ARQn1,na.rm=T),median(myRSStype$ARQn2,na.rm=T),median(myRSStype$ARQn3,na.rm=T),
         median(myRSStype$ARQn4,na.rm=T),median(myRSStype$ARQn5,na.rm=T),median(myRSStype$ARQn6,na.rm=T),median(myRSStype$ARQn7,na.rm=T),
         median(myRSStype$ARQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Quarterly Abnormal Return (Median)",
       x="Quarters (Q(0) is the RSS quarter,   control firms in dotted line )", y = "Abnormal Return")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))


myControl <- myL5L5
myRSStype <- myL5H5

control <- c(median(myControl$ARQp8,na.rm=T),median(myControl$ARQp7,na.rm=T),median(myControl$ARQp6,na.rm=T),median(myControl$ARQp5,na.rm=T),
             median(myControl$ARQp4,na.rm=T),median(myControl$ARQp3,na.rm=T),median(myControl$ARQp2,na.rm=T),median(myControl$ARQp1,na.rm=T),
             median(myControl$ARQ0,na.rm=T),median(myControl$ARQn1,na.rm=T),median(myControl$ARQn2,na.rm=T),median(myControl$ARQn3,na.rm=T),
             median(myControl$ARQn4,na.rm=T),median(myControl$ARQn5,na.rm=T),median(myControl$ARQn6,na.rm=T),median(myControl$ARQn7,na.rm=T),
             median(myControl$ARQn8,na.rm=T))
rss <- c(median(myRSStype$ARQp8,na.rm=T),median(myRSStype$ARQp7,na.rm=T),median(myRSStype$ARQp6,na.rm=T),median(myRSStype$ARQp5,na.rm=T),
         median(myRSStype$ARQp4,na.rm=T),median(myRSStype$ARQp3,na.rm=T),median(myRSStype$ARQp2,na.rm=T),median(myRSStype$ARQp1,na.rm=T),
         median(myRSStype$ARQ0,na.rm=T),median(myRSStype$ARQn1,na.rm=T),median(myRSStype$ARQn2,na.rm=T),median(myRSStype$ARQn3,na.rm=T),
         median(myRSStype$ARQn4,na.rm=T),median(myRSStype$ARQn5,na.rm=T),median(myRSStype$ARQn6,na.rm=T),median(myRSStype$ARQn7,na.rm=T),
         median(myRSStype$ARQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Abnormal Return (Median)",
       x="Quarters (Q(0) is the RSS quarter,   L5L5 firms in dotted line )", y = "Abnormal Return")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))

############################# Accumulated  return ############################################
myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(median(myControl$ARQp8n8,na.rm=T),median(myControl$ARQp7n8,na.rm=T),median(myControl$ARQp6n8,na.rm=T),median(myControl$ARQp5n8,na.rm=T),
             median(myControl$ARQp4n8,na.rm=T),median(myControl$ARQp3n8,na.rm=T),median(myControl$ARQp2n8,na.rm=T),median(myControl$ARQp1n8,na.rm=T),
             median(myControl$ARQ0n8,na.rm=T),median(myControl$ARQn1n8,na.rm=T),median(myControl$ARQn2n8,na.rm=T),median(myControl$ARQn3n8,na.rm=T),
             median(myControl$ARQn4n8,na.rm=T),median(myControl$ARQn5n8,na.rm=T),median(myControl$ARQn6n8,na.rm=T),median(myControl$ARQn7n8,na.rm=T),
             median(myControl$ARQn8n8,na.rm=T))
rss <- c(median(myRSStype$ARQp8n8,na.rm=T),median(myRSStype$ARQp7n8,na.rm=T),median(myRSStype$ARQp6n8,na.rm=T),median(myRSStype$ARQp5n8,na.rm=T),
         median(myRSStype$ARQp4n8,na.rm=T),median(myRSStype$ARQp3n8,na.rm=T),median(myRSStype$ARQp2n8,na.rm=T),median(myRSStype$ARQp1n8,na.rm=T),
         median(myRSStype$ARQ0n8,na.rm=T),median(myRSStype$ARQn1n8,na.rm=T),median(myRSStype$ARQn2n8,na.rm=T),median(myRSStype$ARQn3n8,na.rm=T),
         median(myRSStype$ARQn4n8,na.rm=T),median(myRSStype$ARQn5n8,na.rm=T),median(myRSStype$ARQn6n8,na.rm=T),median(myRSStype$ARQn7n8,na.rm=T),
         median(myRSStype$ARQn8n8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Accumulated Abnormal Return (Median)",
       x="Quarters (Q(0) is the RSS quarter,   control firms in dotted line )", y = "Abnormal Return")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))


myControl <- myL5L5
myRSStype <- myL5H5

control <- c(median(myControl$ARQp8n8,na.rm=T),median(myControl$ARQp7n8,na.rm=T),median(myControl$ARQp6n8,na.rm=T),median(myControl$ARQp5n8,na.rm=T),
             median(myControl$ARQp4n8,na.rm=T),median(myControl$ARQp3n8,na.rm=T),median(myControl$ARQp2n8,na.rm=T),median(myControl$ARQp1n8,na.rm=T),
             median(myControl$ARQ0n8,na.rm=T),median(myControl$ARQn1n8,na.rm=T),median(myControl$ARQn2n8,na.rm=T),median(myControl$ARQn3n8,na.rm=T),
             median(myControl$ARQn4n8,na.rm=T),median(myControl$ARQn5n8,na.rm=T),median(myControl$ARQn6n8,na.rm=T),median(myControl$ARQn7n8,na.rm=T),
             median(myControl$ARQn8n8,na.rm=T))
rss <- c(median(myRSStype$ARQp8n8,na.rm=T),median(myRSStype$ARQp7n8,na.rm=T),median(myRSStype$ARQp6n8,na.rm=T),median(myRSStype$ARQp5n8,na.rm=T),
         median(myRSStype$ARQp4n8,na.rm=T),median(myRSStype$ARQp3n8,na.rm=T),median(myRSStype$ARQp2n8,na.rm=T),median(myRSStype$ARQp1n8,na.rm=T),
         median(myRSStype$ARQ0n8,na.rm=T),median(myRSStype$ARQn1n8,na.rm=T),median(myRSStype$ARQn2n8,na.rm=T),median(myRSStype$ARQn3n8,na.rm=T),
         median(myRSStype$ARQn4n8,na.rm=T),median(myRSStype$ARQn5n8,na.rm=T),median(myRSStype$ARQn6n8,na.rm=T),median(myRSStype$ARQn7n8,na.rm=T),
         median(myRSStype$ARQn8n8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Accumulated Return (Median)",
       x="Quarters (Q(0) is the RSS quarter,   L5L5 firms in dotted line )", y = "Abnormal Return")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))
############################## mean accumulate return #################
myControl <- myCTL
myRSStype <- myRSS

############################### RSS  ################
control <- c(mean(myControl$ARQp8n8,na.rm=T),mean(myControl$ARQp7n8,na.rm=T),mean(myControl$ARQp6n8,na.rm=T),mean(myControl$ARQp5n8,na.rm=T),
             mean(myControl$ARQp4n8,na.rm=T),mean(myControl$ARQp3n8,na.rm=T),mean(myControl$ARQp2n8,na.rm=T),mean(myControl$ARQp1n8,na.rm=T),
             mean(myControl$ARQ0n8,na.rm=T),mean(myControl$ARQn1n8,na.rm=T),mean(myControl$ARQn2n8,na.rm=T),mean(myControl$ARQn3n8,na.rm=T),
             mean(myControl$ARQn4n8,na.rm=T),mean(myControl$ARQn5n8,na.rm=T),mean(myControl$ARQn6n8,na.rm=T),mean(myControl$ARQn7n8,na.rm=T),
             mean(myControl$ARQn8n8,na.rm=T))
rss <- c(mean(myRSStype$ARQp8n8,na.rm=T),mean(myRSStype$ARQp7n8,na.rm=T),mean(myRSStype$ARQp6n8,na.rm=T),mean(myRSStype$ARQp5n8,na.rm=T),
         mean(myRSStype$ARQp4n8,na.rm=T),mean(myRSStype$ARQp3n8,na.rm=T),mean(myRSStype$ARQp2n8,na.rm=T),mean(myRSStype$ARQp1n8,na.rm=T),
         mean(myRSStype$ARQ0n8,na.rm=T),mean(myRSStype$ARQn1n8,na.rm=T),mean(myRSStype$ARQn2n8,na.rm=T),mean(myRSStype$ARQn3n8,na.rm=T),
         mean(myRSStype$ARQn4n8,na.rm=T),mean(myRSStype$ARQn5n8,na.rm=T),mean(myRSStype$ARQn6n8,na.rm=T),mean(myRSStype$ARQn7n8,na.rm=T),
         mean(myRSStype$ARQn8n8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Accumulated Abnormal Return (mean)",
       x="Quarters (Q(0) is the RSS quarter,   control firms in dotted line )", y = "Abnormal Return")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))


myControl <- myL5L5
myRSStype <- myL5H5

control <- c(mean(myControl$ARQp8n8,na.rm=T),mean(myControl$ARQp7n8,na.rm=T),mean(myControl$ARQp6n8,na.rm=T),mean(myControl$ARQp5n8,na.rm=T),
             mean(myControl$ARQp4n8,na.rm=T),mean(myControl$ARQp3n8,na.rm=T),mean(myControl$ARQp2n8,na.rm=T),mean(myControl$ARQp1n8,na.rm=T),
             mean(myControl$ARQ0n8,na.rm=T),mean(myControl$ARQn1n8,na.rm=T),mean(myControl$ARQn2n8,na.rm=T),mean(myControl$ARQn3n8,na.rm=T),
             mean(myControl$ARQn4n8,na.rm=T),mean(myControl$ARQn5n8,na.rm=T),mean(myControl$ARQn6n8,na.rm=T),mean(myControl$ARQn7n8,na.rm=T),
             mean(myControl$ARQn8n8,na.rm=T))
rss <- c(mean(myRSStype$ARQp8n8,na.rm=T),mean(myRSStype$ARQp7n8,na.rm=T),mean(myRSStype$ARQp6n8,na.rm=T),mean(myRSStype$ARQp5n8,na.rm=T),
         mean(myRSStype$ARQp4n8,na.rm=T),mean(myRSStype$ARQp3n8,na.rm=T),mean(myRSStype$ARQp2n8,na.rm=T),mean(myRSStype$ARQp1n8,na.rm=T),
         mean(myRSStype$ARQ0n8,na.rm=T),mean(myRSStype$ARQn1n8,na.rm=T),mean(myRSStype$ARQn2n8,na.rm=T),mean(myRSStype$ARQn3n8,na.rm=T),
         mean(myRSStype$ARQn4n8,na.rm=T),mean(myRSStype$ARQn5n8,na.rm=T),mean(myRSStype$ARQn6n8,na.rm=T),mean(myRSStype$ARQn7n8,na.rm=T),
         mean(myRSStype$ARQn8n8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Accumulated Return (mean)",
       x="Quarters (Q(0) is the RSS quarter,   L5L5 firms in dotted line )", y = "Abnormal Return")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))








################################### number of Institutional Investors ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(median(myControl$NIQp8,na.rm=T),median(myControl$NIQp7,na.rm=T),median(myControl$NIQp6,na.rm=T),median(myControl$NIQp5,na.rm=T),
             median(myControl$NIQp4,na.rm=T),median(myControl$NIQp3,na.rm=T),median(myControl$NIQp2,na.rm=T),median(myControl$NIQp1,na.rm=T),
             median(myControl$NIQ0,na.rm=T),median(myControl$NIQn1,na.rm=T),median(myControl$NIQn2,na.rm=T),median(myControl$NIQn3,na.rm=T),
             median(myControl$NIQn4,na.rm=T),median(myControl$NIQn5,na.rm=T),median(myControl$NIQn6,na.rm=T),median(myControl$NIQn7,na.rm=T),
             median(myControl$NIQn8,na.rm=T))
rss <- c(median(myRSStype$NIQp8,na.rm=T),median(myRSStype$NIQp7,na.rm=T),median(myRSStype$NIQp6,na.rm=T),median(myRSStype$NIQp5,na.rm=T),
         median(myRSStype$NIQp4,na.rm=T),median(myRSStype$NIQp3,na.rm=T),median(myRSStype$NIQp2,na.rm=T),median(myRSStype$NIQp1,na.rm=T),
         median(myRSStype$NIQ0,na.rm=T),median(myRSStype$NIQn1,na.rm=T),median(myRSStype$NIQn2,na.rm=T),median(myRSStype$NIQn3,na.rm=T),
         median(myRSStype$NIQn4,na.rm=T),median(myRSStype$NIQn5,na.rm=T),median(myRSStype$NIQn6,na.rm=T),median(myRSStype$NIQn7,na.rm=T),
         median(myRSStype$NIQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Number of Institutional Investors (Median)",
       x="Quarters(Q(0) is the RSS quarter,   control firms in dotted line )", y = "Number of Institutional Investors")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)) 

################################### percentage of Institutional holdings ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(median(myControl$IHQp8,na.rm=T),median(myControl$IHQp7,na.rm=T),median(myControl$IHQp6,na.rm=T),median(myControl$IHQp5,na.rm=T),
             median(myControl$IHQp4,na.rm=T),median(myControl$IHQp3,na.rm=T),median(myControl$IHQp2,na.rm=T),median(myControl$IHQp1,na.rm=T),
             median(myControl$IHQ0,na.rm=T),median(myControl$IHQn1,na.rm=T),median(myControl$IHQn2,na.rm=T),median(myControl$IHQn3,na.rm=T),
             median(myControl$IHQn4,na.rm=T),median(myControl$IHQn5,na.rm=T),median(myControl$IHQn6,na.rm=T),median(myControl$IHQn7,na.rm=T),
             median(myControl$IHQn8,na.rm=T))
rss <- c(median(myRSStype$IHQp8,na.rm=T),median(myRSStype$IHQp7,na.rm=T),median(myRSStype$IHQp6,na.rm=T),median(myRSStype$IHQp5,na.rm=T),
         median(myRSStype$IHQp4,na.rm=T),median(myRSStype$IHQp3,na.rm=T),median(myRSStype$IHQp2,na.rm=T),median(myRSStype$IHQp1,na.rm=T),
         median(myRSStype$IHQ0,na.rm=T),median(myRSStype$IHQn1,na.rm=T),median(myRSStype$IHQn2,na.rm=T),median(myRSStype$IHQn3,na.rm=T),
         median(myRSStype$IHQn4,na.rm=T),median(myRSStype$IHQn5,na.rm=T),median(myRSStype$IHQn6,na.rm=T),median(myRSStype$IHQn7,na.rm=T),
         median(myRSStype$IHQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - percentage of Institutional holdings (Median)",
       x="Quarters (Q(0) is the RSS quarter,   control firms in dotted line )", y = "percentage of Institutional holdings")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))  






################################### number of insiders ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(median(myControl$NSQp8,na.rm=T),median(myControl$NSQp7,na.rm=T),median(myControl$NSQp6,na.rm=T),median(myControl$NSQp5,na.rm=T),
             median(myControl$NSQp4,na.rm=T),median(myControl$NSQp3,na.rm=T),median(myControl$NSQp2,na.rm=T),median(myControl$NSQp1,na.rm=T),
             median(myControl$NSQ0,na.rm=T),median(myControl$NSQn1,na.rm=T),median(myControl$NSQn2,na.rm=T),median(myControl$NSQn3,na.rm=T),
             median(myControl$NSQn4,na.rm=T),median(myControl$NSQn5,na.rm=T),median(myControl$NSQn6,na.rm=T),median(myControl$NSQn7,na.rm=T),
             median(myControl$NSQn8,na.rm=T))
rss <- c(median(myRSStype$NSQp8,na.rm=T),median(myRSStype$NSQp7,na.rm=T),median(myRSStype$NSQp6,na.rm=T),median(myRSStype$NSQp5,na.rm=T),
         median(myRSStype$NSQp4,na.rm=T),median(myRSStype$NSQp3,na.rm=T),median(myRSStype$NSQp2,na.rm=T),median(myRSStype$NSQp1,na.rm=T),
         median(myRSStype$NSQ0,na.rm=T),median(myRSStype$NSQn1,na.rm=T),median(myRSStype$NSQn2,na.rm=T),median(myRSStype$NSQn3,na.rm=T),
         median(myRSStype$NSQn4,na.rm=T),median(myRSStype$NSQn5,na.rm=T),median(myRSStype$NSQn6,na.rm=T),median(myRSStype$NSQn7,na.rm=T),
         median(myRSStype$NSQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Number of Insiders (Median)",
       x="Quarters (Q(0) is the RSS quarter,   control firms in dotted line )", y = "Number of Insiders")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)) 


################################### number of insiders percentage holding ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(median(myControl$SHQp8,na.rm=T),median(myControl$SHQp7,na.rm=T),median(myControl$SHQp6,na.rm=T),median(myControl$SHQp5,na.rm=T),
             median(myControl$SHQp4,na.rm=T),median(myControl$SHQp3,na.rm=T),median(myControl$SHQp2,na.rm=T),median(myControl$SHQp1,na.rm=T),
             median(myControl$SHQ0,na.rm=T),median(myControl$SHQn1,na.rm=T),median(myControl$SHQn2,na.rm=T),median(myControl$SHQn3,na.rm=T),
             median(myControl$SHQn4,na.rm=T),median(myControl$SHQn5,na.rm=T),median(myControl$SHQn6,na.rm=T),median(myControl$SHQn7,na.rm=T),
             median(myControl$SHQn8,na.rm=T))
rss <- c(median(myRSStype$SHQp8,na.rm=T),median(myRSStype$SHQp7,na.rm=T),median(myRSStype$SHQp6,na.rm=T),median(myRSStype$SHQp5,na.rm=T),
         median(myRSStype$SHQp4,na.rm=T),median(myRSStype$SHQp3,na.rm=T),median(myRSStype$SHQp2,na.rm=T),median(myRSStype$SHQp1,na.rm=T),
         median(myRSStype$SHQ0,na.rm=T),median(myRSStype$SHQn1,na.rm=T),median(myRSStype$SHQn2,na.rm=T),median(myRSStype$SHQn3,na.rm=T),
         median(myRSStype$SHQn4,na.rm=T),median(myRSStype$SHQn5,na.rm=T),median(myRSStype$SHQn6,na.rm=T),median(myRSStype$SHQn7,na.rm=T),
         median(myRSStype$SHQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Number of Insiders percentage holding (Median)",
       x="Quarters(Q(0) is the RSS quarter,   control firms in dotted line )", y = "Number of Insiders Holding %")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))  

################################### Risk ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(median(myControl$RSQp8,na.rm=T),median(myControl$RSQp7,na.rm=T),median(myControl$RSQp6,na.rm=T),median(myControl$RSQp5,na.rm=T),
             median(myControl$RSQp4,na.rm=T),median(myControl$RSQp3,na.rm=T),median(myControl$RSQp2,na.rm=T),median(myControl$RSQp1,na.rm=T),
             median(myControl$RSQ0,na.rm=T),median(myControl$RSQn1,na.rm=T),median(myControl$RSQn2,na.rm=T),median(myControl$RSQn3,na.rm=T),
             median(myControl$RSQn4,na.rm=T),median(myControl$RSQn5,na.rm=T),median(myControl$RSQn6,na.rm=T),median(myControl$RSQn7,na.rm=T),
             median(myControl$RSQn8,na.rm=T))
rss <- c(median(myRSStype$RSQp8,na.rm=T),median(myRSStype$RSQp7,na.rm=T),median(myRSStype$RSQp6,na.rm=T),median(myRSStype$RSQp5,na.rm=T),
         median(myRSStype$RSQp4,na.rm=T),median(myRSStype$RSQp3,na.rm=T),median(myRSStype$RSQp2,na.rm=T),median(myRSStype$RSQp1,na.rm=T),
         median(myRSStype$RSQ0,na.rm=T),median(myRSStype$RSQn1,na.rm=T),median(myRSStype$RSQn2,na.rm=T),median(myRSStype$RSQn3,na.rm=T),
         median(myRSStype$RSQn4,na.rm=T),median(myRSStype$RSQn5,na.rm=T),median(myRSStype$RSQn6,na.rm=T),median(myRSStype$RSQn7,na.rm=T),
         median(myRSStype$RSQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Risk (Median)",
       x="Quarters(Q(0) is the RSS quarter,   control firms in dotted line )", y = "Risk")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))

myControl <- myL5L5
myRSStype <- myL5H5
############################### RSS  ################
control <- c(median(myControl$RSQp8,na.rm=T),median(myControl$RSQp7,na.rm=T),median(myControl$RSQp6,na.rm=T),median(myControl$RSQp5,na.rm=T),
             median(myControl$RSQp4,na.rm=T),median(myControl$RSQp3,na.rm=T),median(myControl$RSQp2,na.rm=T),median(myControl$RSQp1,na.rm=T),
             median(myControl$RSQ0,na.rm=T),median(myControl$RSQn1,na.rm=T),median(myControl$RSQn2,na.rm=T),median(myControl$RSQn3,na.rm=T),
             median(myControl$RSQn4,na.rm=T),median(myControl$RSQn5,na.rm=T),median(myControl$RSQn6,na.rm=T),median(myControl$RSQn7,na.rm=T),
             median(myControl$RSQn8,na.rm=T))
rss <- c(median(myRSStype$RSQp8,na.rm=T),median(myRSStype$RSQp7,na.rm=T),median(myRSStype$RSQp6,na.rm=T),median(myRSStype$RSQp5,na.rm=T),
         median(myRSStype$RSQp4,na.rm=T),median(myRSStype$RSQp3,na.rm=T),median(myRSStype$RSQp2,na.rm=T),median(myRSStype$RSQp1,na.rm=T),
         median(myRSStype$RSQ0,na.rm=T),median(myRSStype$RSQn1,na.rm=T),median(myRSStype$RSQn2,na.rm=T),median(myRSStype$RSQn3,na.rm=T),
         median(myRSStype$RSQn4,na.rm=T),median(myRSStype$RSQn5,na.rm=T),median(myRSStype$RSQn6,na.rm=T),median(myRSStype$RSQn7,na.rm=T),
         median(myRSStype$RSQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Risk (Median)",
       x="Quarters(Q(0) is the RSS quarter,   L5L5 firms in dotted line )", y = "Risk")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)) 

################################### Illiquidity ##################################

myControl <- myCTL
myRSStype <- myRSS
############################### RSS  ################
control <- c(median(myControl$IQQp8,na.rm=T),median(myControl$IQQp7,na.rm=T),median(myControl$IQQp6,na.rm=T),median(myControl$IQQp5,na.rm=T),
             median(myControl$IQQp4,na.rm=T),median(myControl$IQQp3,na.rm=T),median(myControl$IQQp2,na.rm=T),median(myControl$IQQp1,na.rm=T),
             median(myControl$IQQ0,na.rm=T),median(myControl$IQQn1,na.rm=T),median(myControl$IQQn2,na.rm=T),median(myControl$IQQn3,na.rm=T),
             median(myControl$IQQn4,na.rm=T),median(myControl$IQQn5,na.rm=T),median(myControl$IQQn6,na.rm=T),median(myControl$IQQn7,na.rm=T),
             median(myControl$IQQn8,na.rm=T))
rss <- c(median(myRSStype$IQQp8,na.rm=T),median(myRSStype$IQQp7,na.rm=T),median(myRSStype$IQQp6,na.rm=T),median(myRSStype$IQQp5,na.rm=T),
         median(myRSStype$IQQp4,na.rm=T),median(myRSStype$IQQp3,na.rm=T),median(myRSStype$IQQp2,na.rm=T),median(myRSStype$IQQp1,na.rm=T),
         median(myRSStype$IQQ0,na.rm=T),median(myRSStype$IQQn1,na.rm=T),median(myRSStype$IQQn2,na.rm=T),median(myRSStype$IQQn3,na.rm=T),
         median(myRSStype$IQQn4,na.rm=T),median(myRSStype$IQQn5,na.rm=T),median(myRSStype$IQQn6,na.rm=T),median(myRSStype$IQQn7,na.rm=T),
         median(myRSStype$IQQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="RSS - Illiquidity (Median)",
       x="Quarters(Q(0) is the RSS quarter,   control firms in dotted line )", y = "Illiquidity")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)) 

myControl <- myL5L5
myRSStype <- myL5H5
############################### RSS  ################
control <- c(median(myControl$IQQp8,na.rm=T),median(myControl$IQQp7,na.rm=T),median(myControl$IQQp6,na.rm=T),median(myControl$IQQp5,na.rm=T),
             median(myControl$IQQp4,na.rm=T),median(myControl$IQQp3,na.rm=T),median(myControl$IQQp2,na.rm=T),median(myControl$IQQp1,na.rm=T),
             median(myControl$IQQ0,na.rm=T),median(myControl$IQQn1,na.rm=T),median(myControl$IQQn2,na.rm=T),median(myControl$IQQn3,na.rm=T),
             median(myControl$IQQn4,na.rm=T),median(myControl$IQQn5,na.rm=T),median(myControl$IQQn6,na.rm=T),median(myControl$IQQn7,na.rm=T),
             median(myControl$IQQn8,na.rm=T))
rss <- c(median(myRSStype$IQQp8,na.rm=T),median(myRSStype$IQQp7,na.rm=T),median(myRSStype$IQQp6,na.rm=T),median(myRSStype$IQQp5,na.rm=T),
         median(myRSStype$IQQp4,na.rm=T),median(myRSStype$IQQp3,na.rm=T),median(myRSStype$IQQp2,na.rm=T),median(myRSStype$IQQp1,na.rm=T),
         median(myRSStype$IQQ0,na.rm=T),median(myRSStype$IQQn1,na.rm=T),median(myRSStype$IQQn2,na.rm=T),median(myRSStype$IQQn3,na.rm=T),
         median(myRSStype$IQQn4,na.rm=T),median(myRSStype$IQQn5,na.rm=T),median(myRSStype$IQQn6,na.rm=T),median(myRSStype$IQQn7,na.rm=T),
         median(myRSStype$IQQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)
p <- ggplot(gdata, aes(x=x1, y=control)) + 
  geom_line( linetype="dotdash" )+geom_line(aes(x=x1,y=rss),color="red") +
  labs(title="L5H5 - Illiquidity (Median)",
       x="Quarters (Q(0) is the RSS quarter,  L5L5 firms in dotted line )", y = "Illiquidity")
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))
########################## plot multiple lines on different scale ####################

myControl <- myRSS
myRSStype <- myRSS


a<-which(myRSStype$SHQp8 == 0)
a<-which(myRSStype$SHQp7 == 0)
a<-which(myRSStype$SHQp6 == 0)
a<-which(myRSStype$SHQp5 == 0)
a<-which(myRSStype$SHQp4 == 0)
a<-which(myRSStype$SHQp3 == 0)
a<-which(myRSStype$SHQp2 == 0)
a<-which(myRSStype$SHQp1 == 0)
a<-which(myRSStype$SHQ0 == 0)
a<-which(myRSStype$SHQn1 == 0)
a<-which(myRSStype$SHQn2 == 0)
a<-which(myRSStype$SHQn3 == 0)
a<-which(myRSStype$SHQn4 == 0)
a<-which(myRSStype$SHQn5 == 0)
a<-which(myRSStype$SHQn6 == 0)
a<-which(myRSStype$SHQn7 == 0)
a<-which(myRSStype$SHQn8 == 0)

rss <- c(median(myRSStype$SHQp8,na.rm=T),median(myRSStype$SHQp7,na.rm=T),median(myRSStype$SHQp6,na.rm=T),median(myRSStype$SHQp5,na.rm=T),
         median(myRSStype$SHQp4,na.rm=T),median(myRSStype$SHQp3,na.rm=T),median(myRSStype$SHQp2,na.rm=T),median(myRSStype$SHQp1,na.rm=T),
         median(myRSStype$SHQ0,na.rm=T),median(myRSStype$SHQn1,na.rm=T),median(myRSStype$SHQn2,na.rm=T),median(myRSStype$SHQn3,na.rm=T),
         median(myRSStype$SHQn4,na.rm=T),median(myRSStype$SHQn5,na.rm=T),median(myRSStype$SHQn6,na.rm=T),median(myRSStype$SHQn7,na.rm=T),
         median(myRSStype$SHQn8,na.rm=T))


############################### percentage insiders holding and turnover ################
AUC(x =c(-6,-5,-4,-3,-2,-1),y = c(mean(myRSStype$TOQp6,na.rm=T),mean(myRSStype$TOQp5,na.rm=T),
                                  mean(myRSStype$TOQp4,na.rm=T),mean(myRSStype$TOQp3,na.rm=T),
                                  mean(myRSStype$TOQp2,na.rm=T),mean(myRSStype$TOQp1,na.rm=T)),method="spline")

control <- c(median(myRSStype$TOQp8,na.rm=T),median(myRSStype$TOQp7,na.rm=T),median(myRSStype$TOQp6,na.rm=T),median(myRSStype$TOQp5,na.rm=T),
         median(myRSStype$TOQp4,na.rm=T),median(myRSStype$TOQp3,na.rm=T),median(myRSStype$TOQp2,na.rm=T),median(myRSStype$TOQp1,na.rm=T),
         median(myRSStype$TOQ0,na.rm=T),median(myRSStype$TOQn1,na.rm=T),median(myRSStype$TOQn2,na.rm=T),median(myRSStype$TOQn3,na.rm=T),
         median(myRSStype$TOQn4,na.rm=T),median(myRSStype$TOQn5,na.rm=T),median(myRSStype$TOQn6,na.rm=T),median(myRSStype$TOQn7,na.rm=T),
         median(myRSStype$TOQn8,na.rm=T))

rss <- c(median(myRSStype$SHQp8,na.rm=T),median(myRSStype$SHQp7,na.rm=T),median(myRSStype$SHQp6,na.rm=T),median(myRSStype$SHQp5,na.rm=T),
         median(myRSStype$SHQp4,na.rm=T),median(myRSStype$SHQp3,na.rm=T),median(myRSStype$SHQp2,na.rm=T),median(myRSStype$SHQp1,na.rm=T),
         median(myRSStype$SHQ0,na.rm=T),median(myRSStype$SHQn1,na.rm=T),median(myRSStype$SHQn2,na.rm=T),median(myRSStype$SHQn3,na.rm=T),
         median(myRSStype$SHQn4,na.rm=T),median(myRSStype$SHQn5,na.rm=T),median(myRSStype$SHQn6,na.rm=T),median(myRSStype$SHQn7,na.rm=T),
         median(myRSStype$SHQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)


p <- ggplot(gdata, aes(x = x1))
p <- p + geom_line(aes(y = rss,colour="insider holdings"),linetype="solid") 

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = control/20,colour="turnover"),linetype="dotdash")

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
#breaks=c(4,5,6,7),labels=c("4", "5", "6","7"),
p <- p + scale_y_continuous(  sec.axis = sec_axis(~.*20, name = "turnover"))

# modifying colours and theme options
p<- p + scale_colour_manual(values = c("blue","red"))

#p<- p+ scale_linetype_manual(values = c("solid","dotdash"))
p <- p + labs(title="RSS stocks - Insiders Holding and Turnover  (Median)",y = "insiders holdings[%]",
              x = "Quarters (Q(0) is the RSS quarter,  turnover is in dotted line)",
               colour= "Parameter")
p <- p + theme(legend.position = c(0.2, 0.9))
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))

############################### percentage insiders quarterly abnormal return  ################
AUC(x =c(mean(myRSStype$ARQp6,na.rm=T),mean(myRSStype$ARQp5,na.rm=T),
         mean(myRSStype$ARQp4,na.rm=T),mean(myRSStype$ARQp3,na.rm=T),
         mean(myRSStype$ARQp2,na.rm=T),mean(myRSStype$ARQp1,na.rm=T)),
    y = c(mean(myRSStype$SHQp6,na.rm=T),mean(myRSStype$SHQp5,na.rm=T),
          mean(myRSStype$SHQp4,na.rm=T),mean(myRSStype$SHQp3,na.rm=T),
          mean(myRSStype$SHQp2,na.rm=T),mean(myRSStype$SHQp1,na.rm=T)),method="spline")

AUC(x =c(mean(myRSStype$ARQ0,na.rm=T),mean(myRSStype$ARQn1,na.rm=T),
         mean(myRSStype$ARQn2,na.rm=T),mean(myRSStype$ARQn3,na.rm=T)),
    y = c(mean(myRSStype$SHQ0,na.rm=T),mean(myRSStype$SHQn1,na.rm=T),
          mean(myRSStype$SHQn2,na.rm=T),mean(myRSStype$SHQn3,na.rm=T)),method="spline")

control <- c(median(myRSStype$ARQp8,na.rm=T),median(myRSStype$ARQp7,na.rm=T),median(myRSStype$ARQp6,na.rm=T),median(myRSStype$ARQp5,na.rm=T),
             median(myRSStype$ARQp4,na.rm=T),median(myRSStype$ARQp3,na.rm=T),median(myRSStype$ARQp2,na.rm=T),median(myRSStype$ARQp1,na.rm=T),
             median(myRSStype$ARQ0,na.rm=T),median(myRSStype$ARQn1,na.rm=T),median(myRSStype$ARQn2,na.rm=T),median(myRSStype$ARQn3,na.rm=T),
             median(myRSStype$ARQn4,na.rm=T),median(myRSStype$ARQn5,na.rm=T),median(myRSStype$ARQn6,na.rm=T),median(myRSStype$ARQn7,na.rm=T),
             median(myRSStype$ARQn8,na.rm=T))

rss <- c(median(myRSStype$SHQp8,na.rm=T),median(myRSStype$SHQp7,na.rm=T),median(myRSStype$SHQp6,na.rm=T),median(myRSStype$SHQp5,na.rm=T),
         median(myRSStype$SHQp4,na.rm=T),median(myRSStype$SHQp3,na.rm=T),median(myRSStype$SHQp2,na.rm=T),median(myRSStype$SHQp1,na.rm=T),
         median(myRSStype$SHQ0,na.rm=T),median(myRSStype$SHQn1,na.rm=T),median(myRSStype$SHQn2,na.rm=T),median(myRSStype$SHQn3,na.rm=T),
         median(myRSStype$SHQn4,na.rm=T),median(myRSStype$SHQn5,na.rm=T),median(myRSStype$SHQn6,na.rm=T),median(myRSStype$SHQn7,na.rm=T),
         median(myRSStype$SHQn8,na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)


p <- ggplot(gdata, aes(x = x1))
p <- p + geom_line(aes(y = rss-0.3,colour="insider holdings"),linetype="solid") 

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = control/50,colour="quarterly abnormal return"),linetype="dotdash")

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
# breaks=c(-0.4,-0.3,-0.2,-0.1),labels=c("-0.1", "0", "0.1","0.2"),
p <- p + scale_y_continuous( breaks=c(-0.4,-0.3,-0.2,-0.1),labels=c("-0.1", "0", "0.1","0.2"),sec.axis = sec_axis(~.*50, name = "Quarterly Abnormal Return"))

# modifying colours and theme options
p<- p + scale_colour_manual(values = c("blue","red"))
#p<- p + scale_linetype_manual("Parameters",values=c("insider holdings"=2,"quarterly abnormal return"=4))

#p<- p+ scale_linetype_manual(values = c("solid","dotdash"))
p <- p + labs(title="RSS stocks - Insiders Holding and Quarterly abnormal return  (Median)",y = "insiders holdings[%]",
              x = "Quarters (Q(0) is the RSS quarter,  Quarterly abnormal return is in dotted line)",
              colour = "Legend")
p <- p + theme(legend.position = c(0.2, 0.9))
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))

############################### Winsorize percentage insiders quarterly abnormal return  ################





control <- c(median(Winsorize(myRSStype$ARQp8),na.rm=T),median(Winsorize(myRSStype$ARQp7),na.rm=T),median(Winsorize(myRSStype$ARQp6),na.rm=T),median(Winsorize(myRSStype$ARQp5),na.rm=T),
             median(Winsorize(myRSStype$ARQp4),na.rm=T),median(Winsorize(myRSStype$ARQp3),na.rm=T),median(Winsorize(myRSStype$ARQp2),na.rm=T),median(Winsorize(myRSStype$ARQp1),na.rm=T),
             median(Winsorize(myRSStype$ARQ0),na.rm=T),median(Winsorize(myRSStype$ARQn1),na.rm=T),median(Winsorize(myRSStype$ARQn2),na.rm=T),median(Winsorize(myRSStype$ARQn3),na.rm=T),
             median(Winsorize(myRSStype$ARQn4),na.rm=T),median(Winsorize(myRSStype$ARQn5),na.rm=T),median(Winsorize(myRSStype$ARQn6),na.rm=T),median(Winsorize(myRSStype$ARQn7),na.rm=T),
             median(Winsorize(myRSStype$ARQn8),na.rm=T))

rss <- c(median(Winsorize(myRSStype$SHQp8),na.rm=T),median(Winsorize(myRSStype$SHQp7),na.rm=T),median(Winsorize(myRSStype$SHQp6),na.rm=T),median(Winsorize(myRSStype$SHQp5),na.rm=T),
         median(Winsorize(myRSStype$SHQp4),na.rm=T),median(Winsorize(myRSStype$SHQp3),na.rm=T),median(Winsorize(myRSStype$SHQp2),na.rm=T),median(Winsorize(myRSStype$SHQp1),na.rm=T),
         median(Winsorize(myRSStype$SHQ0),na.rm=T),median(Winsorize(myRSStype$SHQn1),na.rm=T),median(Winsorize(myRSStype$SHQn2),na.rm=T),median(Winsorize(myRSStype$SHQn3),na.rm=T),
         median(Winsorize(myRSStype$SHQn4),na.rm=T),median(Winsorize(myRSStype$SHQn5),na.rm=T),median(Winsorize(myRSStype$SHQn6),na.rm=T),median(Winsorize(myRSStype$SHQn7),na.rm=T),
         median(Winsorize(myRSStype$SHQn8),na.rm=T))



x1<-c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8)
#x1=c("-8","-7","-6","-5","-5","-3","-2","-1","0","1","2","3","4","5","6","7","8")
gdata<- data.frame(x1,control,rss)


p <- ggplot(gdata, aes(x = x1))
p <- p + geom_line(aes(y = rss-0.3,colour="insider holdings"),linetype="solid") 

# adding the relative humidity data, transformed to match roughly the range of the temperature
p <- p + geom_line(aes(y = control/50,colour="quarterly abnormal return"),linetype="dotdash")

# now adding the secondary axis, following the example in the help file ?scale_y_continuous
# and, very important, reverting the above transformation
# breaks=c(-0.4,-0.3,-0.2,-0.1),labels=c("-0.1", "0", "0.1","0.2"),
p <- p + scale_y_continuous( breaks=c(-0.4,-0.3,-0.2,-0.1),labels=c("-0.1", "0", "0.1","0.2"),sec.axis = sec_axis(~.*50, name = "Quarterly Abnormal Return"))

# modifying colours and theme options
p<- p + scale_colour_manual(values = c("blue","red"))
#p<- p + scale_linetype_manual("Parameters",values=c("insider holdings"=2,"quarterly abnormal return"=4))

#p<- p+ scale_linetype_manual(values = c("solid","dotdash"))
p <- p + labs(title="RSS stocks - Insiders Holding and Quarterly abnormal return  (Median)",y = "insiders holdings[%]",
              x = "Quarters (Q(0) is the RSS quarter,  Quarterly abnormal return is in dotted line)",
              colour = "Legend")
p <- p + theme(legend.position = c(0.2, 0.9))
p  + scale_x_continuous(breaks=c(-8,-7,-6,-5,-4,-3,-2,-1,-0,1,2,3,4,5,6,7,8))



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


