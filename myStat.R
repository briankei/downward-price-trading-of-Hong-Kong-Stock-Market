library(kableExtra)
pdata <- read.csv("rSplitsFfProfLevTobinQ10Clean.csv", header = T)
pdata$DeclareDate <- as.Date(as.character(pdata$DeclareDate))
pdata$LastTradeDate<- as.Date(as.character(pdata$LastTradeDate))
pdata$ExDate <- as.Date(as.character(pdata$ExDate))
nrow(pdata)
rssStocks <- pdata$Symbol
uniqueRSSstocks <- unique(rssStocks)
length(uniqueRSSstocks)
pdata$X<-NULL
pdata$ï..index<-NULL
pSummary <- summary(pdata)
write.csv(pSummary,"pSummary.csv")
write.csv(pp,"pSummary2.csv")
pStr <-str(pdata)

sd(pdata$ChangeRatio)
mycolnames <-colnames(pdata)
mySd <- c(rep(NA,27))
for(i in 1L:length(mycolnames)) mySd[i]<-sd(pdata[,i])
pp <-rbind(pSummary,mySd)
write.csv(pp,"pSummary2.csv")
pdata[which(pdata$MarketCapM> 521),"Symbol"]
