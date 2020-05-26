setwd("C:/Users/keibr/SynologyDrive/Books/polyu/myR/B2")

shc = read.csv("SHC.csv", header = T)
szc = read.csv("SZC.csv", header = T)
lshc <- shc$code
lszc <- szc$code
#pdataF<-pdataF1
pdataF = read.csv("mAllDID.csv",header = T)
pdataF$Date <- as.character(pdataF$Date)
pdataF$Date <- as.Date(pdataF$Date)

pdataF$shcStk <- 0
pdataF$szcStk<- 0
pdataF$chinaC <- 0

#pdataF <- pdataF[pdataF$SplitRatio <= 1,]  not working
BloombergMissedDate <- c(as.Date("2016-10-21"))
pdataF$SZC <- 0  #SZ Connect
pdataF$SHC <- 0  #SH Connect
pdataF$BDPC <- 0  #Board Director Policy Changed
pdataF$GEM <- 0 #GEM board
SHCDate <- as.Date("2014-11-17")
SHZDate <- as.Date("2016-12-05")
#BDPCDate <- as.Date("2012-12-31")
BDPCDate <- as.Date("2012-06-30") # RSS decidsion can be made before the original effective day, so ajust 6 month earlier

i = 1L
j = 1L

for(j in 1L:nrow(pdataF)){
  code = as.integer(gsub("X","",pdataF[j,"Code"]))
  print(as.character(j))
  if(code %in% lshc) pdataF[j,"shcStk"] <- 1
  if(code %in% lszc) pdataF[j,"szcStk"] <- 1
  
  pdataF[j,"chinaC"] <- pdataF[j,"shcStk"]+pdataF[j,"szcStk"]
  
  if(pdataF[j,"Date"] >= SHCDate) pdataF[j,"SHC"] = 1
  if(pdataF[j,"Date"] >= SHZDate) pdataF[j,"SZC"] = 1
  if(pdataF[j,"Date"] >= BDPCDate) pdataF[j,"BDPC"] = 1
  
  if(code >=8000) pdataF[j,"GEM"] = 1
  
  
}
beep("coin")
pdataF$X <-NULL
write.csv(pdataF,file="mAllDIDDeltaChinaC.csv")
# 
