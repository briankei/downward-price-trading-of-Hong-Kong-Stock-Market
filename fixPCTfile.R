pct=read.csv("PCT_INSIDER_SHARES_OUT_Q1d.csv",header = T)
pct[,1]<-NULL
j = 7L
for(j in 1L:(nrow(pct)/2)){
  a<-which(pct[,j*2]==0)
  if(length(a) > 0){
    if(a[1]==1) a=a[-1]
    pct[a,j*2] = NA 
    print(as.character(j))
  }

  pct[,j*2]=na.locf(pct[,j*2])
}
write.csv(pct,"PCT_INSIDER_SHARES_OUT_Q1dFix.csv")
  

pct=read.csv("PCT_INSIDER_SHARES_OUT_Q2d.csv",header = T)
pct[,1]<-NULL
j = 7L
for(j in 1L:(nrow(pct)/2)){
  a<-which(pct[,j*2]==0)
  if(length(a) > 0){
    if(a[1]==1) a=a[-1]
    pct[a,j*2] = NA 
    print(as.character(j))
  }
  
  pct[,j*2]=na.locf(pct[,j*2])
}
write.csv(pct,"PCT_INSIDER_SHARES_OUT_Q2dFix.csv")

pct=read.csv("NUM_INSIDERS_OWNING_SHARES_Q1d.csv",header = T)
pct[,1]<-NULL
j = 7L
for(j in 1L:(nrow(pct)/2)){
  a<-which(pct[,j*2]==0)
  if(length(a) > 0){
    if(a[1]==1) a=a[-1]
    pct[a,j*2] = NA 
    print(as.character(j))
  }
  
  pct[,j*2]=na.locf(pct[,j*2])
}
write.csv(pct,"NUM_INSIDERS_OWNING_SHARES_Q1dFix.csv")

pct=read.csv("NUM_INSIDERS_OWNING_SHARES_Q2d.csv",header = T)
pct[,1]<-NULL
j = 7L
for(j in 1L:(nrow(pct)/2)){
  a<-which(pct[,j*2]==0)
  if(length(a) > 0){
    if(a[1]==1) a=a[-1]
    pct[a,j*2] = NA 
    print(as.character(j))
  }
  
  pct[,j*2]=na.locf(pct[,j*2])
}
write.csv(pct,"NUM_INSIDERS_OWNING_SHARES_Q2dFix.csv")

pct=read.csv("INSTIT_ONWER_1Wd.csv",header = T)
pct[,1]<-NULL
j = 7L
for(j in 1L:(nrow(pct)/2)){
  a<-which(pct[,j*2]==0)
  if(length(a) > 0){
    if(a[1]==1) a=a[-1]
    pct[a,j*2] = NA 
    print(as.character(j))
  }
  
  pct[,j*2]=na.locf(pct[,j*2])
}
write.csv(pct,"INSTIT_ONWER_1WdFIX.csv")


pct=read.csv("INSTIT_ONWER_2Wd.csv",header = T)
pct[,1]<-NULL
j = 7L
for(j in 1L:(nrow(pct)/2)){
  a<-which(pct[,j*2]==0)
  if(length(a) > 0){
    if(a[1]==1) a=a[-1]
    pct[a,j*2] = NA 
    print(as.character(j))
  }
  
  pct[,j*2]=na.locf(pct[,j*2])
}
write.csv(pct,"INSTIT_ONWER_2WdFIX.csv")


pct=read.csv("EQY_INST_PCT_SH_OUT_Q1d.csv",header = T)
pct[,1]<-NULL
j = 7L
for(j in 1L:(nrow(pct)/2)){
  a<-which(pct[,j*2]==0)
  if(length(a) > 0){
    if(a[1]==1) a=a[-1]
    pct[a,j*2] = NA 
    print(as.character(j))
  }
  
  pct[,j*2]=na.locf(pct[,j*2])
}
write.csv(pct,"EQY_INST_PCT_SH_OUT_Q1dFix.csv")

pct=read.csv("EQY_INST_PCT_SH_OUT_Q2d.csv",header = T)
pct[,1]<-NULL
j = 7L
for(j in 1L:(nrow(pct)/2)){
  a<-which(pct[,j*2]==0)
  if(length(a) > 0){
    if(a[1]==1) a=a[-1]
    pct[a,j*2] = NA 
    print(as.character(j))
  }
  
  pct[,j*2]=na.locf(pct[,j*2])
}
write.csv(pct,"EQY_INST_PCT_SH_OUT_Q2dFix.csv")

