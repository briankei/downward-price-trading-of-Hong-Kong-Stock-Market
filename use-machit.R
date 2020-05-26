m.out = matchit(RSS ~ pm11Return + Book2Market + MarketCapM + 
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
