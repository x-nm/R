# 2016.10.27 BY xnm
# INPUT: "calPCC_HN.RData"

# 1. CAL PCC, IN calPCC_linux.R
# save(geneNum, spNumCtl, spNumExpr, dataExpr, dataCtl, uvPairs, file = paste("calPCC_",label, ".RData",sep=""))


################################################
label <- "HN"
FILE <- paste("calPCC_",label, ".RData",sep="")

lname <- load(FILE)
lname
################################################

# 2. Construct edge feature matrix

# DATA:geneNum ROW, spNum COL. 

uvNum <- length(row.names(uvPairs))

matExpr <- c()
matCtl <- c()

k1 <- sqrt((spNumCtl-1)/spNumCtl)
k2 <- sqrt((spNumExpr-1)/spNumExpr)

for (i in 1:uvNum){
  u<- uvPairs[i,1]
  v<- uvPairs[i,2]
  uExpr <- as.numeric(dataExpr[u,])
  uCtl <- as.numeric(dataCtl[u,])
  vExpr <- as.numeric(dataExpr[v,])
  vCtl <- as.numeric(dataCtl[v,])
  uEM <- mean(uExpr, na.rm = T)
  uCM <- mean(uCtl, na.rm = T)
  vEM <- mean(vExpr, na.rm = T)
  vCM <- mean(vCtl, na.rm = T)
  uESd <- sd(uExpr, na.rm = T)
  uCSd <- sd(uCtl, na.rm = T)
  vESd <- sd(vExpr, na.rm = T)
  vCSd <- sd(vCtl, na.rm = T)
  
  ftE4E <- ((uExpr - uEM)*(vExpr - vEM))/(K1 * K1 * uESd * vESd) #feature value for Expr, cal from expr formula
  ftC4E <- ((uExpr - uCM)*(vExpr - vCM))/(K2 * K2 * uCSd * vCSd)
  ftE4E <- cbind(uvPairs[i], ftE4E)
  ftC4E <- cbind(uvPairs[i], ftC4E)
  matExpr <- rbind(matExpr, ftE4E, ftC4E)
  ftE4C <- ((uCtl - uEM)*(vCtl - vEM))/(K1 * K1 * uESd * vESd)
  frC4C<- ((uCtl - uCM)*(vCtl - vCM))/(K2 * K2 * uCSd * vCSd)
  ftE4C <- cbind(uvPairs[i], ftE4C)
  ftC4C <- cbind(uvPairs[i], ftC4C)
  matCtl <- rbind(matCtl, ftE4C, ftC4C)
}

save(matExpr, matCtl, "edgeFts.RData")

