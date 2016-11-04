# 2016.10.27 BY xnm
# INPUT: "normValue_HN.txt"
# OUTPUT: "calPCC_HN.RData"
# usage: nohup Rscript calPCC_linux.R HN >report.txt 2>&1 &

# setwd("D:\\Files\\Study\\交大\\课题\\16.10.27\\WCGNA\\Data")

rm(list=ls(all=T))

#多线程

Args <- commandArgs(trailingOnly = TRUE)
label <- Args[1]
# label <- "HN"
FILE <- paste("normValue_",label,".txt", sep="")

dataAll <- read.table(FILE, header = T, row.names = 1)
spNumAll <- 8
spNumExpr <- 4 # sample number of experimental group
spNumCtl <- 4 # sample number of control
geneNum <- length(rownames(dataAll))

dataExpr <- dataAll[,1:spNumExpr] # 前4列为实验组
dataCtl <- dataAll[,(spNumExpr+1):spNumAll] # 后四列为对照组

# #TEST
# dataExpr <- dataExpr[1:10,]
# dataCtl <- dataCtl[1:10,]

delta = 0.9 # try

# 1. Calculate PCC and DE of delta
corValExpr <- cor(t(dataExpr), method = "pearson")
corValCtl <- cor(t(dataCtl), method = "pearson")

# 避免数字太小变成NA？

diffCorVal <- corValExpr - corValCtl
diffDelta <- abs(diffCorVal) > delta

# get the (u,v) pairs
uvPairs<-c() #i.e. DE of delta
count<-2
for (i in 1:geneNum){
  for (j in count:geneNum){
    if(!is.na(diffDelta[i,j])){ # to avoid error due to NA...
      if(diffDelta[i,j]=="TRUE"){
        uvPairs<-rbind(uvPairs,c(i,j))
      }
    }
  }
  count<-count+1
}
colnames(uvPairs)<-c("u","v")

dim(uvPairs)[1]

save(geneNum, spNumCtl, spNumExpr, dataExpr, dataCtl, uvPairs, file = paste("calPCC_",label, ".RData",sep=""))
