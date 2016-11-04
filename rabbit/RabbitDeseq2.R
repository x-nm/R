###################################################################
# DESeq2 process for rabbit data
# modified from 160519 version to fit into the whole process script
# Usage: Rscript RabbitDeseq2.R sample1 sample2
# Note: SigUp is compare sample1(up) to sample2 
# 2016.09.09 by xnm
###################################################################

rm(list=ls(all=T))

# ################################
# #TEST
# setwd("D:\\Files\\Study\\½»´ó\\¿ÎÌâ\\16.9.9\\DEG_INPUT\\input")
# sp1 <- "H"
# sp2 <- "N"
# ################################

# sample name & title
Args <- commandArgs(trailingOnly = TRUE)
sp1 <- Args[1] #sample 1, EXPR
sp2 <- Args[2] #sample 2, CTL
sp1File <- paste("raw_count_",sp1,".txt", sep="")
sp2File <- paste("raw_count_",sp2,".txt", sep="")

# import data
sp1Data <- read.table(sp1File,
                      header=T,
                      row.names = 1)
sp2Data <- read.table(sp2File,
                      header=T,
                      row.names = 1)
countData <- cbind(sp1Data, sp2Data)


#colData <- read.table(info,header = T,row.names = 1)
num <- length(names(sp1Data)) # a better way is...?
sp1Col <- cbind(names(sp1Data),c(rep(1,num)))
sp2Col <- cbind(names(sp2Data),c(rep(0,num)))
colData <- rbind(sp1Col, sp2Col) # whether the same format as former?
colnames(colData) <- c("id", "condition")
  
# DESeq2 analysis
library("DESeq2")
dds <- DESeqDataSetFromMatrix(countData = countData,
                              colData = colData,
                              design = ~ condition)
dds <- DESeq(dds)
res <- results(dds)
summary(res)

# order result and save to file
resOrdered <- res[order(res$padj),]
resSig <- subset(resOrdered, padj < 0.05)
resSigUp <- subset(resSig, log2FoldChange > 0)
write.table(as.data.frame(resSigUp),file=paste("DEG_",sp1, sp2, "_up.txt", sep = ""), sep = "\t", quote = F)
resSigDown <- subset(resSig, log2FoldChange < 0)
write.table(as.data.frame(resSigDown),file=paste("DEG_",sp1, sp2, "_down.txt", sep = ""), sep = "\t", quote = F)

# MA-plot
title <- paste("MA-plot-", sp1, sp2, sep = "")
bmp(paste(title,".bmp",sep=""), width = 720, height = 720)
plotMA(res,main=title,ylim=c(-10,10))
dev.off()