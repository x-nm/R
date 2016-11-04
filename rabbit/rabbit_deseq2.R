#################################
# DESeq2 process for rabbit data
# 2016.05.19 by xnm
#################################

setwd("D:\\Files\\Study\\½»´ó\\¿ÎÌâ\\16.1.13\\extract_data_from_raw\\data")

#################################
# define input file
sample <-"aorta_JW_WHHL_raw_count"
info <-"JW_WHHL_sample"
title <- "JW_WHHL"

rm(list=ls(all=T))

# sample<-"aorta_NZW_HC_raw_count"
# info<-"NZW_HC_sample" 
# title <- "NZW_HC"

# sample<-"aorta_WHHL_HC_raw_count"
# info<-"WHHL_HC_sample"
# title <- "WHHL_HC"

# sample<-"aorta_JW_NZW_raw_count"
# info<-"JW_NZW_sample"
# title <- "JW_NZW"
##################################


# import data
countData <- read.table(sample,
                        header=T,
                        row.names = 1)
colData <- read.table(info,header = T,row.names = 1)

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
write.csv(as.data.frame(resOrdered),file=paste(title,"deseq2_resOrdered.csv"), quote = F)

resSig <- subset(resOrdered, padj < 0.1)
write.csv(as.data.frame(resSig),file=paste(title,"deseq2_resSig.csv"), quote = F)

resSigUp <- subset(resSig, log2FoldChange >= 0)
write.csv(as.data.frame(resSigUp),file=paste(title,"deseq2_resSigUp.csv"), quote = F)

resSigDown <- subset(resSig, log2FoldChange < 0)
write.csv(as.data.frame(resSigDown),file=paste(title,"deseq2_resSigDown.csv"), quote = F)

# MA-plot
plotMA(res,main=title,ylim=c(-3,3))




