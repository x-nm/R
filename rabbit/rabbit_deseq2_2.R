#################################
# DESeq2 process_2 for rabbit data
# added heatmap module
# 2016.05.22 by xnm
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

resOrdered <- res[order(res$padj),]
resSig <- subset(resOrdered, padj < 0.1)

################################
# heatmap
################################
# heatmap of the count matrix

library("RColorBrewer")
#source("http://Bioconductor.org/biocLite.R")
#biocLite("gpplots")
library("gplots")
select <- order(rowMeans(counts(dds,normalized=TRUE)),decreasing=TRUE)[1:30]
hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)
heatmap.2(counts(dds,normalized=TRUE)[select,], col = hmcol,
          Rowv = FALSE, Colv = FALSE, scale="none",
          dendrogram="none", trace="none", margin=c(10,6))

heatmap.2(assay(rld)[select,], col = hmcol,
          Rowv = FALSE, Colv = FALSE, scale="none",
          dendrogram="none", trace="none", margin=c(10, 6))
heatmap.2(assay(vsd)[select,], col = hmcol,
          Rowv = FALSE, Colv = FALSE, scale="none",
          dendrogram="none", trace="none", margin=c(10, 6))

# Heatmap of the sample-to-sample distances
distsRL <- dist(t(assay(rld)))
mat <- as.matrix(distsRL)
rownames(mat) <- colnames(mat) <- with(colData(dds),
                                       paste(condition, type, sep=" : "))
hc <- hclust(distsRL)
heatmap.2(mat, Rowv=as.dendrogram(hc),
          symm=TRUE, trace="none",
          col = rev(hmcol), margin=c(13, 13))
