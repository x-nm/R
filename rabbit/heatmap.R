####################################################################################
# heatmap of DEGs
# Input:
#     correct gene set (with gene symbols match the ref)
#     gene_id2symbol.txt
#     DEG_HN/WJ.txt (significant DEGs)
#     normValue_HN/WJ.txt
# Output:
#     corr plot of logFC
#     heatmap of logFC
#     vari%
#     heatmap of normValue
# Usage:
#     Rscript heatmap.R KG keyGeneList.txt
# 2016.11.3 by xnm
#####################################################################################

# setwd("D:\\Files\\Study\\交大\\课题\\16.10.27\\DEG\\heatmap\\input")
setwd("D:\\Files\\Study\\交大\\课题\\16.10.27\\DEG\\heatmap\\output")
options(stringsAsFactors = FALSE);

# Args <- commandArgs(trailingOnly = TRUE)
# lable <- Args[1]
# FILE <- Args[2]

# TEST
lable <- "KG"
FILE <- "keyGeneList.txt"

#####################################################################################


# GENE LIST (用read.table读取所以用的时候要像矩阵一样用才行...)
GENELIST <- read.table(FILE, header = F)
names(GENELIST)<-"GENE"
GENELIST <- GENELIST$GENE #变成向量
geneNum <- length(GENELIST)

# input data
geneId2Sym <- read.csv("gene_id2symbol.txt", header = F, sep = "\t")
names(geneId2Sym)<-c("ID","GeneSymbol")
DEG1 <- read.csv("DEG_WJ.txt",header = T,sep = "\t")# WJ as 1, HN as 2
DEG1 <- DEG1[,c(2,7)]
DEG2 <- read.csv("DEG_HN.txt",header = T,sep = "\t")
DEG2 <- DEG2[,c(2,7)]
NORM1 <- read.csv("normValue_WJ.txt", header = T, sep = "\t")
NORM2 <- read.csv("normValue_HN.txt",header = T, sep = "\t")
# VST1 <- read.csv("vstValue_WJ.txt", header = T, sep = "\t")
# VST2 <- read.csv("vstValue_HN.txt", header = T, sep = "\t")

# PACKAGES  
library("RColorBrewer")
library("gplots")
library("WGCNA")

# COLORS
hmcol <- colorRampPalette(brewer.pal(9, "GnBu"))(100)



###Functions#########################################################################
# get index
indexInDEG1 <- match(GENELIST,DEG1$sy) 
indexInDEG2 <- match(GENELIST,DEG2$sy)
index <- match(GENELIST,geneId2Sym$GeneSymbol)
geneID <- geneId2Sym[index,1]
indexInNorm1 <- match(geneID,row.names(NORM1)) # same as index in VST
indexInNorm2 <- match(geneID,row.names(NORM2))

# vari%
naNum1 <- sum(is.na(indexInDEG1))
varNum1 <- geneNum - naNum1
naNum2 <- sum(is.na(indexInDEG2))
varNum2 <- geneNum - naNum2
variPct1 <- varNum1/geneNum
variPct2 <- varNum2/geneNum
variNum <- rbind(c(varNum1,variPct1),c(varNum2,variPct2))
row.names(variNum) <- c("WJ","HN")
colnames(variNum) <- c("variNum","percent") 
write.table(variNum, paste("vari_",lable,".txt",sep = ""),quote = F, sep = "\t")

# combine matrix
lgFCMat <- cbind(DEG1[indexInDEG1,1],DEG2[indexInDEG2,1])
colnames(lgFCMat) <- c("WJ","HN")
row.names(lgFCMat) <- GENELIST
lgFCMat[is.na(lgFCMat)]<-0

normMat <- cbind(NORM1[indexInNorm1,],NORM2[indexInNorm2,])
row.names(normMat) <- GENELIST
normMat[is.na(normMat)]<-0
normMat<-as.matrix(normMat)
normMatLg <- log2(normMat+1)

# vstMat <- cbind(VST1[indexInNorm1,], VST2[indexInNorm2,])
# row.names(vstMat) <- GENELIST
# vstMat[is.na(vstMat)]<-0
# vstMat <- as.matrix(vstMat)

# plot.1 corr plot of logFC ##########################################################
title <- paste("corr_",lable,"_logFC", sep = "")
bmp(paste(title,".bmp",sep=""), width = 720, height = 720)
plot(lgFCMat[,1], lgFCMat[,2], 
     main = paste("Correlation of Expression Changes of HW in ",lable, sep = "") , 
     xlab = "WHHL log(FC)",
     ylab = "NZW-HC log(FC)",
     col = "blue")

# lines(lgFCMat[,1], lgFCMat[,2],col = "red")

cor(lgFCMat[,1], lgFCMat[,2], method = "pearson")
dev.off()



# plot.2 heatmap of logFC ############################################################
title <- paste("HM_",lable,"_logFC", sep = "")
bmp(paste(title,".bmp",sep=""), width = 720, height = 1024)
labeledHeatmap(Matrix = lgFCMat,
               xLabels = colnames(lgFCMat),
               yLabels = row.names(lgFCMat),
               ySymbols = row.names(lgFCMat),
               colorLabels = FALSE,
               colors = blueWhiteRed(100),
               setStdMargins = FALSE,
               cex.lab = 0.9,
               textMatrix = round(lgFCMat,4),
               cex.text = 0.5,
               zlim = c(-12,12),
               main = paste("Heatmap of logFC in ",lable, sep = ""))
dev.off()

# plot.3 heatmap of log(normValue+1) #################################################
title <- paste("HM_",lable,"_normLg", sep = "")
bmp(paste(title,".bmp",sep=""), width = 840, height = 840)
labeledHeatmap(Matrix = normMatLg,
               xLabels = colnames(normMatLg),
               yLabels = row.names(normMatLg),
               ySymbols = row.names(normMatLg),
               colorLabels = T,
               colors = blueWhiteRed(100),
               setStdMargins = F,
               cex.lab.y = 0.9,
               cex.lab.x = 0.9,
               zlim = c(-18,18),
               main = paste("Heatmap of log(Normalized Counts+1) in ",lable, sep = ""))
dev.off()

# # plot.4 heatmap of vst transformation #################################################
# title <- paste("HM_",lable,"_vst", sep = "")
# bmp(paste(title,".bmp",sep=""), width = 840, height = 840)
# labeledHeatmap(Matrix = vstMat,
#                xLabels = colnames(vstMat),
#                yLabels = row.names(vstMat),
#                ySymbols = row.names(vstMat),
#                colorLabels = T,
#                colors = blueWhiteRed(100),
#                setStdMargins = F,
#                cex.lab.y = 0.9,
#                cex.lab.x = 0.9,
#                zlim = c(-18,18),
#                main = paste("Heatmap of vst in ",lable, sep = ""))
# dev.off()


######################################################################################
# heatmap.2(lgFCMat,
#           col = hmcol,
#           Rowv = FALSE, Colv = FALSE, scale="none",
#           dendrogram="none", trace="none", margin=c(6,8),
#           main = NULL)

# FOR NORM VALUE
# labeledHeatmap(Matrix = normMat,
#                xLabels = colnames(normMat),
#                yLabels = row.names(normMat),
#                ySymbols = row.names(normMat),
#                colorLabels = T,
#                colors = hmcol,
#                #textMatrix = textMatrix,
#                setStdMargins = F,
#                cex.text = 0.3,
#                zlim = c(-5000,5000),
#                main = paste("Heatmap of Normalized Counts"))

# title <- paste("MA-plot-", sp1, sp2, sep = "")
# bmp(paste(title,".bmp",sep=""), width = 720, height = 720)
# plotMA(res,main=title,ylim=c(-10,10))
# dev.off()
