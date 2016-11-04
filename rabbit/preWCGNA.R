###################################################################
# Construct input data for WCGNA and EDGE:
#     low counts removed; 
#     normalized by deseq.
#     VST transformed by deseq.
# Usage: Rscript preWCGNA.R 
# 2016.10.25 by xnm
###################################################################

setwd("D:\\Files\\Study\\½»´ó\\¿ÎÌâ\\16.9.9\\DEG_INPUT\\input")
library("DESeq2")

sp1 <- "W"
sp2 <- "J"
saveLabel <- "W"

singleOrPair = 1 # 1=single, 2=pair
shrunkNum = 10 # if sp of shrunkFactor size expr <= shruncNum, we remove the gene 
shrunkFactor = 3 # 3 for single, 6 for pair 

###################################################################
# Data input
###################################################################

# FOR PAIR (i.e. H+N)
if(singleOrPair==2){
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
  
  num <- length(names(sp1Data))
  sp1Col <- cbind(names(sp1Data),c(rep(1,num))) # 1 = treated
  sp2Col <- cbind(names(sp2Data),c(rep(0,num))) # 0 = CTL
  colData <- rbind(sp1Col, sp2Col) 
  colnames(colData) <- c("sampleId", "Condition")
  write.table(as.data.frame(colData), paste(saveLabel,"Traits.txt",sep = ""), row.names = F,sep = "\t", quote = F)

  dds <- DESeqDataSetFromMatrix(countData = countData,
                                colData = colData,
                                design = ~ Condition)
}

# FOR SINGLE (e.g. W)
if(singleOrPair==1){
  sp1File <- paste("raw_count_",sp1,".txt", sep="")
  countData <- read.table(sp1File,
                        header=T,
                        row.names = 1)
  num <- length(names(countData))
  colData <- cbind(names(countData),c(rep(1,num))) # 1 = treated
  colnames(colData) <- c("sampleId", "Condition")
  
  dds <- DESeqDataSetFromMatrix(countData = countData,
                                colData = colData,
                                design = ~ 1)
}


###################################################################
# DESeq2 NORMALIZATION & VST
###################################################################

dds <- estimateSizeFactors(dds)
sizeFactors(dds)

# REMOVE LOW COUNTS
ddsShrunk <- dds[rowSums(counts(dds, normalized = F)<=shrunkNum)<=shrunkFactor,]

# get normalized value
normalValue <- counts(ddsShrunk, normalized = T)
write.table(as.data.frame(normalValue),file=paste("normValue_",saveLabel, ".txt", sep = ""), sep = "\t", quote = F)

# get VST value
vsd<-varianceStabilizingTransformation(ddsShrunk)
vstValue<-assay(vsd)
write.table(as.data.frame(vstValue),file=paste("vstValue_",saveLabel, ".txt", sep = ""), sep = "\t", quote = F)



#############################################################
# selection of shrunkFactor
#############################################################
# ddsShrunk <- dds[rowSums(counts(dds, normalized = T))>shrunkFactor,]
#
#
# Num <- 10
# ddsNUM <- dds[rowSums(counts(dds, normalized = T))>Num,]
# dim(ddsNUM)
# 
# vary <-counts(ddsNUM)
# 
# index <- order(rowSums(vary),decreasing = F)
# ordCnt <- vary[index,]
# ordCnt[100:105,]
#############################################################