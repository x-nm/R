# uasge: Rscript drawUpDown.R JW_WHHL_Arota
# no .txt
# 2016.10.8 by xnm
Args <- commandArgs(trailingOnly = TRUE)
FILENAME=Args[1]
dat <- read.table(paste(FILENAME,".txt",sep = ""), header = T)
dat <- dat[dat$padj < 0.01,]
dat <- na.omit(dat)
dim(dat)
dat_up <- dat[dat$log2FoldChange > 0,]
dim(dat_up)
dat_down <- dat[dat$log2FoldChange < 0,]
dim(dat_down)
write.table(dat_up,paste("deseq_",FILENAME,"_up.txt",sep=""),sep = "\t", quote = F)
write.table(dat_down,paste("deseq_",FILENAME,"_down.txt",sep=""),sep = "\t", quote = F)