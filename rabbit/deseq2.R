# 2016.05.19 by xnm
# prepare independent sample files
# With tactiful R skills, theses steps can be negleted...= =

setwd("D:\\Files\\Study\\½»´ó\\¿ÎÌâ\\16.1.13\\extract_data_from_raw\\data")

# CountData <- read.table("raw_count_aorta_sample.txt",
#                       header=T,
#                       row.names = 1)
# colData <- read.table("aorta_sample.txt",header = T,row.names = 1)


#but the order is not the same,.. so I try to bind 4 count files
r_1 <-read.table("../extracted_raw_data/raw_count_JW.txt",
                 header = T,
                 row.names = 1)
r_2 <-read.table("../extracted_raw_data/raw_count_NZW.txt",
                 header = T,
                 row.names = 1)
r_3 <-read.table("../extracted_raw_data/raw_count_NZW_HC.txt",
                 header = T,
                 row.names = 1)
r_4 <-read.table("../extracted_raw_data/raw_count_WHHL.txt",
                 header = T,
                 row.names = 1)

JW_WHHL <-cbind(r_1,r_4)
NZW_HC <- cbind(r_2,r_3)
write.table(JW_WHHL,"aorta_JW_WHHL_raw_count",quote = F,sep = "\t")
write.table(NZW_HC,"aorta_NZW_HC_raw_count",quote = F,sep = "\t")

WHHL_HC <- cbind(r_3,r_4)
write.table(WHHL_HC,"aorta_WHHL_HC_raw_count",quote = F,sep = "\t")

JW_NZW <-cbind(r_1, r_2)
write.table(JW_NZW,"aorta_JW_NZW_raw_count",quote = F,sep = "\t")

# dds <- DESeqDataSetFromMatrix(countData = countData_bind,
#                               colData = colData,
#                               design = ~ condition)

