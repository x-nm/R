###################################################################
# WCGNA process for rabbit data
# Usage: Rscript wcgna.R sample
# 2016.10.25 by xnm
###################################################################


setwd("D:\\Files\\Study\\交大\\课题\\16.9.9\\DEG_INPUT\\input")

getwd();
library(WGCNA);
options(stringsAsFactors = FALSE);

# #######################  01   #####################################
HNData = read.table("vstValue_HN.txt", header = T, row.names = 1)
WData = read.table("vstValue_W.txt", header = T, row.names = 1)

datExpr0 = as.data.frame(t(WData))

gsg = goodSamplesGenes(datExpr0, verbose = 3); # 检测数据缺失情况等
gsg$allOK


if (!gsg$allOK)
{
  # Optionally, print the gene and sample names that were removed:
  if (sum(!gsg$goodGenes)>0)
    printFlush(paste("Removing genes:", paste(names(datExpr0)[!gsg$goodGenes], collapse = ", ")));
  if (sum(!gsg$goodSamples)>0)
    printFlush(paste("Removing samples:", paste(rownames(datExpr0)[!gsg$goodSamples], collapse = ", ")));
  # Remove the offending genes and samples from the data:
  datExpr0 = datExpr0[gsg$goodSamples, gsg$goodGenes]
}


# Sample clustering to detect outliers
sampleTree = hclust(dist(datExpr0), method = "average");
# Plot the sample tree: Open a graphic output window of size 12 by 9 inches
# The user should change the dimensions if the window is too large or too small.
sizeGrWindow(12,9)
#pdf(file = "Plots/sampleClustering.pdf", width = 12, height = 9);
par(cex = 0.6);
par(mar = c(0,4,2,0))
plot(sampleTree, main = "Sample clustering to detect outliers", sub="", xlab="", cex.lab = 1.5,
     cex.axis = 1.5, cex.main = 2)

# # Plot a line to show the cut
# abline(h = 100, col = "red"); # change the defalt 15 to 100
# # Determine cluster under the line
# clust = cutreeStatic(sampleTree, cutHeight = 100, minSize = 10)
# table(clust)
# # clust 1 contains the samples we want to keep.
# keepSamples = (clust==1) # index
# datExpr = datExpr0[keepSamples, ]
# nGenes = ncol(datExpr)
# nSamples = nrow(datExpr)

datExpr = datExpr0
nGenes = ncol(datExpr)
nSamples = nrow(datExpr)


datTraits = read.table("HNTraits.txt", header = T, row.names = 1)


# Re-cluster samples
sampleTree2 = hclust(dist(datExpr), method = "average")
# Convert traits to a color representation: white means low, red means high, grey means missing entry
traitColors = numbers2colors(datTraits, signed = FALSE);
# Plot the sample dendrogram and the colors underneath.
plotDendroAndColors(sampleTree2, traitColors,
                    groupLabels = names(datTraits),
                    main = "Sample dendrogram and trait heatmap")

save(datExpr, datTraits, file = "HN_dataInput.RData")
save(datExpr, file = "W_dataInput.RData")
#######################  END1   ####################################

#######################  02   #####################################
# enableWGCNAThreads()
# lnames = load(file = "HN_dataInput.RData")

######## CHOOSE SOFT THRES ##########################################
# Choose a set of soft-thresholding powers
# powers = c(c(1:10), seq(from = 12, to=20, by=2))
powers = c(seq(from = 12, to=36, by=2))
# Call the network topology analysis function
sft = pickSoftThreshold(datExpr, powerVector = powers, verbose = 5)
# Plot the results:
sizeGrWindow(9, 5)
par(mfrow = c(1,2));
cex1 = 0.9;
# Scale-free topology fit index as a function of the soft-thresholding power
plot(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     xlab="Soft Threshold (power)",ylab="Scale Free Topology Model Fit,signed R^2",type="n",
     main = paste("Scale independence"));
text(sft$fitIndices[,1], -sign(sft$fitIndices[,3])*sft$fitIndices[,2],
     labels=powers,cex=cex1,col="red");
# this line corresponds to using an R^2 cut-off of h
abline(h=0.90,col="red")
# Mean connectivity as a function of the soft-thresholding power
plot(sft$fitIndices[,1], sft$fitIndices[,5],
     xlab="Soft Threshold (power)",ylab="Mean Connectivity", type="n",
     main = paste("Mean connectivity"))
text(sft$fitIndices[,1], sft$fitIndices[,5], labels=powers, cex=cex1,col="red")
######## CHOOSE SOFT THRES END ##########################################






# net = blockwiseModules(datExpr, power = 6,
#                        TOMType = "unsigned", minModuleSize = 30,
#                        reassignThreshold = 0, mergeCutHeight = 0.25,
#                        numericLabels = TRUE, pamRespectsDendro = FALSE,
#                        saveTOMs = TRUE,
#                        saveTOMFileBase = "femaleMouseTOM", 
#                        verbose = 3)

# # open a graphics window
# sizeGrWindow(12, 9)
# # Convert labels to colors for plotting
# mergedColors = labels2colors(net$colors)
# # Plot the dendrogram and the module colors underneath
# plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
#                     "Module colors",
#                     dendroLabels = FALSE, hang = 0.03,
#                     addGuide = TRUE, guideHang = 0.05)
# 
# 
# 
# moduleLabels = net$colors
# moduleColors = labels2colors(net$colors)
# MEs = net$MEs;
# geneTree = net$dendrograms[[1]];
# save(MEs, moduleLabels, moduleColors, geneTree, 
#      file = "FemaleLiver-02-networkConstruction-auto.RData")