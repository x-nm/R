library(WGCNA);
options(stringsAsFactors = FALSE);
enableWGCNAThreads()
# allowWGCNAThreads() # which one?
lnames = load(file = "W_dataInput.RData")
lnames

net = blockwiseModules(datExpr0, power = 30,
                       TOMType = "unsigned", minModuleSize = 30,
                       reassignThreshold = 0, mergeCutHeight = 0.25,
                       numericLabels = TRUE, pamRespectsDendro = FALSE,
                       saveTOMs = TRUE,
                       saveTOMFileBase = "WTOM", 
                       verbose = 3,
                       maxBlockSize = 20000)

# open a graphics window
# sizeGrWindow(12, 9)
# Convert labels to colors for plotting
mergedColors = labels2colors(net$colors)
# Plot the dendrogram and the module colors underneath
title <- "W_dendrogram"
bmp(paste(title,".bmp",sep=""), width = 720, height = 720)
plotDendroAndColors(net$dendrograms[[1]], mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)
dev.off()

moduleLabels = net$colors
moduleColors = labels2colors(net$colors)
MEs = net$MEs;
geneTree = net$dendrograms[[1]];
save(MEs, moduleLabels, moduleColors, geneTree, 
     file = "W_net.RData")