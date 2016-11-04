lname <- load("W_net.RData")
lname <- load("W_dataInput.RData")


# annote
annot = read.csv("gene_id2name.txt", header = F, sep = "\t")
dim(annot)
names(annot) <- c("probe","geneName")
probes = names(datExpr)
probes2annot = match(probes, annot$probe)
# The following is the number or probes without annotation:
sum(is.na(probes2annot))
# Should return 0.
# 3, as the last 3 is wrong??



# Create the starting data frame
geneInfo0 = data.frame(geneId = probes,
                       geneSymbol = annot$geneName[probes2annot],
                       moduleColor = moduleColors)
# Order modules by their significance for highCol
#modOrder = order(-abs(cor(MEs, highCol, use = "p")));
# Add module membership information in the chosen order
# for (mod in 1:ncol(geneModuleMembership))
# {
#   oldNames = names(geneInfo0)
#   geneInfo0 = data.frame(geneInfo0, geneModuleMembership[, modOrder[mod]], 
#                          MMPvalue[, modOrder[mod]]);
#   names(geneInfo0) = c(oldNames, paste("MM.", modNames[modOrder[mod]], sep=""),
#                        paste("p.MM.", modNames[modOrder[mod]], sep=""))
# }
# Order the genes in the geneInfo variable first by module color, then by geneTraitSignificance
geneOrder = order(geneInfo0$moduleColor, -abs(geneInfo0$GS.highCol)); # why -abs?
geneInfo = geneInfo0[geneOrder, ]

# 
# geneOrder2 = order(geneInfo0$moduleColor, abs(geneInfo0$GS.highCol)); # why -abs?
# geneInfo2 = geneInfo0[geneOrder2, ]

#=====================================================================================
#
#  Code chunk 10
#
#=====================================================================================


write.csv(geneInfo, file = "geneInfo.csv", quote = F, sep = "\t")
