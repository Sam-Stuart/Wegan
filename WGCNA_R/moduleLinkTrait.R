# This R codes relate modules to external clinical traits 
# (corresponding to Tutorial I - Session 3)

library(WGCNA) 
library(tidyverse)
options(stringsAsFactors = FALSE)

#===============================================================================

# Quantify module-trait associations 

#===============================================================================

# Load saved data from previous sessions 
load("./output-WGCNA/consensusDataInput.RData")
load("./femaleLiver-networkConstruct-auto.RData") 

# Define numbers of genes and samples 
datExpr <- multiExpr.rm[[1]]$data # female liver expression data 

nGenes <- ncol(datExpr)
nSamples <- nrow(datExpr)

datTraits <- Traits[[1]]$data
datTraits <- datTraits[rownames(datTraits) %in% rownames(datExpr), ] 

# Recalculate MEs (aka. modules) with color labels 
MEs0 <- WGCNA::moduleEigengenes(expr = datExpr, colors = moduleColors)$eigengenes 
MEs <- orderMEs(MEs0)
moduleTraitCor <- WGCNA::cor(x = MEs, 
                             y = datTraits, 
                             use = "pairwise.complete.obs", 
                             method = "pearson")  
moduleTraitPvalue <- WGCNA::corPvalueStudent(moduleTraitCor, nSamples) 

# Display display correlation and their p-values 
WGCNA::sizeGrWindow(10, 6)

textMatrix <- paste(signif(moduleTraitCor, 2), "\n(",
                    signif(moduleTraitPvalue, 1), ")", sep = "") 
dim(textMatrix) <- dim(moduleTraitCor) 
par(mar = c(6, 8.5, 3, 3)) 

# Display the correlation values within a heatmap plot 
labeledHeatmap(Matrix = moduleTraitCor,
                xLabels = names(datTraits),
                yLabels = names(MEs),
                ySymbols = names(MEs),
                colorLabels = FALSE,
                colors = blueWhiteRed(50),
                textMatrix = textMatrix,
                setStdMargins = FALSE,
                cex.text = 0.5,
                zlim = c(-1, 1),
                main = paste("Module-trait relationships"))


#===============================================================================

# Gene relationship to trait and important modules 

#===============================================================================

weight <- as.data.frame(datTraits$weight_g)
names(weight) <- "weight" 
 
modNames <- base::substring(names(MEs), 3)  # Extract ME names 

geneModuleMembership <- as.data.frame(WGCNA::cor(datExpr, 
                                                 MEs, 
                                                 use = "pairwise.complete.obs"))
MMPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneModuleMembership), 
                                           nSamples)) 

names(geneModuleMembership) <- paste("MM", modNames, sep = "") 
names(MMPvalue) = paste("p.MM", modNames, sep = "") 

geneTraitSignificance <- as.data.frame(cor(datExpr, 
                                           weight, 
                                           use = "pairwise.complete.obs"))
GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance),
                                           nSamples)) 

names(geneTraitSignificance) <- paste("GS.", names(weight), sep = "") 
names(GSPvalue) <- paste("p.GS.", names(weight), sep = "") 
    
    
#===============================================================================

# Intramodular analysis: identify genes with high GS and MM  

#===============================================================================

# Plot a scatterplot of Gene Significance vs. Module Membership in the brown module 
module <- "brown" 
column <- base::match(module, modNames) 
moduleGenes <- moduleColors == module 

WGCNA::sizeGrWindow(7, 7) 
par(mfrow = c(1, 1))
verboseScatterplot(abs(geneModuleMembership[moduleGenes, column]),
                   abs(geneTraitSignificance[moduleGenes, 1]),
                   xlab = paste("Module Membership in", module, "module"),
                   ylab = "Gene significance for body weight",
                   main = paste("Module membership vs. gene significance\n"),
                   cex.main = 1.2, 
                   cex.lab = 1.2,
                   cex.axis = 1.2,
                   col = module)

#===============================================================================

# Summary output of network analysis results 

#===============================================================================

# Return all probe IDs included in the analysis 
names(datExpr)

# Probe IDs belonging to the brown module 
names(datExpr)[moduleColors == "brown"]

# Connect probe IDs to gene names and Entrez codes 
annot <- read.csv(file = "./rawData-WGCNA/GeneAnnotation.csv")  
dim(annot)
names(annot)
probes <- names(datExpr)
probes2annot <- match(probes, annot$substanceBXH) 

# The number or probes without annotation 
sum(is.na(probes2annot)) 

# Create the starting data frame holding probe ID, gene symbol, locus link ID
# (Entrez code), module color, gene significance for weight, and module membership 
# and p-values in all modules 
geneInfo0 <- data.frame(substanceBXH = probes, 
                        geneSymbol = annot$gene_symbol[probes2annot],
                        LocusLinkID = annot$LocusLinkID[probes2annot],
                        moduleColor = moduleColors,
                        geneTraitSignificance,
                        GSPvalue)

# Order modules by their significance for weight 
modOrder <- base::order(-abs(cor(MEs, weight, use = "p")), decreasing = FALSE)

# Add module membership information in the chosen order 
for (mod in 1:ncol(geneModuleMembership)) {
    oldName <- names(geneInfo0)
    geneInfo0 <- data.frame(geneInfo0, 
                            geneModuleMembership[, modOrder[mod]],
                            MMPvalue[, modOrder[mod]])
    names(geneInfo0) <- c(oldName, 
                          paste("MM.", modNames[modOrder[mod]], sep = ""), 
                          paste("p.MM.", modNames[modOrder[mod]], sep = ""))
}

# Order the genes in the geneInfo variable first by module color, then by 
# geneTraitSignificance 
geneOrder <- order(geneInfo0$moduleColor, -abs(geneInfo0$GS.weight)) 
geneInfo0 <- geneInfo0[geneOrder, ]

write.csv(geneInfo0, file = "./output-WGCNA/geneInfo.csv")

# fix(geneInfo0)









