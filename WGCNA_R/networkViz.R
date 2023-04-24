# Network visualization using WGCNA functions  

library(WGCNA)
options(stringsAsFactors = FALSE)

load("./WGCNA_data/wgcna_dataInputClean.RData")
load("./WGCNA_data/femaleLiver-networkConstruct-auto.RData") 
load("./WGCNA_data/consensusDataInput.RData") 

#===============================================================================

# Visualizing the gene network 

#===============================================================================

# Define numbers of genes and samples 
datExpr <- multiExpr[[1]]$data # female liver expression data 

nGenes <- ncol(datExpr)
nSamples <- nrow(datExpr)

# Define clinical traits data 
datTraits <- Traits[[1]]$data
datTraits <- datTraits[rownames(datTraits) %in% rownames(datExpr), ] 

# Calculate topological overlap anew 
dissTOM <- 1 - TOMsimilarityFromExpr(datExpr, power = 6)

# Transform dissTOM with a power to make moderately strong connections more 
# visible in the heatmap 
plotTOM <- dissTOM^7 

# Set diagonal to NA for a nicer plot 
base::diag(plotTOM) <- NA 

# Call the plot function 
# sizeGrWindow(9, 9) 
# TOMplot(plotTOM, 
#         geneTree, 
#         moduleColors, 
#         main = "Network heatmap plot, all genes")


# Restrict the number of plotted genes to 400 
nSelect <- 400 

# For reproducibility, we set the random seed 
set.seed(10) 
select <- sample(nGenes, size = nSelect)
selectTOM <- dissTOM[select, select] 

selectTree <- hclust(as.dist(selectTOM), method = "average") 
selectColors <- moduleColors[select] 

sizeGrWindow(9, 9) 

plotDiss <- selectTOM^7 
diag(plotDiss) <- NA 

TOMplot(plotDiss, selectTree, selectColors, 
        main = "Netwrok heatmap plot, selected genes")


#===============================================================================

# Visualizing the network of eigengenes 

#===============================================================================

# Recalculate module eigengenes 
MEs <- moduleEigengenes(datExpr, moduleColors)$eigengenes 

# Isolate weight from the clinical traits 
weight <- as.data.frame(datTraits$weight_g) 
names(weight) <- "weight" 

# Add the weight to existing module eigengenes 
MET <- WGCNA::orderMEs(cbind(MEs, weight)) 

# Plot the relationships among the eigengenes and the trait 
sizeGrWindow(5, 7.5)
par(cex = 0.9) 
plotEigengeneNetworks(MET, "", 
                      marDendro = c(0, 4, 1, 2),
                      marHeatmap = c(3, 4, 1, 2),
                      cex.lab = 0.8,
                      xLabelsAngle = 90)

# Split the dendrogram and heatmap plots 
# Plot the dendrogram 
sizeGrWindow(6, 6)
par(cex = 1.0) 
plotEigengeneNetworks(MET, 
                      "Eigengene dendrogram",
                      marDendro = c(0, 4, 2, 0),
                      plotHeatmaps = FALSE) 

par(cex = 1.0)
plotEigengeneNetworks(MET, 
                      "Eigengene adjacency heatmap",
                      marHeatmap = c(3, 4, 2, 2),
                      plotDendrograms = FALSE,
                      xLabelsAngle = 90)








