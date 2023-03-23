# This script was run in R v3.6.1 using the R packages WGCNA v1.68 and sva v3.34.0.
# See this webpage for information on how to switch R versions if needed:
# https://support.rstudio.com/hc/en-us/articles/200486138-Changing-R-versions-for-RStudio-desktop   

library("ggplot2") # For PCA plot
library("WGCNA") # For network analysis
library("DESeq2") # For data transformation

# Allow multi-threading within WGCNA: 
# This function detects the number of threads available and helps speed up certain calculations.
enableWGCNAThreads()

options(stringsAsFactors=FALSE) # Required for WGCNA. 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Input, inspect and prepare astro expression data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd('WORKING_DIRECTORY_PATH')

counts_raw <- read.csv(file="counts_data_file.csv", header=TRUE, row.names = 1)
# Filter expression data using a CPM cut-off: 
# Remove genes who have row sum less than 1 CPM for at least 53 libraries. 53 is 95% of the number of samples 
counts_round <- ceiling(counts_raw) # Round values up.
# counts_round <- counts_round[rowSums(counts_round>0)>=53,] 

# Normalize the filtered expression data using a variance stabilizing transformation, a DESq2 function:
# This is suggested by the creators of WGCNA here: https://horvath.genetics.ucla.edu/html/CoexpressionNetwork/Rpackages/WGCNA/faq.html. 
# counts_matrix <- data.matrix(counts_raw_by_gname[,-c(1:2)]) # Create matrix of integers for transformation.
counts_trans <- varianceStabilizingTransformation(as.matrix(counts_round))
counts_trans <- as.data.frame(counts_trans) # Turn matrix back into data frame.

# Filter data based on variance, choosing the top 90% most variable genes:
counts_trans$variance <- apply(counts_trans, 1, var) # Create a column of calculated variances.
counts_trans_var <- counts_trans[counts_trans$variance >= quantile(counts_trans$variance, c(.10)), ] # Use the 10% quantile as the cut-off.
counts_trans_var$variance <- NULL # Remove the variance column.

# Transpose corrected data. 
counts_0 <- as.data.frame(t(counts_trans_var))

# The function goodSampleGenes checks for missing entries, entries with weights below a threshold, 
# and zero-variance genes, and returns a list of samples and genes that pass criteria on maximum number 
# of missing or low weight values:
gsgcounts <- goodSamplesGenes(counts_0, verbose=3)
gsgcounts$allOK # This returns TRUE, so all genes pass inspection. If it does not, run the following
# if (!gsgcounts$allOK)
# {
#   # print the gene and sample names that were removed:
#   if (sum(!gsgcounts$goodGenes)>0)
#     printFlush(paste("Removing genes:", paste(names(counts_0)[!gsgcounts$goodGenes], collapse=", ")));
#   if (sum(!gsgcounts$goodSamples)>0)
#     printFlush(paste("Removing samples:", paste(rownames(counts_0)[!gsgcounts$goodSamples], collapse=", ")));
#   # Remove the offending genes and samples from the data:
#   counts_0 <- counts_0[gsgcounts$goodSamples, gsgcounts$goodGenes]
# }

# Hierarchical clustering based on average normalized expression values is used to detect outlier samples:
Treecounts <- hclust(dist(counts_0), method="average")
datTraits <- read.csv("sample_trait_data.csv", header=TRUE, row.names=1)
traitColors <- numbers2colors(datTraits, signed = FALSE);

png(filename = "metabolites other sample tree.png", res=1200, width=6.5, height=4, units="in")
par(cex=0.6)
par(mar=c(0,5,2,0))
plotDendroAndColors(Treecounts, traitColors,groupLabels = names(datTraits),main = "Sample dendrogram and trait heatmap")
dev.off()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Network construction~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Choose a set of soft-thresholding powers to test: 
# We will use two different techniques to choose the soft-threshold from the set, the Scale-free topology fit and the Mean connectivity. 
# We will choose a soft-threshold as a trade-off between these two techniques.
powers <- c(1:20)
soft_threshcounts <- pickSoftThreshold(counts_0, dataIsExpr = TRUE, powerVector=powers, verbose=5, corFnc="bicor", corOptions="use='p'") 

# Scale-free topology fit index as a function of the soft-thresholding power: 
# We want our network to follow an approximate scale-free topology. Scale-free networks of genes are robust in that they correlate genes 
# in a manner that greatly exceeds the average correlation.  
png(filename = "metabolites other scale-free topology.png", res=1200, width=6.5, height=5, units="in")
plot(soft_threshcounts$fitIndices[,1], -sign(soft_threshcounts$fitIndices[,3])*soft_threshcounts$fitIndices[,2],
     xlab="Soft threshold (power)",ylab="Scale-free topology fit (R^2)",type="n",
     main=paste("Metabolites other \nScale-free topology fit"))
text(soft_threshcounts$fitIndices[,1], -sign(soft_threshcounts$fitIndices[,3])*soft_threshcounts$fitIndices[,2],
     labels=powers, cex=0.9, col="red")
abline(h=0.4, col="red") # This line corresponds to using an R^2 cut-off of 0.90. R^2 values provide a goodness of fit statistic. We are fitting our soft-thresholding powers to a hypothetical scale-free network. 
dev.off() 

-sign(soft_threshcounts$fitIndices[,3])*soft_threshcounts$fitIndices[,2] # 5 was the first soft-thresholding power to exceed the R^2 cut-off. 

# Mean connectivity as a function of the soft-thresholding power: We want highly correlated genes to be connected and fall 
# into the same modules. We will choose a soft-thresholding power that will ensure that highly connected genes are connected 
# in a manner that greatly exceeds the mean connectivity.  
png(filename = "metabolites other mean connectivity.png", res=1200, width=6.5, height=5, units="in")
plot(soft_threshcounts$fitIndices[,1], soft_threshcounts$fitIndices[,5],
     xlab="Soft threshold (power)",ylab="Mean connectivity", type="n",
     main=paste("Metabolites other \nMean connectivity fit"))
text(soft_threshcounts$fitIndices[,1], soft_threshcounts$fitIndices[,5], labels=powers, cex=0.9, col="red")
dev.off() # An "elbow" exists at 10, which is indicative of a good soft-thresholding power choice. 

# We will go with 10 as our soft-threshold power for the network. 

# Generate TOM and identify expression modules: 
# Pair-wise correlation (connectivity) values between genes are calculated using the biweight midcorrelation function, and correlation 
# values are then weighted by raising them to the soft-threshold power of 7. The topological overlay map is then constructed to calculate 
# the interconnectedness between correlated genes proportional to the number of neighbors that a pair of genes share in common. A signed 
# network is being employed to differentiate between positively correlated genes that are strongly connected, and negatively correlated 
# genes that are not strongly connected. Module membership is calculated by correlating the gene and its associated module eigengene using 
# the biweight midcorrelation function. 
TOMcounts <-  blockwiseModules(counts_0, maxBlockSize=1000, minBlockSize=0, minModuleSize=1,
                               corType="bicor", maxPOutliers=0.05, pearsonFallback="individual",
                               power=10, networkType="signed", TOMType="signed", reassignThreshold=1E-8,
                               mergeCutHeight=0.4, deepsplit=2, numericLabels=TRUE, verbose=4) # Make sure block-size exceeds the number of network genes. You may require more memory to run this step if you are unable to create a single block.

# IMPORTANT: Parameter optimization: Try different combinations and save figures under different file names (I like to use the parameter 
# numbers in the file names to set them apart). Try different minModuleSize parameters to determine cut-off (20 to 50; looking at 
# table(moduleColorscounts), we want a smaller number of small modules), maxPOutliers 0.05 or 0.1 (look at module dendrogram 
# and choose the figure that looks less messy. If both figures look good, check the ME dendrogram. The more variation in heights, the better.), 
# and different mergeCutHeight parameters (Look at module dendrogram, ME heatmap and ME dendrogram to determine cut-off. We want good 
# separation between modules without over merging. Note that grey=no assigned module). Start with no color sequence (ie choose defaults) for 
# module colors by excluding the argument "colorSeq". Once you settle on your parameters, you can adjust the color palette.

# Label modules using colors:
moduleColorscounts <- labels2colors(TOMcounts$colors, zeroIsGrey=TRUE, colorSeq=c("yellow", "paleturquoise", "blue", "green", "darkslateblue", "purple", "orange", "red", "cyan", "pink", "slateblue", "darkgreen", "darkcyan", "lightcyan", "gold", "darkturquoise", "saddlebrown", "darkgoldenrod", "deeppink", "brown", "tan", "salmon", "greenyellow", "darkmagenta", "violet", "magenta", "lightyellow"))
table(moduleColorscounts) # See the number of modules and their respective sizes. The grey color is reserved for unassigned genes.

# Calculate module eigengenes: Module eigengenes represent the expression profile of their respective module. They are the first
# principal components of the modules.
MEscounts <- TOMcounts$MEs # Extract MEs
MEs0counts <- moduleEigengenes(counts_0, moduleColorscounts)$eigengenes # Assign colors to MEs
MEscounts <- orderMEs(MEs0counts) # Order MEs by hierarchical clustering

# Plot the interconnectedness dendrogram and the module colors underneath: 
# Densely interconnected genes will be clustered into modules and displayed in an interconnectedness dendrogram, where genes with the 
# highest intramodular connectivity were located at the tip of each module branch. Note that modules are networks in and of themselves.
# Intramodular connectivity is calculated by taking the sum of the pairwise biweight midcorrelation values (edge weights) of the gene 
# to its module counterparts.
png(filename = "Module dendrogram.png", res=1200, width=6.5, height=4, units="in")
plotDendroAndColors(TOMcounts$dendrograms[[1]], main=paste("Cluster dendrogram with modules"), colors = moduleColorscounts, groupLabels = "Module colors", hang = 0.03, dendroLabels=FALSE, addGuide=TRUE, guideHang=0.05)
dev.off()

# Plot the eigengene dendrogram: Modules are clustered by eigengene correlation using average linkage. Highly correlated eigengenes 
# represent similar module networks.
png(filename = "eigengene correlation dendrogram.png", res=1200, width=6.5, height=5, units="in")
plotEigengeneNetworks(MEscounts, "Module eigengene correlation dendrogram", plotDendrograms=TRUE, plotHeatmaps=FALSE, plotAdjacency=FALSE, excludeGrey=FALSE, marDendro=c(1,4,4,1))
dev.off()

# Plot the eigengene heatmap matrix: Modules are related pair-wise. Similar modules (positive/red side of correlation spectrum) and
# dissimilar modules (negative/blue side of correlation spectrum) are showcased.
png(filename = "eigengene correlation heatmap.png", res=1200, width=9, height=7, units="in")
plotEigengeneNetworks(MEscounts, "Module eigengene correlation heatmap", plotHeatmaps=TRUE, plotDendrograms=FALSE, plotAdjacency=FALSE, excludeGrey=TRUE, marHeatmap=c(8,10,3,3), xSymbols=names(MEscounts[-6]), ySymbols=names(MEscounts[-6]))
dev.off()

# Create "for" loop to generate gene lists in .txt format for each color in moduleColorscounts (ie for each module):
# The gene lists from differentially expressed modules will be used for enrichment testing.
network_genescounts <- as.data.frame(rownames(counts_trans_var)) # List of genes and gene names that will be included in the TOM. This will be important for the enrichment test following ME differential expression analysis.
for (colorcounts in moduleColorscounts){
  inModcounts <- is.finite(match(moduleColorscounts, colorcounts))
  gene.listcounts <- network_genescounts[inModcounts,]
  write.table(gene.listcounts, file=paste("counts_module_", colorcounts, ".txt",sep=""), row.names=FALSE, col.names="metabolite", sep="\t", quote=FALSE)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~HIV trait analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Input and format trait data: 
# Traits are HIV status. HIV samples were given a 1, while Control samples were given a 0.
# trait_data <- read.csv(file="sample_trait_data_age_class.csv", header=TRUE) 
trait_data <- read.csv(file="sample_trait_data.csv", header=TRUE) 
samples <- rownames(counts_0) 
traitRowscounts <- match(samples, trait_data$Sample)
trait_datacounts <- as.data.frame(trait_data[traitRowscounts, -1])
rownames(trait_datacounts) <- trait_data[traitRowscounts, 1]
colnames(trait_datacounts) <- "Group"
nSample <- length(samples)
MEscounts_no_grey <- MEscounts[,-6]

# Correlate modules with traits using Spearman's correlation function:
moduleTraitCorcounts <- cor(MEscounts_no_grey, trait_datacounts, use = "p", method="spearman") # Obtain correlation value
moduleTraitPvaluecounts <- corPvalueStudent(moduleTraitCorcounts, nSample) # Obtain Student asymptotic p-values. 
textMatrixcounts_pvalues <- paste(signif(moduleTraitCorcounts, 2), "\n(", signif(moduleTraitPvaluecounts, 2), ")", sep = "")
textMatrixcounts <- signif(moduleTraitCorcounts, 2)
dim(textMatrixcounts) <- dim(moduleTraitCorcounts)

# Create figure with p-values displayed:
# We will use this figure to determine significance.
png(filename = "module-trait correlation with p-values.png", res=1200, width=6.5, height=11, units="in")
par(mar = c(3, 9, 3, 3))
labeledHeatmap(Matrix = moduleTraitCorcounts,
               xLabels = names(trait_datacounts),
               yLabels = names(MEscounts_no_grey),
               ySymbols = names(MEscounts_no_grey),
               colorLabels = TRUE,
               colors = blueWhiteRed(30),
               textMatrix = textMatrixcounts_pvalues,
               setStdMargins = FALSE,
               cex.text = 1,
               zlim = c(-1,1),
               main="Module-trait correlation")
dev.off()
