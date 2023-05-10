# This R codes relate modules to external clinical traits 
# (corresponding to Tutorial I - Session 3)

library(WGCNA) 
library(tidyverse)
options(stringsAsFactors = FALSE)

#===============================================================================

# Quantify module-trait associations 

#===============================================================================

# Load saved data from previous sessions 
load("./WGCNA_data/consensusDataInput.RData")
load("./WGCNA_data/femaleLiver-networkConstruct-auto.RData") 

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



#===============================================================================

# Reproduce base-R plot using ggplot2 

#===============================================================================

# Main input data 
df_moduleTraitCor <- as.data.frame(moduleTraitCor) 

# Reorder modules according to the correlation matrix 
dist_moduleTraitCor <- dist(moduleTraitCor, method = 'euclidean')
# Hierarchical clustering of modules based on correlation matrix  
hc_moduleTrait <- stats::hclust(dist_moduleTraitCor, method = 'average') 

# Reorder traits according to the correlation matrix 
dist_moduleTraitCor_t <- dist(t(moduleTraitCor), method = 'euclidean')   
# Hierarchical clustering of modules based on correlation matrix  
hc_moduleTrait_t <- stats::hclust(dist_moduleTraitCor_t, method = 'average') 


# Re-order by column 
df_moduleTraitCor <- df_moduleTraitCor[hc_moduleTrait$order, hc_moduleTrait_t$order]

# y-axis labels 
label_yaxis <- rownames(df_moduleTraitCor)
# x-axis labels 
label_xaxis <- colnames(df_moduleTraitCor) 


df_moduleTraitCor2 <- df_moduleTraitCor %>%
    rownames_to_column("modulecolor") %>% 
    tidyr::gather(key = "trait", value = "rho", -modulecolor) 

# Text 
df_textMatrix <- as.data.frame(textMatrix) 
colnames(df_textMatrix) <- colnames(moduleTraitCor)  # rename columns 
rownames(df_textMatrix) <- rownames(moduleTraitCor)  # rename rows 

df_textMatrix2 <- df_textMatrix %>% 
    rownames_to_column("modulecolor") %>% 
    tidyr::gather(key = "trait", value = "rho_pval", -modulecolor)

# Add text to the main input data 
df_moduleTraitCor3 <- df_moduleTraitCor2 %>% 
    left_join(df_textMatrix2, 
              by = c("trait", "modulecolor"))


# Plot a heatmap using geom_tile()  
ggplot(df_moduleTraitCor3, 
       aes(x = trait, 
           y = modulecolor, 
           fill = rho)) + 
    geom_tile(color = "black") + # heat map 
    ggplot2::geom_text(aes(label = rho_pval),
                       color = "white",
                       check_overlap = TRUE,
                       inherit.aes = TRUE,
                       size = 2) +
    scale_fill_gradient2(low = "#075AFF",
                         mid = "#FFFFCC",
                         high = "#FF0000",
                         midpoint = 0,
                         limit = c(-1, 1),
                         name = "Pearson\nCorrelation") +
    labs(x = "Traits",
         y = "Modules",
         color = "Correlation n\ coefficient") +
    theme(axis.text.x = element_text(angle = 45, 
                                     hjust = 1, 
                                     vjust = 1,
                                     size = 10),
          axis.text.y = element_text(size = 10)) +
    scale_x_discrete(limits = label_xaxis) +
    scale_y_discrete(limits = label_yaxis)




