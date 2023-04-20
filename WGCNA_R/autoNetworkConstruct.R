# This R file demonstrates automatic construction of the gene network 
# and identification of modules 

library(WGCNA)
library(tidyverse)
library(gridExtra) # Layout multiple ggplot figures 

options(stringsAsFactors = FALSE)
lnames <- load(file = "./WGCNA_output/consensusDataInput.RData")
lnames 

# Choose the soft-threshold power: analysis of network topology 
powers <- c(c(1:10), seq(from = 12, to = 20, by = 2)) 
datExpr <- multiExpr.rm[[1]]$data # Female liver data 

# Call the network topology analysis function - takes a while ... 
sft <- WGCNA::pickSoftThreshold(datExpr, 
                                powerVector = powers, 
                                verbose = 5)

# Plot 
sizeGrWindow(9, 5)
par(mfrow = c(1, 2)) 
cex1 = 0.9 

plot(sft$fitIndices[, 1], -sign(sft$fitIndices[, 3])*sft$fitIndices[, 2], type = "n") 
text(sft$fitIndices[, 1], -sign(sft$fitIndices[, 3])*sft$fitIndices[, 2],
     labels = powers, 
     cex = cex1, col = "red") 
abline(h = 0.90, col = "red") 
plot(sft$fitIndices[, 1], sft$fitIndices[, 5],
     xlab = "Soft threshold (power)", ylab = "Mean connectivity", type = "n",
     main = paste("Mean connectivity")) 
text(sft$fitIndices[, 1], sft$fitIndices[, 5], labels = powers, cex = cex1, col = "red") 


# One-step network construction and module detection 
net <- WGCNA::blockwiseModules(datExpr, 
                               power = 6,
                               TOMType = "unsigned", 
                               minModuleSize = 30,
                               reassignThreshold = 0, 
                               mergeCutHeight = 0.25,
                               numericLabels = TRUE, 
                               pamRespectsDendro = FALSE,
                               saveTOMs = TRUE,
                               saveTOMFileBase = "femaleMouseTOM",
                               verbose = 3)
table(net$colors) 

# Open a graphic window 
pdf(file = "./output-WGCNA/femaleLiver-dendrogram.pdf", width = 12, height = 7)
# sizeGrWindow(12, 9)
mergedColors <- WGCNA::labels2colors(net$colors)

plotDendroAndColors(net$dendrograms[[1]], 
                    mergedColors[net$blockGenes[[1]]],
                    "Module colors",
                    dendroLabels = FALSE, hang = 0.03,
                    addGuide = TRUE, guideHang = 0.05)
dev.off()


moduleLabels <- net$colors
moduleColors <- labels2colors(net$colors) 
MEs <- net$MEs
geneTree <- net$dendrograms[[1]] 

save(MEs, moduleLabels, moduleColors, geneTree,
     file = "./output-WGCNA/femaleLiver-networkConstruct-auto.RData")



#===============================================================================

# Reproduce base R plots using ggplot2 

#===============================================================================
# Scale-free topology fit index vs Soft-threshold power
library(ggplot2) 
# Input data for plotting 
df_fitIndices <- sft$fitIndices 
# Create a new column for y-axis in the first plot  
df_fitIndices <- df_fitIndices %>% 
        dplyr::mutate(plot1_y = -sign(slope)*SFT.R.sq) 
        

plot1 <- ggplot2::ggplot(df_fitIndices, 
                        aes(x = Power, y = plot1_y)) +
                geom_point() + 
                geom_text(aes(label = Power),
                          nudge_x = 0.02, nudge_y = 0.025, # Shift texts along axis
                          check_overlap = TRUE) +
                labs(x = "Soft power thresholds",
                     y = "Scale-free topology fit index") + 
                scale_y_continuous(limits = c(0, 1)) + 
                theme_classic()
        
print(plot1) 


# Mean connectivity  
plot2 <- ggplot2::ggplot(df_fitIndices,
                         aes(x = Power, y = mean.k.)) + 
                geom_point() + 
                geom_label(aes(label = Power),
                           position = "nudge") +
                labs(x = "Soft power thresholds",
                     y = "Mean connectivity") + 
                theme_classic() 

print(plot2)

# Arrange two plots side-by-side on one page 












 
