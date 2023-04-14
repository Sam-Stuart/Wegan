
plot.moduleTraitHeatmap <- function(mSetObj,
                                    power = 6) {
    
    library(WGCNA)
    
    mSetObj <- .get.mSet(mSetObj)

    # Subset clinical traits data frame from mSet object. 
    # In clinical trait data frame, rows are samples and columns are clinical variables  
    traits <- mSetObj$dataSet$traits  

    # Gene expression data frame with row being samples and columns are genes 
    datExpr <- mSetObj$dataSet$exprObj[[1]]$data 
    
    # Match datExpr and traits 
    traits <- traits[rownames(datTraits) %in% rownames(datExpr), ]  
    traitRow <- base::match(rownames(datExpr), rownames(traits))
    traits <- traits[traitRow, ]
    
    # Define numbers of genes and samples 
    numGenes <- ncol(datExpr)
    numSamples <- nrow(datExpr) 
    
    # Define module color labels ... 
    net <- WGCNA::blockwiseModules(datExpr, 
                                   power = power, # Default value
                                   TOMType = "unsigned", 
                                   minModuleSize = 30,
                                   reassignThreshold = 0, 
                                   mergeCutHeight = 0.25,
                                   numericLabels = TRUE, 
                                   pamRespectsDendro = FALSE,
                                   saveTOMs = TRUE,
                                   saveTOMFileBase = "femaleMouseTOM",
                                   verbose = 3)

    moduleColors <- WGCNA::labels2colors(net$colors)
    
    MEs0 <- WGCNA::moduleEigengenes(datExpr, moduleColors)$eigengenes 
    MEs <- WGCNA::orderMEs(MEs0)
    
    moduleTraitCorr <- WGCNA::cor(MEs, traits, use = "p") 
    moduleTraitPvalue <- WGCNA::corPvalueStudent(moduleTraitCorr, numSamples) 
    
    # Plot a heatmap to illustrate relationship between modules and clinical traits
    textMatrix <- paste(signif(moduleTraitCorr, 2), "\n(",
                        signif(moduleTraitPvalue, 1), ")", sep = "") 
    dim(textMatrix) <- dim(moduleTraitCor) 
    par(mar = c(6, 8.5, 3, 3)) 
    
    # Display the correlation values within a heatmap plot 
    pdf(file)
    
    WGCNA::labeledHeatmap(Matrix = moduleTraitCorr,
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
    
    dev.off()

    mSetObj$imgSet$modTraitHeatmap <- file 

    .set.mSet(mSetObj) 

} 




