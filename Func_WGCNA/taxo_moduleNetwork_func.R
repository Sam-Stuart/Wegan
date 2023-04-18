#'Engigenes network 
#'@description Plot dendrogram and heatmap to illustrate engigenes network 
#'@param mSetObj Input name of the created mSet Object
#'@param custom_norm Allow users to load customized normalized data 
#'@param power Soft threshold, default is 6 
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
plot.eigengenesNetwork <- function(mSetObj = NULL,
                                   custom_norm = NULL,
                                   power = 6) {
    
    library(WGCNA)
    library(tidyverse) 
    source("./Func_WGCNA/general_data_utils.R") 
    
    if(!is.null(custom_norm)){
        datExpr <- custom_norm 
    } 
        
    mSetObj <- .get.mSet(mSetObj) 
    
    if(is.null(mSetObj$dataSet$exprObj) | is.null(mSetObj$dataSet$traits)) {
        stop("mSetObj lacks required element for analysis.")
    } 
    
    datExpr <- mSetObj$dataSet$exprObj[[1]]$data 
        
    # Define nSamples and nGenes 
    nSamples <- nrow(datExpr) 
    nGenes <- ncol(datExpr) 
    
    # Define module colors 
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
    
    # Recalculate module eigengenes 
    MEs <- moduleEigengenes(datExpr, moduleColors)$eigengenes 
    
    # Define clinical traits 
    traits <- mSetObj$dataSet$traits 
    # Match datExpr and traits 
    traits <- traits[rownames(traits) %in% rownames(datExpr), ] 
    
    # Extract clinical variables names
    clinicalVar <- names(traits) 
    
    # Create a list to store plots 
    plot_list <- list() 
    for(i in 1:length(clinicalVar)) {
        var <- clinicalVar[i]
        var_df <- as.data.frame(traits[, i]) 
        base::names(var_df) <- var 
        
        # Add the weight to existing module eigengenes 
        MET <- WGCNA::orderMEs(cbind(MEs, var_df)) 
        
        # Plot the relationships among the eigengenes and the trait 
        # sizeGrWindow(5, 7.5)
        # par(cex = 0.9) 
        # plotEigengeneNetworks(MET, "", 
        #                       marDendro = c(0, 4, 1, 2),
        #                       marHeatmap = c(3, 4, 1, 2),
        #                       cex.lab = 0.8,
        #                       xLabelsAngle = 90)
        
        # Split the dendrogram and heatmap plots 
        # Plot the dendrogram 
        # sizeGrWindow(6, 6)
        file = paste(var, "Eigengene_network.pdf", sep = "_") # Define file names 
        
        pdf(file, width = 12, height = 8)
        par(cex = 1.0) 
        WGCNA::plotEigengeneNetworks(MET, 
                                    "Eigengene dendrogram",
                                     marDendro = c(0, 4, 2, 0),
                                     plotHeatmaps = FALSE) 
        
        par(cex = 1.0)
        WGCNA::plotEigengeneNetworks(MET, 
                                     "Eigengene adjacency heatmap",
                                      marHeatmap = c(3, 4, 2, 2),
                                      plotDendrograms = FALSE,
                                      xLabelsAngle = 90)
        
        dev.off() 
        
        plot_list[i] <- list(file) 
    }
    
    mSetObj$imgSet <- plot_list
    
    return(mSetObj)
    
}


#===============================================================================

# Validate the above function 

#===============================================================================

load('./mSet_example.RData') 
load("./clinicalTrait_example.RData")
load("./net_example.RData")
source("./Func_WGCNA/dataInputClean_fun.R")

.on.public.web <- FALSE  

mSetObj <- make.exprSet(mSetObj = mSetObj_example)

mSetObj$dataSet$traits <- allTraits 

# args(plot.eigengenesNetwork)


# debug(plot.eigengenesNetwork) 

plot.eigengenesNetwork(mSetObj = mSetObj) 

# undebug(plot.eigengenesNetwork) 








