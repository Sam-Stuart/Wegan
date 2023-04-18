#'Engigenes network 
#'@description Plot dendrogram and heatmap to illustrate engigenes network 
#'@param mSetObj Input name of the created mSet Object
#'@param custom_norm Allow users to load customized normalized data 
#'@param power Soft threshold, default is 6 
#'@param imgName Image names either automatically generated or manually set 
#'@param format PNG or PDF 
#'@param dpi Default is 72 
#'@param width Image width 
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
plot.eigengenesNetwork <- function(mSetObj = NULL,
                                   custom_norm = NULL,
                                   power = 6,
                                   imgName = "auto", #  auto(matic) or manual
                                   format = "png", # png or pdf 
                                   dpi = 72, 
                                   width = NA) {
    
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

        # Set image file names 
        if (imgName == "auto") {
            file <- paste(var, "Eigengene_network", ".", format, sep = "") 
        } else {
            file <- paste(imgName[i], "dpi", dpi, ".", format, sep = "")  
        }
        
        # Set plot dimensions 
        if (is.na(width)) {
            w <- 480  
        } else if (width == 0) {
            w <- 480 
        } else {
            w <- width
        }
        h <- w # height
        
        # Set up the PDF device 
        if (format == "png") {
            png(file, width = w, height = h)
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
            
        } else {
            # Default image is FDF format 
            pdf(file, width = w, height = h)
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
        }
        
        plot_list[i] <- list(file) 
    }
    
    mSetObj$imgSet$moduleNetworkPlots <- plot_list  
    
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








