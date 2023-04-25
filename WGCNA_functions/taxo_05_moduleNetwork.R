#'Engigenes network 
#'@description Plot dendrogram and heatmap to illustrate engigenes network 
#'@param mSetObj Input name of the created mSet Object
#'@param custom_norm Allow users to load customized normalized data 
#'@param power Soft threshold, default is 6 
#'@param imgName Image name, "auto" or "manual", default is "auto"
#'@param format Select the image format, "png" or "pdf". Default is "png" 
#'@param dpi Define the resolution. If the image format is "pdf", users do not need define the dpi. For "png" format, the default dpi is 72. It is suggested that for high-resolution images, choose a dpi of 300. 
#'@param width Define image sizes, there 2 default widths. The first, width = NULL, is 10.5. The second default is width = 0, where the width is 7.2. Otherwise, users can customize widths on their own 
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
plot.eigengenesNetwork <- function(mSetObj = NULL,
                                   custom_norm = NULL,
                                   power = 6,
                                   imgName = "auto", #  auto or manual modes
                                   format = "png", # png or pdf 
                                   dpi = 72, # Image resolution, dot per inch
                                   width = NULL) {  # Image dimension, inches 
    
    library(WGCNA)
    library(tidyverse) 
    source("./WGCNA_functions/taxo_00_generalDataUtils.R")  
    
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
            # Name plot for download 
            imgName_js <- paste(var, ".json", sep = "") 
        } else {
            file <- paste(imgName[i], "dpi", dpi, ".", format, sep = "") 
            # Name plot for download 
            imgName_js <- paste(imgName[i], ".json", sep = "") 
        }
        
        
        # Define sizes for the final plot 
        if(is.null((width))) {
            w <- 10.5 
        } else if (width == 0) {
            w <- 7.2 
        } else {
            w <- width
        }
        h <- w # height 
        
        # Set up the PDF device 
        if (format == "png") {
            png(file, width = w, units = "cm", res = dpi) 
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
            pdf(file, width = w, height = h, units = "cm")
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
        imgName_json <- list(imgName_js)  
    }
    
    # Save the resulting figure file name to mSet object 
    mSetObj$imgSet$moduleNetworkPlots <- plot_list  
    
    .set.mSet(mSetObj)
    
}


#===============================================================================

# Validate the above function 

#===============================================================================

load('./WGCNA_data/mSet_example.RData') 
load("./WGCNA_data/clinicalTrait_example.RData")
source("./WGCNA_functions/taxo_01_dataInputClean.R")

.on.public.web <- FALSE  
mSetObj <- make.exprSet(mSetObj = mSetObj_example)
mSetObj$dataSet$traits <- allTraits 

# args(plot.eigengenesNetwork)


# debug(plot.eigengenesNetwork) 
plot.eigengenesNetwork(mSetObj = mSetObj) 
# undebug(plot.eigengenesNetwork) 









