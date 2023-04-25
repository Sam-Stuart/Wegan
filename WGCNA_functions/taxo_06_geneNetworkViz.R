#'Build gene network
#'@description Build the heatmap and dendrogram based on the gene network 
#'@param mSetObj Input name of the created mSet Object
#'@param custom_norm Customized normalization provided by users 
#'@param power Soft threshold, dafault value is 6 
#'@param imgName Image name, "auto" or "manual", default is "auto"
#'@param format Select the image format, "png" or "pdf". Default is "png" 
#'@param dpi Define the resolution. If the image format is "pdf", users do not need define the dpi. For "png" format, the default dpi is 72. It is suggested that for high-resolution images, choose a dpi of 300. 
#'@param width Define image sizes, there 2 default widths. The first, width = NULL, is 10.5. The second default is width = 0, where the width is 7.2. Otherwise, users can customize widths on their own 
#'@param method The method to calculate hierarchical structure
#'@param nSelect Gene number showing in the network  
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
plot.geneNetwork <- function(mSetObj = NULL, 
                             custom_norm = NULL, # Allow users load customized normalized data
                             power = 6,
                             imgName,   
                             format = "png", # png or pdf 
                             dpi = 72, # Image resolution, dot per inch
                             width = NULL, 
                             method = "average", # Hierarchical method 
                             nSelect = 400) { 
    
    library(WGCNA)
    library(tidyverse) 
    source("./WGCNA_functions/taxo_00_generalDataUtils.R")  
    
    if(!is.null(custom_norm)){
        datExpr <- custom_norm 
    } else {
        mSetObj <- .get.mSet(mSetObj) 
        # Define gene expression data set 
        datExpr <- mSetObj$dataSet$exprObj[[1]]$data 
    }
    
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
    
    # Define clinical traits 
    # traits <- mSetObj$dataSet$traits 
    
    # Match datExpr and traits 
    # traits <- traits[rownames(traits) %in% rownames(datExpr), ]  # traits == datTraits 
    
    # Calculate topological overlap anew 
    dissTOM <- 1 - TOMsimilarityFromExpr(datExpr, power = 6)
    
    # Transform dissTOM with a power to make moderately strong connections more 
    # visible in the heatmap 
    # plotTOM <- dissTOM^7 
    
    # Set diagonal to NA for a nicer plot 
    # base::diag(plotTOM) <- NA 
    
    # Restrict the number of plotted genes to 400 
    nSelect <- 400 
    
    # For reproducibility, we set the random seed 
    set.seed(10) 
    
    select <- base::sample(nGenes, size = nSelect)
    
    selectTOM <- dissTOM[select, select] 
    
    selectTree <- stats::hclust(as.dist(selectTOM), method = "average") 
    
    selectColors <- moduleColors[select] 
    
    plotDiss <- selectTOM^7 
    # Set diagonal to NA for a nicer plot 
    base::diag(plotDiss) <- NA  
    
    # Define sizes for the final plot 
    if(is.null((width))) {
        w <- 10.5 
    } else if (width == 0) {
        w <- 7.2 
    } else {
        w <- width
    }
    h <- w # height 
    
    if (format == "png") {
        png(paste(imgName, ".", format, sep = ""), 
            # width = w, 
            # units = "cm", 
            res = dpi)
        
        WGCNA::TOMplot(plotDiss, 
                       selectTree, 
                       selectColors, 
                       main = "Netwrok heatmap plot, selected genes") 
        dev.off()
    } else {
        pdf(paste(imgName, ".", format, sep = "")) 
        WGCNA::TOMplot(plotDiss, 
                       selectTree, 
                       selectColors, 
                       main = "Netwrok heatmap plot, selected genes") 
        dev.off()
    }
    
    # Name plot for download 
    imgName_json <- paste(imgName, ".json", sep = "")
    
    imgName_export <- paste(imgName, "dpi", dpi, ".", format, sep = "")
    # Save the resulting figure file name to mSet object 
    mSetObj$imgSet$geneNetwork <- imgName_export 
    
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

args(plot.geneNetwork)


plot.geneNetwork(mSetObj = mSetObj,
                 imgName = "geneNetworkplots",
                 nSelect = 500) 

