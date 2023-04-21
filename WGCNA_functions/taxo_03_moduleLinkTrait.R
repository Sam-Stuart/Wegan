#'Link modules and clinical traits 
#'@description Plot heatmap to illustrate correlation between modules (engingene)
#'and clinical trats
#'@param mSetObj Input name of the created mSet Object
#'@param power Soft threshold, default is 6 
#'@param file Image file name
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
plot.moduleTraitHeatmap <- function(mSetObj, 
                                    power = 6, 
                                    file) {
    
    library(WGCNA)
    library(tidyverse)
    source('./WGCNA_functions/taxo_00_generalDataUtils.R') 
    
    mSetObj <- .get.mSet(mSetObj)
    
    # Check if it contains the required element 
    if(is.null(mSetObj$dataSet$exprObj) | is.null(mSetObj$dataSet$traits)) {
        stop("mSet object does not contain required element for analysis.")
    }

    # Subset clinical traits data frame from mSet object. 
    # In clinical trait data frame, rows are samples and columns are clinical variables  
    traits <- mSetObj$dataSet$traits  

    # Gene expression data frame with row being samples and columns are genes 
    datExpr <- mSetObj$dataSet$exprObj[[1]]$data
    
    # Match datExpr and traits 
    traits <- traits[rownames(traits) %in% rownames(datExpr), ]  
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
    MEs <- WGCNA::orderMEs(MEs0) # Similar eigengenes are next to each other 
    # Fast calculation of Pearson correlation 
    moduleTraitCorr <- WGCNA::cor(MEs, 
                                  traits, 
                                  use = "p", # aka. pairwise.complete.obs 
                                  method = "pearson") 
    # Calculate student asymptotic p-value for given correlations 
    moduleTraitPvalue <- WGCNA::corPvalueStudent(moduleTraitCorr, 
                                                 numSamples) 
    
    # Plot a heatmap to illustrate relationship between modules and clinical traits
    textMatrix <- paste(signif(moduleTraitCorr, 2), 
                        "\n(", signif(moduleTraitPvalue, 1), ")", 
                        sep = "") 
    dim(textMatrix) <- dim(moduleTraitCorr) 
    
 
    # Main input data 
    df_moduleTraitCor <- as.data.frame(moduleTraitCorr) 
    
    # Reorder modules according to the correlation matrix 
    dist_moduleTraitCor <- dist(moduleTraitCorr, method = 'euclidean')
    # Hierarchical clustering of modules based on correlation matrix  
    hc_moduleTrait <- stats::hclust(dist_moduleTraitCor, method = 'average') 
    
    # Reorder traits according to the correlation matrix 
    dist_moduleTraitCor_t <- dist(t(moduleTraitCorr), method = 'euclidean')   
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
    colnames(df_textMatrix) <- colnames(moduleTraitCorr)  # rename columns 
    rownames(df_textMatrix) <- rownames(moduleTraitCorr)  # rename rows 
    
    df_textMatrix2 <- df_textMatrix %>% 
        rownames_to_column("modulecolor") %>% 
        tidyr::gather(key = "trait", value = "rho_pval", -modulecolor)
    
    # Add text to the main input data 
    df_moduleTraitCor3 <- df_moduleTraitCor2 %>% 
        left_join(df_textMatrix2, 
                  by = c("trait", "modulecolor"))
    
    
    
    # Display the correlation values within a heatmap plot 
    png(file, width = 16, height = 12, units = "in", res = 120)   
    # Plot a heatmap using geom_tile()  
    ht_moduleTrait <- ggplot(df_moduleTraitCor3, 
                             aes(x = trait, 
                                 y = modulecolor, 
                                 fill = rho)) + 
                      geom_tile(color = "grey") + # heat map 
                      ggplot2::geom_text(aes(label = rho_pval),
                                             color = "white",
                                             check_overlap = TRUE,
                                             inherit.aes = TRUE,
                                             size = 3) +
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
    print(ht_moduleTrait)
    dev.off()

    mSetObj$imgSet$modTraitHeatmap <- file # Store image file name into mSet object  

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

# debug(plot.moduleTraitHeatmap)
plot.moduleTraitHeatmap(mSetObj = mSetObj,
                        file = "./WGCNA_output/ggplotModuleTrait.png")  
# undebug(plot.moduleTraitHeatmap)









