# This self-defined function identifies particular genes strongly associated with
# modules membership (MM) and clinical traits (GS = Gene significance)

#'Identify genes highly associated with module membership and clinical traits 
#'@description Output a table of genes and their coefficients corresponding to statistical associations with module membership and clinical traits 
#'@param mSetObj Input name of the created mSet Object
#'@param power Soft threshold, default is 6 
#'@param file Image file name
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
tabulate.intraModule <- function(mSetObj = NULL, power = 6, file) {
    
    library(WGCNA)
    library(tidyverse) 
    source("./WGCNA_functions/generalDataUtils.R") # Source util function for the testing purpose
    
    mSetObj <- .set.mSet(mSetObj) 
    
    # Define gene expression data set 
    datExpr <- mSetObj$dataSet$exprObj[[1]]$data 
    
    # Define nSamples 
    nSamples <- nrow(datExpr) 
    
    # Define clinical traits 
    traits <- mSetObj$dataSet$traits 
    
    # Match datExpr and traits 
    traits <- traits[rownames(traits) %in% rownames(datExpr), ]  
    traitRow <- base::match(rownames(datExpr), rownames(traits))
    traits <- traits[traitRow, ]
    
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
    
    # Calculate MEs (aka. modules) with color labels 
    MEs0 <- WGCNA::moduleEigengenes(expr = datExpr, 
                                    colors = moduleColors)$eigengenes 
    MEs <- WGCNA::orderMEs(MEs0)
    
    # Iterate over clinical variables  
    clinicalVar <- names(traits)
    
    geneInfo_list <- list() 
    for (i in 1:length(clinicalVar)) {
        # Store variable in a data frame 
        df_var <- as.data.frame(traits[, i])
        # Rename the variable 
        names(df_var) <- clinicalVar[i]
        
        modNames <- base::substring(names(MEs), 3)  # Extract module names 
        
        geneModuleMembership <- as.data.frame(WGCNA::cor(datExpr, 
                                                         MEs, 
                                                         use = "pairwise.complete.obs"))
        
        MMPvalue <- as.data.frame(WGCNA::corPvalueStudent(as.matrix(geneModuleMembership), 
                                                   nSamples)) 
        
        names(geneModuleMembership) <- paste("MM", modNames, sep = "") 
        
        names(MMPvalue) = paste("p.MM", modNames, sep = "") 
        
        geneTraitSignificance <- as.data.frame(cor(datExpr, 
                                                   df_var, 
                                                   use = "pairwise.complete.obs"))
        
        GSPvalue <- as.data.frame(corPvalueStudent(as.matrix(geneTraitSignificance),
                                                   nSamples)) 
        
        names(geneTraitSignificance) <- paste("GS.", names(df_var), sep = "") 
        
        names(GSPvalue) <- paste("p.GS.", names(df_var), sep = "") 
        
        # Order modules by their significance for clinical variables  
        modOrder <- base::order(-abs(cor(MEs, df_var, use = "p")))
        
        # Add module membership information in the chosen order 
        for (mod in 1:ncol(geneModuleMembership)) {
            
            geneInfo <- data.frame(geneID = rownames(MMPvalue),
                                   moduleColor = moduleColors,
                                   geneTraitSignificance,
                                   GSPvalue)
            
            oldName <- names(geneInfo)
            
            geneInfo <- data.frame(geneInfo, 
                                   geneModuleMembership[, modOrder[mod]],
                                   MMPvalue[, modOrder[mod]])
            
            names(geneInfo) <- c(oldName, 
                                 paste("MM.", modNames[modOrder[mod]], sep = ""), 
                                 paste("p.MM.", modNames[modOrder[mod]], sep = ""))
        }
        
        # Order the genes in the geneInfo variable first by module color, then by 
        # geneTraitSignificance 
        geneOrder <- order(geneInfo$moduleColor, 
                           -abs(geneInfo[3])) 

        geneInfo <- geneInfo[geneOrder, ]
        # Store individual geneInfo into a list 
        geneInfo_list[i] <- list(geneInfo)
        
        # Export the output 
        write.csv(geneInfo, file = file)
    }
    
    return(geneInfo_list)

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



# debug(tabulate.intraModule)

testing <- tabulate.intraModule(mSetObj, net = net)

# undebug(tabulate.intraModule)









