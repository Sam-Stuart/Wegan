#'Find the soft threshold 
#'@description Assist users graphically in finding optimal soft threshold for network co-expression analysis based on relationship between free-scale topology, mean connectivity and soft thresholds 
#'@param mSetObj Input name of the created mSet Object
#'@param custom_norm Customized normalization provided by users 
#'@param powerVector vector of soft threshold values
#'@param file string indicates image file name
#'@param verbose parameter in function WGCNA::pickSoftThreshold() 
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
findSoftThreshold <- function(mSetObj = NA, 
                              custom_norm = NA, 
                              powerVector = NULL,
                              file,
                              verbose = NULL){
    
    library(WGCNA) 
    library(tidyverse) 
    source("./WGCNA_functions/taxo_00_generalDataUtils.R")  
    
    mSetObj <- .get.mSet(mSetObj) 
    
    # Check if it contains the required element 
    if(is.null(mSetObj$dataSet$exprObj)) {
        stop("mSet object does not contain required element for the analysis.")
    }
    
    # Choose the optimal soft-threshold power
    if(is.null(powerVector)){
        powers <- c(c(1:10), seq(from = 12, to = 20, by = 2))
    } else {
        powers <- powerVector
        }
    
    datExpr <- mSetObj$dataSet$exprObj[[1]]$data 

    # Network topology analysis
    sft <- WGCNA::pickSoftThreshold(datExpr, 
                                    powerVector = powers, 
                                    verbose = 5)
    
    
    # Plot for visual investigation of soft thresholds 
    pdf(file) # PDF device 
    par(mfrow = c(1, 2)) # Layout of two plots
    cex1 <- 0.9 
    # Scale-free topology fit index vs Soft-threshold power 
    plot(sft$fitIndices[, 1], 
         -sign(sft$fitIndices[, 3])*sft$fitIndices[, 2], 
         type = "n") # for no plotting 
    text(sft$fitIndices[, 1], 
         -sign(sft$fitIndices[, 3])*sft$fitIndices[, 2],
        labels = powers, 
        cex = cex1, 
        col = "red") 
    abline(h = 0.90, 
           col = "red") 
    
    # Mean connectivity vs Soft-threshold power 
    plot(sft$fitIndices[, 1], 
         sft$fitIndices[, 5],
        xlab = "Soft threshold (power)", 
        ylab = "Mean connectivity", 
        type = "n", # for no plotting 
        main = paste("Mean connectivity")) 
    text(sft$fitIndices[, 1], 
         sft$fitIndices[, 5], 
         labels = powers, 
         cex = cex1, 
         col = "red") 
    
    dev.off()
    
    # ggplot - scatter plots side-by-side 
    
    
    
    
    
    # Save the resulting figure file name to mSet obejct 
    mSetObj$imgSet$softThreshold <- file
    
    .set.mSet(mSetObj)

} 


# To code for module-detection function after obtaining optimal soft threshold ...


#===============================================================================

# Validate the resulting function 

#===============================================================================

load("./WGCNA_data/mSet_example.RData")
source("./WGCNA_functions/taxo_01_dataInputClean.R") 

# Call my function 
.on.public.web <- FALSE 

mSetObj <- make.exprSet(mSetObj = mSetObj_example)  

# Call the new function 
# debug(findSoftThreshold)
findSoftThreshold(mSetObj = mSetObj, file = "./WGCNA_output/softhreshold.pdf")  
# undebug(findSoftThreshold)



