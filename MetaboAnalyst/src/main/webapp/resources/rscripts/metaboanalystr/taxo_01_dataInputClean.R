#'Make the gene expression object
#'@description Store single set of raw data in a list for subsequent analysis
#'@param mSetObj Input name of the created mSet Object
#'@param custom_norm Customized normalization provided by users 
#'@param nSets Number of sets of expression data, default is 1
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
make.exprSet <- function(mSetObj = NA, 
                         custom_norm = "false", 
                         nSets = 1){ 
    # Load WGCNA library 
    library(WGCNA) 
    library(tidyverse)
    source("./WGCNA_functions/taxo_00_generalDataUtils.R")  
    
    # Source the file, general_data_utils.R 
    mSetObj <- .get.mSet(mSetObj)
    # Create an empty list vector to store gene expression data 
    multiExpr <- base::vector(mode = "list", 
                              length = nSets) # Indicates the set number
    
    # For a single set of gene expression data (genes X samples) 
    multiExpr[[1]] <- list(data = as.data.frame(t(mSetObj$dataSet$orig)))
    names(multiExpr[[1]]$data) <- rownames(mSetObj$dataSet$orig) # Feature by column 
    rownames(multiExpr[[1]]$data) <- names(mSetObj$dataSet$orig) # Sample by row 
    
    # Assign the resulting object to mSet 
    mSetObj$dataSet$exprObj <- multiExpr 

    .set.mSet(mSetObj) # Return the resulting mSet
}


#===============================================================================

# Validate the above self-defined function using toy data 

#===============================================================================
# # Load the toy data 
load("./WGCNA_data/mSet_example.RData")
# 
# # Call my function 
.on.public.web <- FALSE
# 
mSetObj <- make.exprSet(mSetObj = mSetObj_example)
# 
str(mSetObj)




