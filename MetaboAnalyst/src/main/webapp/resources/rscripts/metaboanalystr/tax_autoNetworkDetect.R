#'Find the soft threshold 
#'@description Assist users graphically in finding optimal soft threshold for network co-expression analysis based on relationship between free-scale topology, mean connectivity and soft thresholds 
#'@param mSetObj Input name of the created mSet Object
#'@param custom_norm Customized normalization provided by users 
#'@param powerVector Vector of soft threshold values
#'@param imgName Image name 
#'@param format Select the image format, "png" or "pdf". Default is "png" 
#'@param dpi Define the resolution. If the image format is "pdf", users do not need define the dpi. For "png" format, the default dpi is 72. It is suggested that for high-resolution images, choose a dpi of 300. 
#'@param width Define image sizes, there 2 default widths. The first, width = NULL, is 10.5. The second default is width = 0, where the width is 7.2. Otherwise, users can customize widths on their own   
#'@param verbose parameter in function WGCNA::pickSoftThreshold() 
#'@author Xin (David) Zhao\email{xzhao1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
findSoftThreshold <- function(mSetObj = NA, 
                              custom_norm = NA, 
                              powerVector = NULL, 
                              imgName, 
                              format = "png", 
                              dpi = 72, 
                              width = NULL, 
                              verbose = NULL){
                              ## mSetObj = NA # Doesn't correspond to any input type as it uses mSetObj 
                              ## custom_norm = NA # Text box 
                              ## powerVector = NULL # Text box
                              ## verbose = NULL # Static dropdown 
    
    library(WGCNA) 
    library(tidyverse) 
    library(ggplot2) # Load ggplot2 
    library(gridExtra) # Layout multiple ggplot figures 
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

    # Reproduce plots using ggplot2
    # Input data for plotting 
    df_fitIndices <- sft$fitIndices 
    # Create a new column for y-axis in the first plot  
    df_fitIndices <- df_fitIndices %>% 
        dplyr::mutate(plot1_y = -sign(slope)*SFT.R.sq) 
    
    # Define sizes for the final plot 
    if(is.null((width))) {
        w <- 10.5 
    } else if (width == 0) {
        w <- 7.2 
    } else {
        w <- width
    }
    h <- w # height 
    
    # Scale-free topology fit index 
    plot1 <- ggplot2::ggplot(df_fitIndices, 
                             aes(x = Power, y = plot1_y)) +
        geom_point() + 
        geom_label(aes(label = Power),
                   position = "nudge") +
        labs(x = "Soft power thresholds",
             y = "Scale-free topology fit index") + 
        scale_y_continuous(limits = c(0, 1)) + 
        theme_classic()
    # Mean connectivity  
    plot2 <- ggplot2::ggplot(df_fitIndices,
                             aes(x = Power, y = mean.k.)) + 
        geom_point() + 
        geom_label(aes(label = Power),
                   position = "nudge") +
        labs(x = "Soft power thresholds",
             y = "Mean connectivity") + 
        theme_classic() 
    # Arrange two plots side-by-side on one page 
    plots_merged <- gridExtra::grid.arrange(plot1, plot2, nrow = 1) 
    
    # Export plots 
    ggplot2::ggsave(filename = paste("./WGCNA_output/", imgName, ".", format, sep = ""),
                    plot = plots_merged, # Plot to export 
                    device = format,
                    scale = 1,
                    width = w,
                    height = h,
                    units = "cm",
                    dpi = dpi) 

    # Name plot for download 
    imgName_json <- paste(imgName, ".json", sep = "")
    
    imgName_export <- paste(imgName, "dpi", dpi, ".", format, sep = "")
    # Save the resulting figure file name to mSet object 
    mSetObj$imgSet$softThreshold <- imgName_export 
    
    .set.mSet(mSetObj)
} 


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
findSoftThreshold(mSetObj = mSetObj, imgName = "softThresholdggPlots")   
# undebug(findSoftThreshold)
