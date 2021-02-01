#'Perform Dispersal analysis
#'@description Perform Dispersal analysis, i.e bgdispersal, beals smoothing , beta dispersal and s-diva dispersal
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'@param mSetObj Input name of the created mSet Object
#'University of Alberta, Canada
#'License: 
#'@export
#'


library(Cairo)
library(vegan)

# -------------Coefficients of Biogeographical Dispersal Direction--------------------------------------------------


bgdispersalWegan <- function(){
    print("bgdispersal function activated")
    mSetObj <- .get.mSet(mSetObj);
    input <- mSetObj

#   if(input=='BCI'){
#       data(BCI);
#        mat <- BCI;
#   }else if(input =='dune'){
#       data(dune);
#        mat <- dune;
#    }else{
#        mat <- mSetObj$dataSet$norm
#    }
    # Calculate the bg dispersal
    output <- bgdispersal(mat)

    # store the item to the bgdispersal object
    mSetObj$analSet$bgdispersal <- output;

    return(.set.mSet(mSetObj));
}

bsmoothWegan <- function(){
    
    mSetObj <- .get.mSet(mSetObj);
    input <- mSetObj

#   if(input=='BCI'){
#       data(BCI);
#        data <- BCI;
#   }else if(input =='dune'){
#       data(dune);
#        data <- dune;
#    }else{
#        data <- mSetObj$dataSet$norm
#    }
    # Calculate the bg dispersal
    output <- beals(data)

    # store the item to the bgdispersal object
    mSetObj$analSet$bgdispersal <- output;

    return(.set.mSet(mSetObj));
}