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


bgdispersalWegan <- function(mSetObj=NA){
    print("-----------------------------bgdispersal function activated------------------------------------");
    mSetObj <- .get.mSet(mSetObj);
    print(mSetObj$dataSet$orig);

    print("----------------mSetObj called--------------------------------------");
    print(class(mSetObj));
    print("names of mSetobj: ");
    print(names(mSetObj));
    print("names of $dataSet: ");
    print(names(mSetObj$dataSet));
    print("names of $analSet: ");
    print(names(mSetObj$analSet));
    # Calculate the bg dispersal
    output <- bgdispersal(mSetObj$dataSet$orig)

    print("bgdispersal function implemented");

    # store the item to the bgdispersal object
    mSetObj$analSet$bgdispersal <- output;
    print("------------------ mSetObj$analSet$bgdispersal : -----------------------------");
    print(mSetObj$analSet$bgdispersal);
    

    return(.set.mSet(mSetObj));
}


PlotBGD <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, bgdnum){
  bgdnum = bgdnum;
  mSetObj <- .get.mSet(mSetObj);
  
  DD1 <-mSetObj$analSet$bgdispersal$DD1;

  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w;
  
  mSetObj$imgSet$dispersal.bgd <- imgName;

  Cairo::Cairo(file = imgName, unit="in",width=w, height=h, dpi=dpi, type=format, bg="white");
  plot(DD1);
  dev.off();
  return(.set.mSet(mSetObj));
}
bsmoothWegan <- function(){
    
    mSetObj <- .get.mSet(mSetObj);
    

#   
    # Call upon the beals smoothing function
    output <- beals(mSetObj)

    # store the item to the bgdispersal object
    mSetObj$analSet$bsmooth <- output;

    return(.set.mSet(mSetObj));
}