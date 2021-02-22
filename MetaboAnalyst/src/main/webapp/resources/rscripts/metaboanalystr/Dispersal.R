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
    print("-----------------------------bgdispersalWegan function activated-1-2-3---------------------------------");
    mSetObj <- .get.mSet(mSetObj);
    #print(mSetObj$dataSet$orig);

    #print(class(mSetObj));
    #print("names of mSetobj: ");
    #print(names(mSetObj));
    #print("names of $dataSet: ");
    #print(names(mSetObj$dataSet));
    #print("names of $analSet: ");
    #print(names(mSetObj$analSet));
    # Calculate the bg dispersal
    output <- bgdispersal(mSetObj$dataSet$orig)
    print(" MsetObj$dataSet$norm  :  ") ;
    print(mSetObj$dataSet$norm);
    
    # store the item to the bgdispersal object
    mSetObj$analSet$bgdispersal <- output;
    
    

    return(.set.mSet(mSetObj));
}


PlotBGD <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, bgdnum){
    print("----------------PlotBGD--------------------------------------");
    

    bgdnum = bgdnum;
    mSetObj <- .get.mSet(mSetObj);

    # Check which matrix to plot : 
    if (imgName == "bgd1_0_"){
        print("------DD1 CHOSEN------");
        mat <- as.matrix(mSetObj$analSet$bgdispersal$DD1);
        
    }else if (imgName == "bgd2_0_"){
        print("------DD2 CHOSEN------");
        mat <-mSetObj$analSet$bgdispersal$DD2 ;
        
    }else {
        mat <-mSetObj$analSet$bgdispersal$DD3
        print("------DD3 CHOSEN------");
        }
    
    print(" ----------------Image name : --------------------------------");
      print(imgName);
    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    if(is.na(width)){
      w <- 10;
    }else if(width == 0){
      w <- 8;
    }else{
      w <- width;
    }
    h <- w;

    mSetObj$imgSet$dispersal.bgd1 <- imgName;
    # Use the Cairo package to plot the data
    Cairo::Cairo(file = imgName, unit="in",width=w, height=h, dpi=dpi, type=format, bg="white");
    print(mat);
    dev.off();
    return(.set.mSet(mSetObj));
}

#'BGD matrix
#'@description Return a matrix of values from the bgdispersal output
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Leif Wilm (lwilm@ualberta.ca)
#'McGill University, Canada
#'License: GNU GPL (>= 2)

GetBGDSigMat <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  print( "    GetBGDSigMat function: ");
  cat(CleanNumber(mSetObj$analSet$bgdispersal$DD1));
  return(CleanNumber(mSetObj$analSet$bgdispersal$DD1));
}

GetBGDSigRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  rownames(mSetObj$analSet$bgdispersal$DD1);
}

GetBGDSigColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  colnames(mSetObj$analSet$bgdispersal$DD1);
}

GetBGDSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$bgdispersal$DD1;
}



## Beals smoothing function 
bsmoothWegan <- function(){
    
    mSetObj <- .get.mSet(mSetObj);
    

#   
    # Call upon the beals smoothing function
    output <- beals(mSetObj)

    # store the item to the bgdispersal object
    mSetObj$analSet$bsmooth <- output;

    return(.set.mSet(mSetObj));
}