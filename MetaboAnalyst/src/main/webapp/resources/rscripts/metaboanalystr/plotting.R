#'Plotting Module R scripts 
#'@description Perform Dispersal analysis, i.e bgdispersal, beals smoothing , beta dispersal and s-diva dispersal
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'@param mSetObj Input name of the created mSet Object
#'University of Alberta, Canada
#'License: 
#'@export
#'


library(Cairo)
library(vegan)

#####  Linear Graphs ##### 

linearGraph <- function(mse

bgdispersalWegan <- function(mSetObj=NA){
    
    mSetObj <- .get.mSet(mSetObj);
    
    # Calculate the bg dispersal
    output <- bgdispersal(mSetObj$dataSet$orig);
    
    # Store the item to the bgdispersal object
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
    dev.off();
    return(.set.mSet(mSetObj));
}

#'BGD matrix
#'@description Return a matrix of values from the bgdispersal output
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@author Leif Wilm (lwilm@ualberta.ca)
#'UofA, Canada
#'License: GNU GPL (>= 2)

GetBGDSigMat <- function(mSetObj=NA, tableName){
  
    mSetObj <- .get.mSet(mSetObj);
  
    if(tableName == 'bgd1'){
        return(CleanNumber(mSetObj$analSet$bgdispersal$DD1));
    }else if(tableName == 'bgd2'){
        return(CleanNumber(mSetObj$analSet$bgdispersal$DD2));
    }else if(tableName == 'bgd3'){
        return(CleanNumber(mSetObj$analSet$bgdispersal$DD3));
    }else if(tableName == 'bgd4'){
        return(CleanNumber(mSetObj$analSet$bgdispersal$DD4));
    }else if(tableName == 'bgd5'){
        zero_mat <- mSetObj$analSet$bgdispersal$McNemar
        zero_mat[is.na(zero_mat)] <- 0
        return(CleanNumber(zero_mat));
    }else if(tableName == 'bgd6'){
        return(CleanNumber(mSetObj$analSet$bgdispersal$prob.McNemar));
    }
}

GetBGDSigRowNames <- function(mSetObj=NA, tableName){
    mSetObj <- .get.mSet(mSetObj);

    if(tableName == 'bgd1'){
        rownames(mSetObj$analSet$bgdispersal$DD1);
    }else if(tableName == 'bgd2'){
        rownames(mSetObj$analSet$bgdispersal$DD2);
    }else if(tableName == 'bgd3'){
        rownames(mSetObj$analSet$bgdispersal$DD3);
    }else if(tableName == 'bgd4'){
        rownames(mSetObj$analSet$bgdispersal$DD4);
    }else if(tableName == 'bgd5'){
        rownames(mSetObj$analSet$bgdispersal$McNemar);
    }else if(tableName == 'bgd6'){
        rownames(mSetObj$analSet$bgdispersal$prob.McNemar);
    }


  rownames(mSetObj$analSet$bgdispersal$DD1);
}

GetBGDSigColNames <- function(mSetObj=NA, tableName){
    mSetObj <- .get.mSet(mSetObj);
    
    if(tableName == 'bgd1'){
        colnames(mSetObj$analSet$bgdispersal$DD1);
    }else if(tableName == 'bgd2'){
        colnames(mSetObj$analSet$bgdispersal$DD2);
    }else if(tableName == 'bgd3'){
        colnames(mSetObj$analSet$bgdispersal$DD3);
    }else if(tableName == 'bgd4'){
        colnames(mSetObj$analSet$bgdispersal$DD4);
    }else if(tableName == 'bgd5'){
        colnames(mSetObj$analSet$bgdispersal$McNemar);
    }else if(tableName == 'bgd6'){
        colnames(mSetObj$analSet$bgdispersal$prob.McNemar);
    }
}

GetBGDSigFileName <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$bgdispersal$DD1;
}



###### Beals smoothing function 
bealsWegan <- function(mSetObj=NA){
    print("Beals Wegan");
    mSetObj <- .get.mSet(mSetObj);
   
    # Call upon the beals smoothing function 
    # default beals
    output <- beals(mSetObj$dataSet$orig)
    
    # store the item to the bgdispersal object
    mSetObj$analSet$beals <- output;
    
    return(.set.mSet(mSetObj));
}


PlotBeals <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcnum=0){
  
mSetObj <- .get.mSet(mSetObj);
  
  beals_matrix <- as.matrix(mSetObj$analSet$beals);
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  
  mSetObj$imgSet$defaultbeals<- imgName;
  pa <- decostand(mSetObj$dataSet$orig, "pa");

  h <- w;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  boxplot(as.vector(beals_matrix) ~ unlist(pa), xlab="Presence", ylab="Beals");

  dev.off();
  return(.set.mSet(mSetObj));
}


####### BETA DISPERSAL ###### 

betadisperWegan <- function(mSetObj=NA){
    print(" beta dispersal");
    mSetObj <- .get.mSet(mSetObj);
       
    data <- mSetObj$dataSet$orig
    data_type <- mSetObj$dataSet$type;
    #data(varespec);
    #data <- varespec;
    print(mSetObj);
    print(mSetObj$dataSet);
    ## Bray-Curtis distances between samples
    dis <- vegdist(data);
    ## First 16 sites grazed, remaining 8 sites ungrazed
    if (data_type == 'Varespec'){
        groups <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed"))
    }else if (data_type == 'Dune'){
        print(" data type is dune");
        groups <- factor(c(rep(1,10), rep(2,10)), labels = c("group A","group B"))
    }
    ## Calculate multivariate dispersions
    print(" made it here");
    mod <- betadisper(dis, groups)
    print(" and here ");
    
    # store the item to the bgdispersal object
    mSetObj$analSet$betadisper <- mod;
    
    return(.set.mSet(mSetObj));
}


PlotBetaDisper <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcnum=0){
  mSetObj <- .get.mSet(mSetObj);
  
  mod <- mSetObj$analSet$betadisper;
    
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  data_type <- mSetObj$dataSet$type;
  title <- paste('Multivariate Dispersions of',data_type);
  mSetObj$imgSet$defaultbetadisper<- imgName;
  h <- w;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(mod, main = title);
  dev.off();
  return(.set.mSet(mSetObj));
}
