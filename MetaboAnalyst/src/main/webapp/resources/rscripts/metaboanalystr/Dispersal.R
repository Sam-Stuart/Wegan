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
    print("------------######---------bgdispersalWegan------------#######----------");
    
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
    print(mat);
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
    print( "    GetBGDSigMat function123: ");
  
    if(tableName == 'bgd1'){
        print(' Row AFFRIMATIVE ')
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

    print("GetBGDSigRowNames R");
    print(tableName);
    if(tableName == 'bgd1'){
        print(' Row AFFRIMATIVE ')
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
    print("GetBGDSigColNames");
    mSetObj <- .get.mSet(mSetObj);
    print(tableName);   
  
    if(tableName == 'bgd1'){
        print(' Row AFFRIMATIVE ')
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
    print("beals Wegan 1");
    mSetObj <- .get.mSet(mSetObj);
   
    # Call upon the beals smoothing function 
    # default beals
    output <- beals(mSetObj$dataSet$orig)
    
    # store the item to the bgdispersal object
    mSetObj$analSet$beals <- output;
    
    return(.set.mSet(mSetObj));
}


PlotBeals <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcnum=0){
  print(" -------------------------PLOT Beals  ------------------");
  mSetObj <- .get.mSet(mSetObj);
  
  beals_matrix <- as.matrix(mSetObj$analSet$beals);
    
  print("plot beals 1");  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  
  print("plot beals 2");
  mSetObj$imgSet$defaultbeals<- imgName;
  print("plot beals 3");
  print(dim(mSetObj$dataSet$orig));

  pa <- decostand(mSetObj$dataSet$orig, "pa");

  print("plot beals 5");
  print(dim(beals_matrix));
  h <- w;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  boxplot(as.vector(beals_matrix) ~ unlist(pa), xlab="Presence", ylab="Beals");

  print("plot beals 6");
  dev.off();
  return(.set.mSet(mSetObj));
}


####### BETA DISPERSAL ###### 

betadisperWegan <- function(mSetObj=NA){
    print("betaDispersal Wegan 1");
    mSetObj <- .get.mSet(mSetObj);
       
    data <- mSetObj$dataSet$orig
    print(dim(data));
    print(data);
    #data(varespec);
    #data <- varespec;
    print(" ########## BETA 2");
    print(dim(data));
    ## Bray-Curtis distances between samples
    dis <- vegdist(data);
    print(dis);
    ## First 16 sites grazed, remaining 8 sites ungrazed
    groups <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed"))
    print(groups);
    ## Calculate multivariate dispersions
    mod <- betadisper(dis, groups)
    print(mod)
    print(class(mod));
    
    # store the item to the bgdispersal object
    mSetObj$analSet$betadisper <- mod;
    
    return(.set.mSet(mSetObj));
}


PlotBetaDisper <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcnum=0){
  print(" -------------------------PLOT Beta Dispersal  ------------------");
  mSetObj <- .get.mSet(mSetObj);
  
  mod <- mSetObj$analSet$betadisper;
    
  print("plot beta disper 1");  
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  
  print("plot beta 2");
  print(imgName);
  mSetObj$imgSet$defaultbetadisper<- imgName;
  print("plot beta 3");
  
   
  print(class(mod));
  
  print(dim(mod));
  h <- w;
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  plot(mod);
  print("plot beta 6");
  dev.off();
  return(.set.mSet(mSetObj));
}
