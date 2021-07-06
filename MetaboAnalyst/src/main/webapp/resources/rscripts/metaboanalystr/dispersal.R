#'Perform Dispersal analysis
#'@description Perform Dispersal analysis, i.e bgdispersal, beals smoothing , beta dispersal and s-diva dispersal
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'@param mSetObj Input name of the created mSet Object
#'University of Alberta, Canada
#'License: 
#'@export
#'


library(gt)

# -------------Coefficients of Biogeographical Dispersal Direction-------------------------------------------------- 


bgdispersalWegan <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
    
    # Calculate the bg dispersal
    output <- bgdispersal(mSetObj$dataSet$orig);

       
    # Store the item to the bgdispersal object
    mSetObj$analSet$bgdispersal <- output; 
     
    
    return(.set.mSet(mSetObj));
}


PlotBGD <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, bgdnum){
    

    mSetObj <- .get.mSet(mSetObj);
    

    # Check which matrix to plot : 
    if (imgName == "bgd1_0_"){
        mat <- mSetObj$analSet$bgdispersal$DD1;
        mat <- data_check(mat);
        write.csv(mat, file = "bgd_DD1.csv");
    }else if (imgName == "bgd2_0_"){
        mat <- mSetObj$analSet$bgdispersal$DD2;
        mat <- data_check(mat);
        write.csv(mat, file = "bgd_DD2.csv");
    }else if (imgName == "bgd3_0_"){
        mat <- mSetObj$analSet$bgdispersal$DD3;
        mat <- data_check(mat);
        write.csv(mat, file = "bgd_DD3.csv");
    }else if (imgName == "bgd4_0_"){
        mat <- mSetObj$analSet$bgdispersal$DD4;
        mat <- data_check(mat);
        write.csv(mat, file = "bgd_DD4.csv");
    }else if (imgName == "bgd5_0_"){
        mat <- mSetObj$analSet$bgdispersal$McNemar;
        write.csv(mat, file = "bgd_McNemar.csv");
    }else if (imgName == "bgd6_0_"){
        mat <-mSetObj$analSet$bgdispersal$prob.McNemar;
        write.csv(mat, file = "bgd_ProbMcNemar.csv");
    }
    
    mat <- as.data.frame(round(mat,2));
    
    gt_mat <- gt(mat) %>% tab_options(table.font.size = 6); 
    
    # Fix the Formatting says Dana
    if(is.na(width)){
        w <- 10;
    }else if(width == 0){
        w <- 8;
    }else{
        w <- width;
    }

    # save the image name
    

    imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
    mSetObj$imgSet$dispersal$bgd1 <- imgName;

    # Use the gt package to plot the data
    gtsave(gt_mat, filename = imgName)
    #dev.off();
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
bealsWegan <- function(mSetObj=NA, spcs = 'NA', ref = 'NA', type = 0, incld = TRUE){
    mSetObj <- .get.mSet(mSetObj);
    
    # Call upon the beals smoothing function 
    data <- mSetObj$dataSet$orig;
    # alter species if needed 

    if((spcs == 'NA')||(tolower(spcs)=='all')){
        spcs = NA
    } 
    if (incld =='TRUE'){
        incld = TRUE
    } else if (incld == 'FALSE'){
        incld = FALSE
    } else {
        print ("ERROR with INCLUDE argument");
    }
    
    # call upon beals vegan function
    
    output <- beals(data, species = spcs, type = type, include = incld)
    
    
    # save the new data set in the mSetObject
    mSetObj$analSet$beals <- output;
    
    #write csv of new data set
    write.csv(output, file = "beals_matrix.csv");
    return(.set.mSet(mSetObj));
}


PlotBeals <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA, pcnum=0, species = 'NA'){
  
  mSetObj <- .get.mSet(mSetObj);
  
  beals_matrix <- as.matrix(mSetObj$analSet$beals);
  # Save Image name
  imgName = paste(imgName, "dpi", dpi, ".", format, sep="");
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
 
  mSetObj$imgSet$defaultbeals<- imgName;
 # Use decostand
  if ((species == 'NA')||(tolower(species) =='all')){
    pa <- decostand(mSetObj$dataSet$orig, "pa");
  } else {
    pa <- decostand(mSetObj$dataSet$orig[,species],"pa");
  }

  h <- w;
  # plot boxplot
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  boxplot(as.vector(beals_matrix) ~ unlist(pa), xlab="Presence", ylab="Beals");

  dev.off();
  return(.set.mSet(mSetObj));
}


####### BETA DISPERSAL ###### 

betadisperWegan <- function(mSetObj=NA){
    mSetObj <- .get.mSet(mSetObj);
       
    data <- mSetObj$dataSet$orig
    data_type <- mSetObj$dataSet$type;
    
    
    ## Bray-Curtis distances between samples
    dis <- vegdist(data);
    ## First 16 sites grazed, remaining 8 sites ungrazed
    if (data_type == 'Varespec'){
        groups <- factor(c(rep(1,16), rep(2,8)), labels = c("grazed","ungrazed"))
    }else if (data_type == 'Dune'){
        
        groups <- factor(c(rep(1,10), rep(2,10)), labels = c("group A","group B"))
    }
    ## Calculate multivariate dispersions
   
    mod <- betadisper(dis, groups)
    
    
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


### Other Functions : 

# Get columns : 

disp.reg.columns <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  
  data <- select_if(mSetObj$dataSet$orig, is.numeric)
  count.all.numeric.cols <- ncol(data)
  name.all.numeric.cols <- colnames(data)
  
  num.col.results <- list(
    count=count.all.numeric.cols,
    names=name.all.numeric.cols
  )
  return(name.all.numeric.cols)
  
}

# Check that the first data rows and columns are not repeating 

data_check <- function(mat){
    # check first column
   
    mat <- as.matrix(mat)
    
    if (mat[1,1] == mat[1,2] && mat[2,1] == mat[2,2] && mat[3,1] == mat[3,2]){
        mat <- mat[,-c(1)];
    }
    # check first row
    if (mat[1,1] == mat[2,1] && mat[1,2] == mat[2,2] && mat[1,3] == mat[2,3]){
        mat <- mat[-c(1),]
    }

    # rename columns and rows 
    rownames(mat) <- 1:dim(mat)[1]
    colnames(mat) <- 1:dim(mat)[2]

    return (mat)
}