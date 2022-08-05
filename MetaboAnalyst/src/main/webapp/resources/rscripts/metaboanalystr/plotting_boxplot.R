library("ggplot2")
library("dplyr")
library("viridis")
library("stringr")
library("vegan")



boxPlot_setup <- function(mSetObj = NA, 
                          facA = "NULL", facB = "NULL", facC = "NULL",
                          fillColor = "NULL",
                          xlab = "NULL", ylab = "NULL",
                          legendTitle = "Legend Title", mainTitle = "Main Title",
                          data="NULL"){
  mSetObj <- .get.mSet(mSetObj)

  #AddErrMsg("This is an AddErrMsg message");
    

  if (facA != "NULL"){
      if (facA == facB){
          AddErrMsg("Data selections cannot be the same");
          stop("Cannot select same data columns for the boxplot.")
      }
  }
  
  

  if (data=="NULL") {
    input <- mSetObj$dataSet$orig
  } else {
    input <- mSetObj$dataSet$orig
  }



  #set up data
  data_fac <- select_if(input, is.character) #factor columns for grouping (using 1 or 2)
  data_num <- select_if(input, is.numeric) #numeric data for box plot (using 1)

  
  #Data check
  if (ncol(data_num)<1) {
    print("Your data set has 0 numeric variables.")
    
  }
  
  if (ncol(data_fac)<1) {
    print("Your data set has 0 grouping variables.")
    
  }
  
  #Set independent variable name
  if (facA=="NULL") {
    facA <- colnames(data_fac)[1]# Default is to choose the first factor column as response column. otherwise facB determined by function factor.columns() below
  } 
  
  #Set dependent variable name
  if (facB=="NULL") {
        facB <- colnames(data_num)[1]# Default is to choose the first numeric column as response column. otherwise facB determined by function numeric.columns() below
  } 
  
  #Set grouping data variable name
  if (facC=="NULL") {
    if (length(colnames(data_fac))>1) {
      facC <- colnames(data_fac)[2]# Default is to choose the second factor column as grouping column. otherwise facB determined by function factor.columns() below
    } 
  } 
  
  # Set x axis label if null
  if (xlab == "NULL"){
    if(facA == "NULL" ){
      xlab <- "Independent Variable"
    } else {
      xlab <- facA
    }
  }
  
  # set y axis label if null
  if (ylab == "NULL"){
    if(facB == "NULL"){
      ylab <- "Dependent Variable"
    } else {
      ylab <- facB
    }
  }
  
  ###SET GROUPING LEGEND TITLE###
  if (legendTitle=="NULL"){
    legendTitle <- " "
  }
  
  #Set variable data 
  facA_data <- data_fac[,facA] #factor
  facB_data <- data_num[,facB]  #numeric
  
  # set grouping data - if exists 
  if (facC=="NULL") { 
    facC_data <- NULL
  } else {
    facC_data <- data_fac[,facC]  #factor   
  } 
  
  facA_dim <- length(unique(facA_data)) # number of groups in facA
  facC_dim <- length(unique(facC_data)) # number of groups in facC
    
  # select Color if no fill variable :
  if (facC == "NULL"){ #default no 2nd factor var
    if (fillColor == "NULL"){ #default fill color
      pallete <- viridis(facA_dim)
    } else if (fillColor != "NULL"){
      if (fillColor == 'r'){
        pallete <- rainbow(facA_dim)
      } else if (fillColor == 'v'){
            pallete <- viridis(facA_dim)
      } else if (fillColor == 'p'){
            pallete <- plasma(facA_dim) 
      } else if (fillColor == 'g'){
            pallete <- grey.colors(facA_dim)
      } else if (fillColor == 'b'){
            pallete <- 'lightblue'
      }
    }

    if (length(pallete) >1 & length(pallete) != facA_dim){
      pallete <- "NULL"
      stop("dimensions of color must be 1 or eqaul to grouping dimensions (",facA_dim, ")" )
    }
  } else if(facC[1] != "NULL"){# option where fill variable is present (facC)
    if (fillColor=="NULL") {
      pallete <- viridis(facC_dim)
    } 
    if(fillColor != "NULL"){
        if (fillColor == 'r'){
          pallete <- rainbow(facC_dim)
        } else if (fillColor == 'v'){
          pallete <- viridis(facC_dim)
        } else if (fillColor == 'p'){
          pallete <- plasma(facC_dim+1) #Add 1 to remove light yellow
        } else if (fillColor == 'g'){
          pallete <- grey.colors(facC_dim)
        } 
      if (length(pallete) > 1 & length(pallete) != facC_dim){
        pallete <- "NULL"
        stop("dimensions of color must be eqaul to grouping dimensions (",facC_dim, ")" )
      }
    }
  }
 
  # save properties to object
  mSetObj$analSet$boxPlot <- list(data = input, facA_data = facA_data, facB_data = facB_data, facC_data = facC_data,
                                  pallete = pallete, xlab=xlab, ylab=ylab, legendTitle = legendTitle, mainTitle=mainTitle);
  
  return(.set.mSet(mSetObj));
}

plotBoxPlot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj)
 
  # call upon graph parameters
  input <- mSetObj$analSet$boxPlot$data;
  facA_data <- mSetObj$analSet$boxPlot$facA_data;
  facB_data <- mSetObj$analSet$boxPlot$facB_data;
  facC_data <- mSetObj$analSet$boxPlot$facC_data;

  pallete <- mSetObj$analSet$boxPlot$pallete;
  xlab <- mSetObj$analSet$boxPlot$xlab;
  ylab <- mSetObj$analSet$boxPlot$ylab;
  mainTitle <- mSetObj$analSet$boxPlot$mainTitle;
  legendTitle <- mSetObj$analSet$boxPlot$legendTitle;



  # Set plot dimensions
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w;
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$boxPlot <- imgName;
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  if (is.null(facC_data)){ # No fill variable
    p0 <- ggplot(input, aes(x=facA_data, y=facB_data)) +
            geom_boxplot(fill = pallete) + labs(title = mainTitle, x = xlab, y = ylab) +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_text(size=12, colour="black"), 
            axis.text.y=element_text(size=12, colour="black"), legend.text=element_text(size=12), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12), legend.title=element_text(size=12), plot.title=element_text(hjust = 0.5))
    show(p0)
  } else { #yes fill variable
    p0 <- ggplot(input, aes(x=facA_data, y=facB_data, fill = facC_data)) +
            geom_boxplot() + labs(title = mainTitle, x = xlab, y = ylab, fill = legendTitle) +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x=element_text(size=12, colour="black"), 
            axis.text.y=element_text(size=12, colour="black"), legend.text=element_text(size=12), axis.title.x=element_text(size=12),
            axis.title.y=element_text(size=12), legend.title=element_text(size=12), plot.title = element_text(hjust = 0.5)) + theme_bw()
    if(fillColor!="NULL"){
      p0 <- p0 + scale_fill_manual(values = pallete)
    }
    show(p0)
  }

 dev.off();
   
  return(.set.mSet(mSetObj));
}



factor.columns <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj)
  data <- mSetObj$analSet$boxPlot$data;
  data_fac <- select_if(data, is.character) #factor columns for grouping
  if(is.null(data_fac)){
    fac.col.names <- "No Grouping Data in this dataset";
    return(fac.col.names);
  }
  fac.col.names <- colnames(data_fac)
  return(fac.col.names)
}



numeric.columns <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj)
  data <- mSetObj$analSet$boxPlot$data;
  data_num <- select_if(data, is.numeric) #numeric columns for dependent variable
  num.col.names <- colnames(data_num)
  return(num.col.names)
}

all.columns <- function(mSetObj=NA) {
  mSetObj <- .get.mSet(mSetObj)
  data <- mSetObj$dataSet$orig;
  col.names <- colnames(data)
  return(col.names)
}