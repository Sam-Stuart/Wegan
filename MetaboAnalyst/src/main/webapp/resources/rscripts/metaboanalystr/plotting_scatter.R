#scatterPlot_plotting
library(vegan)
print("We are priting in Scatter plot, outside the functions")
##### SCATTER PLOT ####
#'Scatter Plot'
#'@description Produce scatter plot components based on user data and preferences
#'@usage scatterPlot_setup(mSetObj = NA, facA=NULL, facB=NULL, type = NULL, 
#'# line_color = "red", xlab = 'x axis',ylab = 'y axis', maintitle = 'Title')
#'@param mSetObj Input the name of the created mSetObj
#'@param facA list of independent variables
#'@param facB list of dependent variables
#'@param type type of (if any) trend line to be used. NULL = no trendline; 'lbf' = line of best fit ; 'lowess' = locally weighted trend line
#'@param line_color color of line of best fit.
#'@param xlab x axis title. NULL will choose column name
#'@param ylab y axis title, NULL will choose column name
#'@param maintitle graph title 
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
#'

scatterPlot_setup <- function(mSetObj = NA, facA=NULL, facB=NULL, type = NULL, line_color = "red", xlab = NULL,
                              ylab = NULL, maintitle = 'Title'){
  mSetObj <- .get.mSet(mSetObj)
  print("inside scatterPlot_setup function");
  #Set independent variable name
  if (is.null(facA)){
    facA <- colnames(mSetObj$dataSet$norm)[1]; #Default is first column.
  } 
  #Set dependent  variable name
  if (is.null(facB)){
    facB <- colnames(mSetObj$dataSet$norm)[2];#Default is second column.
  } 
  
  #Variable type check
  if (is.factor(mSetObj$dataSet$norm[,facA] || mSetObj$dataSet$norm[,facB])==TRUE){
    #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
    stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
  }
  
  #Define formula
  formula <- as.formula(paste0(facB, "~", facA)) ;
  # x axis label
  if (is.null(xlab) & is.character(facA)){
    xlab <- facA
  }
  # y axis label
  if (is.null(ylab) & is.character(facB)){
    ylab <- facB
  }
  # set up data
  if (is.numeric(rownames(mSetObj$dataSet$norm))){
    data <- mSetObj$dataSet$norm[order(as.numeric(rownames(mSetObj$dataSet$norm))),drop=FALSE];
  }  else if (is.character(rownames(mSetObj$dataSet$norm))){
    data <- mSetObj$dataSet$norm
  }
  
  
  # save properties to object 
  mSetObj$analSet$scatterPlot <- list(data = data, facA = facA, facB = facB,
                                      formula = formula, type=type, line_color = line_color,
                                      xlab=xlab, ylab=ylab, maintitle=maintitle);
  return(.set.mSet(mSetObj));
  
}

#'Produce a Scatter plot'

#'@description uses components provided by scatterPlot_setup to produce a scatter plot
#'@usage plotScatterPlot(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width.   
#'@author  Leif Wilm\email{lwilm@ualberta.ca} 
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export  
# PlotPieChart function calls upon the pieChart data set up by the pieChart_setUp function



plotScatterPlot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj)

  # call upon graph parameters
  data <- mSetObj$analSet$scatterPlot$data;
  facA <- mSetObj$analSet$scatterPlot$facA;
  facB <- mSetObj$analSet$scatterPlot$facB;
  formula <- mSetObj$analSet$scatterPlot$formula;
  type <- mSetObj$analSet$scatterPlot$type;
  line_color <- mSetObj$analSet$scatterPlot$line_color;
  xlab <- mSetObj$analSet$scatterPlot$xlab;
  ylab <- mSetObj$analSet$scatterPlot$ylab;
  maintitle <- mSetObj$analSet$scatterPlot$maintitle;
  
  
  #Set plot dimensions 
  if(is.na(width)){
    w <- 10;
  }else if(width == 0){
    w <- 8;
  }else{
    w <- width;
  }
  h <- w;
  
  #Name plot for download
  imgName <- "test"
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$scatterPlot <- imgName;
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  plot <- plot(formula = formula, data = data,pch = 19, xlab = xlab, ylab = ylab, main = maintitle);
  
  # Add line of best fit if requested
  if(!is.null(type)){
    attach(data)
    if(type == 'lbf'){# Line of best fit
      abline(lm(formula, data = data), col = line_color);
    } else if (type == 'lowess'){ # Locally Weighted Scatterplot Smoothing
      lines(loess(formula, data = data)$fitted , col = line_color);
      
    }
    detach(data)
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
  
}




