# BOXPLOT 

library("ggplot2") 
library("dplyr")
library("viridis")
library("stringr")


#'@description Produce box plot components based on user data and preferences
#'@usage boxPlot_setup(mSetObj = NA, facA=NULL, facB=NULL, type = NULL, 
#'# line_color = "red", xlab = 'x axis',ylab = 'y axis', mainTitle = 'Title')
#'@param mSetObj Input the name of the created mSetObj
#'@param facA data for x axis component, default is first column
#'@param facB data for y axis component, default is second column
#'@param facC data for fill component default is NULL
#'@param type type of (if any) trend line to be used. NULL = no trendline; 'lbf' = line of best fit ; 'lowess' = locally weighted trend line
#'@param fillColor color or palette of fill of boxplots
#'@param xlab x axis title. NULL will choose column name
#'@param ylab y axis title, NULL will choose column name
#'@param boxlabs labels for each boxplot in graph
#'@param legendTitle legend title 
#'@param mainTitle graph title 
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
#'

boxPlot_setup <- function(mSetObj = NA, facA = NULL, facB = NULL, facC = NULL, 
                          type = NULL, fillColor = NULL,
                          xlab = NULL, ylab = NULL, boxlabs = NULL, legendTitle = NULL, mainTitle = NULL){
  mSetObj <- .get.mSet(mSetObj)
  data <- mSetObj$dataSet$norm
  # Set x axis label if null
  if (is.null(xlab)){
    if(!is.null(facA) & is.character(facA)){
      xlab <- facA
    } else {
      xlab <- colnames(data)[1]
    }
  } 
  # set y axis label if null
  if (is.null(ylab)){
    if(!is.null(facB) & is.character(facB)){
      ylab <- facB
    } else {
      ylab <- colnames(data)[2]
    }
  }
  
  #Set x axis variable data
  if (is.null(facA)){
    facA <- (data)[1]; #Default is first column.
  } else {
    facA <- (data[,facA])
  }
  #Set y axis data 
  if (is.null(facB)){
    facB <- (data)[2];#Default is second column.
  } else {
    facB <- data[,facB]
  }
  
  # set fill data - if exists
  if (!is.null(facC)){
    facC <- data[,facC]
  }
  
  # x axis label
  if (is.null(xlab) & is.character(facA)){
    xlab <- facA
  }
  # y axis label
  if (is.null(ylab) & is.character(facB)){
    ylab <- facB
  }
  # box labels
  if (is.null(boxlabs)){
    
  } else if ((length(boxlabs) == 1) & (str_length(boxlabs) > 1)){
    boxlabs <- str_replace_all(boxlabs, "\n", "") # remove and newlines 'enters'
    boxlabs <- str_replace_all(boxlabs, ";", ",") # replace all semi colons with comma's
    boxlabs <- str_split(boxlabs, ",")[[1]] # split string of colors separated by comma's into substrings
} else {
  stop("Unable to format colors correctly")
}
  
  
  
  facA_dim <- length(unique(facA)) # number of groups in facA
  facC_dim <- length(unique(facC)) # number of groups in facC
  # select Color if no fill variable :
  if (is.null(facC)){
    if (is.null(fillColor)){
      fillColor = 'lightblue'
    } else if ((length(fillColor) == 1) & (str_length(fillColor) == 1)){
      if (fillColor == 'r'){
        fillColor <- rainbow(facA_dim)
      } else if (fillColor == 'v'){
        fillColor <- viridis(facA_dim)
      } else if (fillColor == 'p'){
        fillColor <- plasma(facA_dim)
      } else if (fillColor == 'g'){
        fillColof <- grey.colors(facA_dim)
      } else {
        stop("Unknown color palette selected")
      }
    } else if ((length(fillColor) == 1) & (str_length(fillColor) > 1)){
      fillColor <- str_split(fillColor, ",")[[1]] # split string of colors into substrings
      
    }
    
    if (length(fillColor) >1 & length(fillColor) != facA_dim){
        stop("dimensions of color must be 1 or eqaul to grouping dimensions (",facA_dim, ")" )
    
    }
    
  } else if(!is.null(facC)){# option where fill variable is present (facC)
    if(!is.null(fillColor)){
      if ((length(fillColor) == 1) & (str_length(fillColor) == 1)){
        if (fillColor == 'r'){
          fillColor <- rainbow(facC_dim)
        } else if (fillColor == 'v'){
          fillColor <- viridis(facC_dim)
        } else if (fillColor == 'p'){
          fillColor <- plasma(facC_dim)
        } else if (fillColor == 'g'){
          fillColof <- grey.colors(facC_dim)
        } else {
          stop("Unknown color palette selected")
        }
      } else if ((length(fillColor) == 1) & (str_length(fillColor) > 1)){
        fillColor <- str_split(fillColor, ",")[[1]] # split string of colors into substrings
      }
      if (length(fillColor) >1 & length(fillColor) != facC_dim){
        stop("dimensions of color must be eqaul to grouping dimensions (",facC_dim, ")" )
      }
    }
    
    
  }
  
  
  
  # save properties to object 
  mSetObj$analSet$boxPlot <- list(data = data, facA = facA, facB = facB, facC = facC,
                                      type=type, fillColor = fillColor,
                                      xlab=xlab, ylab=ylab, boxlabs = boxlabs,
                                      legendTitle = legendTitle, mainTitle=mainTitle);
  
  return(.set.mSet(mSetObj));
}

#'Produce a Box plot'

#'@description uses components provided by boxPlot_setup to produce a scatter plot
#'@usage plotBoxPlot(mSetObj, imgName, format="png", dpi=72, width=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.
#'The second default is width=0, where the width is 8. Otherwise users can input their own width.   
#'@author  Leif Wilm\email{lwilm@ualberta.ca} 
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export  
# PlotPieChart function calls upon the pieChart data set up by the pieChart_setUp function

plotBoxPlot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  mSetObj <- .get.mSet(mSetObj)
  
  # call upon graph parameters
  data <- mSetObj$analSet$boxPlot$data;
  facA <- mSetObj$analSet$boxPlot$facA;
  facB <- mSetObj$analSet$boxPlot$facB;
  facC <- mSetObj$analSet$boxPlot$facC;
  #type <- mSetObj$analSet$boxPlot$type;
  fillColor <- mSetObj$analSet$boxPlot$fillColor;
  boxlabs <- mSetObj$analSet$boxPlot$boxlabs;
  xlab <- mSetObj$analSet$boxPlot$xlab;
  ylab <- mSetObj$analSet$boxPlot$ylab;
  mainTitle <- mSetObj$analSet$boxPlot$mainTitle;
  
  
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
  
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
  mSetObj$imgSet$boxPlot <- imgName;
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white");
  
  if (is.null(facC)){ # No fill variable
    p0 <- ggplot(data, aes(x=facA, y=facB)) + 
      geom_boxplot( fill = fillColor) + labs(title = mainTitle, x = xlab, y = ylab) +
      theme(plot.title = element_text(hjust = 0.5))
    p0
  } else {
    p0 <- ggplot(data, aes(x=facA, y=facB, fill = facC)) + 
      geom_boxplot() + labs(title = mainTitle, x = xlab, y = ylab, fill = 'hello world') +
      theme(plot.title = element_text(hjust = 0.5))
    if(!is.null(fillColor)){
      p0 + scale_fill_manual(values = fillColor)
    } else {
      p0
    }
    
  }
  
  dev.off();
  return(.set.mSet(mSetObj));
}

