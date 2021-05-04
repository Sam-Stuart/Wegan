library(viridis)

#'Pie Chart'
#'@description Construct pie chart components based on user data and preferences
#'@usage pieChart_setup(mSetObj = NA, byrow = TRUE, columns = NULL,rows = NULL,labels = NULL, colors = NULL, main = NULL, lgnd = FALSE)
#'@param mSetObj Input the name of the created mSetObj
#'@param byrow Boolean deciding whether data shall be read by row or column of data file
#'@param columns columns to be used
#'@param rows rows to be used 
#'@param labels label names for data sections
#'@param colors list of colors to be used for pie graph, NULL or 'r' = rainbow(); 'v' : viridis ; 'p' : plasma , and 'g' : greyscale .
#'@param mainTitle main title for the pie chart
#'@param lgnd boolean value; TRUE = there will be a legend, FALSE = no legend. 
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export


pieChart_setup <- function(mSetObj = NA, byrow = TRUE, columns = NULL,rows = NULL, 
                           labels = NULL, colors = NULL, mainTitle = NULL, lgnd = FALSE){
  
  mSetObj <- .get.mSet(mSetObj)
  input <- mSetObj$dataSet$norm
  
  # Read in the data : 
  if(!is.vector(input)){
    if (byrow){ # read data by rows boolean. 
      if (is.null(rows)){
        input <- input[1,]# Default is to use the first row of the data set
      } else {
        input <- input[rows,]
      }
      if (!is.null(columns)){ 
        input <- input[,columns] # input selected columns,  default is to use all the columns
      } 
      
      if (is.numeric(input)){
        input <- input[which(input > 0)] # remove any zeros from the data.
      }
      if (is.null(labels)){ 
        labels = colnames(input) # grab column names from data
      }
      
    } else { # read data by column 
      if(is.null(columns)){
        input <- input[,1] # default is first column.
      } else {
        input <- input[,columns]
      } 
      if (!is.null(rows)){
        input <- input[rows,] # input selected rows, default is to use all the rows.
      }
      if (is.numeric(input)){# if data is numerical 
        input <- input[which(input > 0)] # remove any zeros from the data.
      }
      
      
      if (is.null(labels) & !is.null(rownames(input))){ #if no user-inputted labels, and the data has row names, use the data row names.
        labels <- rownames(input)
      }
    }
  }
  cat("RIGHT BEFORE IS FACTOR")
  print(is.factor(input))
  data <- table(input)
  
  
    
  if (length(labels) != length(data) ){
    if (!is.null(names(data))){
      labels <- names(data)
    } else {
      stop("Number of labels given does not match number of data points")
    }
  }
  
  # select Color 
  if (is.null(colors)){
    colors <- rainbow(length(data))
  }
  if ((length(colors) == 1) & (str_length(colors) == 1)){ # preset palettes 
    if ( colors == 'r'){
      colors <- rainbow(length(data))
    } else if (colors == 'v'){
      colors <- viridis(length(data))
    } else if (colors == 'p'){
      colors <- plasma(length(data))
    } else if (colors == 'g'){
      colors <- grey.colors(length(data))
    } else {
      stop("Unknown color palette selected")
    }
  } else if ((length(colors) == 1) & (str_length(colors) > 1)){
    colors <- str_split(colors, ",")[[1]] # split string of colors into substrings
  } else {
    stop("Unable to format colors correctly")
  }
  
  
  
  # save variables to the object
  mSetObj$analSet$pieChart <- list(data = data, colors = colors, mainTitle = mainTitle)
  
  
  
  
  if (!lgnd){ # save labels based on existence of legend or not
    mSetObj$analSet$pieChart$labels <- labels
    mSetObj$analSet$pieChart$legend <- NULL
  } else {
    pct = paste(round(100*data/sum(data),1),"%", sep = " ");
    mSetObj$analSet$pieChart$labels <- pct;
    mSetObj$analSet$pieChart$legend <- labels; 
  }
  return(.set.mSet(mSetObj));
}

#'Produce a Pie Chart'
#'@description uses data provided by pieChart_setup to produce a pie chart
#'@usage plotPieChart(mSetObj, imgName, format="png", dpi=72, width=NA)
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
plotPieChart <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  
  mSetObj <- .get.mSet(mSetObj) 
  data <- mSetObj$analSet$pieChart$data
  colors <- mSetObj$analSet$pieChart$colors
  main <- mSetObj$analSet$pieChart$mainTitle
  labels <- mSetObj$analSet$pieChart$labels
  legend_labels = mSetObj$analSet$pieChart$legend
  
  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width == 0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$pieChart <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  pie(data, col = colors, main = main, labels = labels)
  if (!is.null(legend_labels)){
    legend("topright", legend = legend_labels, fill = colors)
  }
  dev.off()
  
  return(.set.mSet(mSetObj))
}



