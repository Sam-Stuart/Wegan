

#### BAR GRAPH ##### 
#'Bar Graph'
#'@description Produce bar graph components based on user data and preferences
#'@usage barGraph_setup(mSetObj = NA, byrow = FALSE, colNum = NULL, rowNum = NULL,
#' colors = NULL, xlab = NULL, ylab = NULL, barLabels = NULL, mainTitle = NULL)
#'@param mSetObj Input the name of the created mSetObj
#'@param byrow boolean value, TRUE = read data by row(s), FALSE = read data by column(s)
#'@param colNum column number(s) to use for data
#'@param rowNum type of (if any) trend line to be used. NULL = no trendline; 'lbf' = line of best fit ; 'lowess' = locally weighted trend line
#'@param colors color of bars
#'@param xlab x axis title. NULL will choose column name
#'@param ylab y axis title, NULL will choose column name
#'@param barLabels y axis title, NULL will choose column name
#'@param maintitle graph title 
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export


barGraph_setup <- function(mSetObj = NA, byrow = "FALSE", colNum = "NULL", rowNum = "NULL",
                           colors = "NULL", xlab = "NULL", ylab = "NULL", barLabels = "NULL", mainTitle = "NULL"){
  
  print("This comment is in plotting_bargraph.R");
  mSetObj <- .get.mSet(mSetObj)
  input <- mSetObj$dataSet$norm
  if (!is.vector(input)){
    if (byrow != "FALSE"){ # read data by rows boolean. 
        
      if (rowNum == "NULL"){
        input <- input[1,]# Default is to use the first row of the data set
      } else {
        input <- input[rowNum,]
      }
      if (colNum != "NULL"){ 
        input <- input[,colNum] # input selected columns,  default is to use all the columns
      }  
      
      if (is.numeric(input)){
        input <- input[which(input > 0)] # remove any zeros from the data.
      }
      if (barLabels == "NULL"){  # if barLabels is NULL 
        barLabels = colnames(input) # grab column names from data
      }
      
    } else { # read data by column 
    
      if(colNum == "NULL"){
        input <- input[,1] # default is first column.
        
      } else {
        input <- input[,colNum]
      } 
      if (rowNum != "NULL"){
        input <- input[rowNum,] # input selected rows, default is to use all the rows.
      }
      if (is.numeric(input)){
        input <- input[which(input > 0)] # remove any zeros from the data.
      }
      
      
      if ((barLabels == "NULL") & !is.null(rownames(input))){ #if no user-inputted labels, and the data has row names, use the data row names.
        barLabels <- rownames(input)
      } 
    }
  }
  
  # select Color
  if (colors == "NULL"){
    colors <- 'lightblue'
  } else if (colors == 'r'){
    colors <- rainbow(length(input))
  } else if (colors == "v"){
    colors <- viridis(length(input))
  } else if (colors == "p"){
    colors <- plasma(length(input))
  } else if (colors == 'g'){
    colors <- grey.colors(length(input))
  }
    
  count <- table(input) # group data 
  # Save bar graph parameters in mSetObj$analSet 
  mSetObj$analSet$barGraph <- list(count = count, barColor = colors, xlab = xlab, ylab = ylab,
                                   barLabels = barLabels, mainTitle = mainTitle)
  
  return(.set.mSet(mSetObj));
}

#'Produce a Bar Graph'
#'@description uses data provided by barGraph_setup to produce a bar graph
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


plotBarGraph <- function(mSetObj=NA, imgName = NA, format="png", dpi=72, width=NA){
  # call upon variables
  mSetObj <- .get.mSet(mSetObj)
  count = mSetObj$analSet$barGraph$count
  barColor <- mSetObj$analSet$barGraph$barColor
  xlab <- mSetObj$analSet$barGraph$xlab
  ylab <- mSetObj$analSet$barGraph$ylab
  barLabels <- mSetObj$analSet$barGraph$barLabels
  mainTitle <- mSetObj$analSet$barGraph$mainTitle
  
  
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
  mSetObj$imgSet$barGraph <- imgName
  
  #Generate plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  
  barplot(height = count, names.arg = barLabels, main = mainTitle, xlab = xlab, ylab = ylab, col = barColor)
  
  dev.off()
  
  return(.set.mSet(mSetObj))
}



