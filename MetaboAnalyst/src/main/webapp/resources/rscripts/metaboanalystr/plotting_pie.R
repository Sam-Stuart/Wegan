
#'Pie Chart'
#'@description Construct pie chart components based on default variables and user input
#'@usage pieChart_setup(mSetObj = NA, byrow = TRUE, columns = NULL,rows = NULL,labels = NULL, colors = NULL, main = NULL, lgnd = FALSE)
#'@param mSetObj Input the name of the created mSetObj
#'@param byrow Boolean deciding whether data shall be read by row or column of data array, default is FALSE
#'@param bysum Boolean for whether data is represented by sum or by count. default is FALSE; by count
#'@param columns columns to be represented in pie chart; vector of strings or integers; default is first column. 
#'@param rows rows to be represented in the pie chart; vector of strings or integers : default is NULL
#'@param labels label names for data sections : Should be a vector of strings equal to number of pie partitions, if NULL, will give default labels according to the data
#'@param colors list of colors to be used for pie graph, NULL or 'v' : viridis; 'r' = rainbow(); 'p' : plasma , and 'g' : greyscale .
#'@param mainTitle main title for the pie chart
#'@param lgnd boolean value; TRUE = there will be a legend, FALSE = no legend. default is FALSE
#'@author Leif Wilm\email lwilm@ualberta.ca 
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
pieChart_setup <- function(mSetObj = NA, byrow = "false", bysum = "false", columns = 1,rows = "NULL", 
                           labels = "NULL", colors = "NULL", mainTitle = "Main Title", lgnd = "false"){
  
  #print("BANANA PIE : This comment is in plotting_pieChart.R");
  mSetObj <- .get.mSet(mSetObj)
  input <- mSetObj$dataSet$norm
  
  if(!is.numeric(rows)){
      if (rows == "NULL"){ # default is all rows 
      rows <- c(1:nrow(input))
    }
  }
  if(!is.numeric(columns)){
      if (columns == "NULL"){ # default is all columns
      columns <- c(1:ncol(input))
    }
  }
  
  
  if(is.vector(input)){ # check that data input is not a vector
    data <- input
  }else{
    if (is.character(columns)){# check if columns are strings
      for (column_name in columns){ # check for incorrect column names
        if (!column_name %in% names(input)){
          stop("'",column_name,"' is not a valid column name")
        }
      }
    } 
    input <- input[rows,columns]
  }
  # numeric data check 
  numeric_data = FALSE
  if (is.data.frame(input)||is.array(input)){
    if(is.numeric(input[1,1])){
      numeric_data = TRUE
    }
  }else{
    if(is.numeric(input[1])){
      numeric_data = TRUE
    }
  }
  if(numeric_data){ # if data is numerical
    if(bysum=="true"){ # sums up the data
      if(byrow=="true"){ # sums up the data by rows
        if(length(columns)>1){ # more than one column
          data <- rowSums(input)
        } else{ # one column of data 
          data <- input
        }
      } else { # by row is false, sum data by columns
        if(length(rows)>1){ # more than one row
          data <- colSums(input)
        } else { # one row of data
          data <- input
        }
      }
    } else{ # by sum is false; data is a count of the number of occurrences of each numerical factor
      data <- table(input)
      
    }
  } else{# data is categorical, count number of occurrences of each categorical factor
    data <- table(input)
    
  }
  
  if (length(labels)==1){
      if (labels == "NULL"){ 
        labels = names(data) # grab column names from data
    } else{
        labels <- gsub(";", ",", labels, fixed=TRUE)
        labels <- unlist(strsplit(labels, split=","))
    }
  }
  
  
  # select Color 
  if(length(colors)==1){
      if (colors == "NULL"){ # default color is rainbow
      colors <- viridis(length(data))
    } else if ((length(colors) == 1) & (str_length(colors) == 1)){ # preset palettes 
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
  }
  
  # save variables to the object
  mSetObj$analSet$pieChart <- list(data = data, colors = colors, mainTitle = mainTitle )
  if (lgnd != "true"){ # save labels based on existence of legend or not
    mSetObj$analSet$pieChart$labels <- labels
    mSetObj$analSet$pieChart$legend <- NULL
  } else { # set up legend :
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
#'@author  Leif Wilm\email  lwilm@ualberta.ca
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
  if (length(legend_labels)>1){
    legend("topright", legend = legend_labels, fill = colors)
  }
  
  dev.off()
  return(.set.mSet(mSetObj))
}