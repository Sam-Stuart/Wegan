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
#'@param type To Be Determined....
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
boxPlot_setup <- function(mSetObj = NA, facA = "NULL", facB = "NULL", facC = "NULL",
                          type = "NULL", fillColor = "NULL",
                          xlab = "NULL", ylab = "NULL", boxlabs = "NULL", legendTitle = "Legend Title", mainTitle = "Main Title"){
  mSetObj <- .get.mSet(mSetObj)
  print("Setting up the boxplot .......");
  #input <- mSetObj$dataSet$norm
  data <- mSetObj$dataSet$norm
  #data <- iris
print("iris pure: ")
  print(head(iris))
print("iris Normalized: ")
  print(head(data))
print("iris OG :" )
print(head(mSetObj$dataSet$orig))

  data <- mSetObj$dataSet$orig
 #  facA = 
# facB = "Sepal.Length"
  #facC = "NULL"
  #type = "NULL"
  #fillColor = "v"
  #xlab = "NULL"
  #ylab = "Sepal Length"
  #boxlabs = "NULL"
  #legendTitle = "NULL"
  #mainTitle = "Iris Boxplot!!"
  # Set x axis label if null
  if (xlab == "NULL"){
    if(facA == "NULL" ){
      xlab <- "Independant Variable"
    } else if (length(facA) == 1) {
      xlab <- facA
    } else {
      print(".....")
    }
  }
  # set y axis label if null
  if (ylab == "NULL"){
    if(facB == "NULL"){
      ylab <- "Dependant Variable"
    } else if (length(facB == 5)) {
      ylab <- facB
    } else{
      ylab <- "......"
    }
  }
    
  #Set x axis variable data
  if (is.character(facA)){
    if (facA == "NULL"){
      try(facA <- (data)[4]) ; #Default is fourth column.
    } else {
      facA <- (data[,facA])
    }
  }
  facA <- unlist(facA) # turn list into a vector
  facA <- unname(facA) # remove column names from vector
  #Set y axis data
  if (is.character(facB)){
    if (facB == "NULL"){
      facB <- (data)[2]; #Default is second column.
    } else {
      facB <- data[,facB]
    }
  }
  facB <- unlist(facB) # turn list into a vector
  facB <- unname(facB) # remove column names from vector
  # set fill data - if exists
  
  if (!facC == "NULL"){
    facC <- data[,facC]
    facC <- unlist(facC)
    facC <- unname(facC)
  }
  # box labels
  if (boxlabs == "NULL"){
  } else if ((length(boxlabs) == 1) & (str_length(boxlabs) > 1)){
    boxlabs <- str_replace_all(boxlabs, "\n", "") # remove and newlines 'enters'
    boxlabs <- str_replace_all(boxlabs, ";", ",") # replace all semi colons with comma's
    boxlabs <- str_split(boxlabs, ",")[[1]] # split string of colors separated by comma's into substrings
  } else {
     stop("Unable to format box labels correctly")
  }
  facA_dim <- length(unique(facA)) # number of groups in facA
  facC_dim <- length(unique(facC)) # number of groups in facC
   
  # select Color if no fill variable :
  if (facC == "NULL"){
    if (fillColor == "NULL"){
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
  } else if(!facC[1] == "NULL"){# option where fill variable is present (facC)
      if(!fillColor[1] == "NULL"){
        if ((length(fillColor) == 1) & (str_length(fillColor) == 1)){
          if (fillColor == 'r'){
            fillColor <- rainbow(facC_dim)
          } else if (fillColor == 'v'){
            fillColor <- viridis(facC_dim)
          } else if (fillColor == 'p'){
            fillColor <- plasma(facC_dim)
          } else if (fillColor == 'g'){
            fillColor <- grey.colors(facC_dim)
          } else {
            stop("Unknown color palette selected")
          }
      } else if ((length(fillColor) == 1) & (str_length(fillColor) > 1)){
        fillColor <- str_split(fillColor, ",")[[1]] # split string of colors into substrings
      }
      if (length(fillColor) > 1 & length(fillColor) != facC_dim){
        stop("dimensions of color must be eqaul to grouping dimensions (",facC_dim, ")" )
      }
    }
  }
  # save properties to object
print("saving everyting")


  mSetObj$analSet$boxPlot <- list(data = data, facA = facA, facB = facB, facC = facC,
                                      type=type, fillColor = fillColor,
                                      xlab=xlab, ylab=ylab, boxlabs = boxlabs,
                                      legendTitle = legendTitle, mainTitle=mainTitle);
  return(.set.mSet(mSetObj));
  

}
    


#boxPlot_setup <- function(mSetObj = NA){
#    print("boxplot testing");
#}
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
#'@author  Leif Wilm\email lwilm@ualberta.ca
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

plotBoxPlot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA){
  print("plot box plotting function") 
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
  legendTitle <- mSetObj$analSet$boxPlot$legendTitle;
  
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
  if (facC == "NULL"){ # No fill variable
    print("fac C be Null")
    p0 <- ggplot(data, aes(x=facA, y=facB)) +
      geom_boxplot( fill = fillColor) + labs(title = mainTitle, x = xlab, y = ylab) +
      theme(plot.title = element_text(hjust = 0.5))
    show(p0)
   } else {
    p0 <- ggplot(data, aes(x=facA, y=facB, fill = facC)) +
      geom_boxplot() + labs(title = mainTitle, x = xlab, y = ylab, fill = legendTitle) +
      theme(plot.title = element_text(hjust = 0.5))
    if(!is.null(fillColor)){
      p0 + scale_fill_manual(values = fillColor)
    } else {
      show(p0)
    }
  }
  dev.off();
  
return(.set.mSet(mSetObj));
}