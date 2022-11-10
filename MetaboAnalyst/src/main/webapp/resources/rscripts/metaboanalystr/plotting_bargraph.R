



#### BAR GRAPH #####
#'Bar Graph'
#'@description Produce bar graph components based on user data and preferences
#'@usage barGraph_setup(mSetObj = NA, byrow = FALSE, colNum = NULL, rowNum = NULL,
#' colors = NULL, xlab = NULL, ylab = NULL, barLabels = NULL, mainTitle = NULL)
#'@param mSetObj Input the name of the created mSetObj
#'@param facA The categorical data name
#'@param colors color of bars
#'@param xlab x axis title. NULL will choose column name
#'@param ylab y axis title, NULL will choose column name
#'@param barLabels y axis title, NULL will choose column name
#'@param maintitle graph title
#'@author Hieu Nguyen\email{nguyet3@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
library(dplyr)
library(reshape2)
library(ggplot2)
library(vegan)

mSetObj = list()
data()
input <- read.csv(file = "/Users/hieunguyen/Documents/Wegan/pitlatrine.csv")
mSetObj$dataSet$orig = input
mSetObj$dataSet$norm = input
facA = "Country"
colors = "NULL"
xlab = "NULL"
ylab = "NULL"
aggregate_function = "NULL"
barLabels = "NULL"
mainTitle = "NULL"
data = "false"


barGraph_setup <-
  function(mSetObj = NA,
           facA = "NULL",
           colors = "NULL",
           xlab = "NULL",
           ylab = "NULL",
           barLabels = "NULL",
           mainTitle = "NULL",
           aggregate_function = "NULL",
           data = "false") {
    mSetObj <- .get.mSet(mSetObj)
    
    if (data == "false") {
      input <- mSetObj$dataSet$norm
    } else {
      input <- mSetObj$dataSet$orig
    }
    
    categorical_data <- select_if(input, is.character)
    numerical_data <- select_if(input, is.numeric)
    
    if (facA == "NULL" & length(as.matrix(categorical_data)) > 0)
      #Set it to the first column name
      facA <- colnames(categorical_data)[1]
    
    
    # Create a hash table of aggregate functions
    if (aggregate_function == "NULL")
      aggregate_function = "mean"
    
    if (length(as.matrix(categorical_data)) == 0) {
      input$Sites <- row.names(input)
      categorical_data <- select_if(input, is.character)
      df <- input
      md.df <- melt(df, id = c('Sites'))
    } else {
      
      # Create aggregate data
      df <-
        aggregate(numerical_data,
                  by = select(input, c(facA)),
                  FUN = get(aggregate_function))
      md.df <- melt(df, id = c(facA))
    }
    
    #Filter 0 values
    md.df <- filter(md.df, value > 0)
    # Label for each bar
    if (barLabels == "NULL")
      barLabels <- colnames(numerical_data)
    

    
    if (xlab == "NULL")
      xlab <- facA
    
    if (ylab == "NULL")
      ylab <- "Values"
    
    
    # select Color
    if (colors == "NULL") {
      colors <- 'lightblue'
    } else if (colors == 'r') {
      colors <- rainbow(length(input))
    } else if (colors == "v") {
      colors <- viridis(length(input))
    } else if (colors == "p") {
      colors <- plasma(length(input))
    } else if (colors == 'g') {
      colors <- grey.colors(length(input))
    }
    
    # Save bar graph parameters in mSetObj$analSet
    mSetObj$analSet$barGraph <-
      list(
        md.df = md.df,
        facA = facA,
        barColor = colors,
        xlab = xlab,
        ylab = ylab,
        barLabels = barLabels,
        mainTitle = mainTitle
      )
    
    return(.set.mSet(mSetObj))
    
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
#'@author  Hieu Nguyen\email{htnguye3@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
imgName = "test"
format = "png"
dpi = 72
width = NA

plotBarGraph <-
  function(mSetObj = NA,
           imgName = NA,
           format = "png",
           dpi = 72,
           width = NA) {
    # call upon variables
    mSetObj <- .get.mSet(mSetObj)
    md.df <- mSetObj$analSet$barGraph$md.df
    barColor <- mSetObj$analSet$barGraph$barColor
    facA <- mSetObj$analSet$barGraph$facA
    xlab <- mSetObj$analSet$barGraph$xlab
    ylab <- mSetObj$analSet$barGraph$ylab
    barLabels <- mSetObj$analSet$barGraph$barLabels
    mainTitle <- mSetObj$analSet$barGraph$mainTitle
    
    # Convert to sym to use with aes
    facA <- sym(facA)
    
    #Set plot dimensions
    if (is.na(width)) {
      w <- 10.5
    } else if (width == 0) {
      w <- 7.2
    } else{
      w <- width
    }
    h <- w
    
    #Name plot for download
    imgName <- paste(imgName, "dpi", dpi, ".", format, sep = "")
    mSetObj$imgSet$barGraph <- imgName
    
    
    
    #Generate plot
    Cairo::Cairo(
      file = imgName,
      unit = "in",
      dpi = dpi,
      width = w,
      height = h,
      type = format,
      bg = "white"
    )
    
    plot <-
      ggplot(md.df, aes(
        x = !!facA,
        y = value,
        group = variable,
        fill = variable
      )) +
      geom_bar(stat = "identity",
               color = "black",
               position = "dodge") +
      labs(title = mainTitle, x = xlab, y = ylab) +
      theme_bw() +
      geom_text(
        aes(label = sprintf("%0.2f", value)),
        vjust = -0.3,
        size = 3,
        # adding values
        position = position_dodge(0.9)
      )
    show(plot)
    dev.off()
    
    if (!.on.public.web) {
      return(.set.mSet(mSetObj))
    }
  }
