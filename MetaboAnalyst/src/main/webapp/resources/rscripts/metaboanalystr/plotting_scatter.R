#scatterPlot_plotting
library(vegan)
library(dplyr)
library(ggplot2)
# mSetObj = list()
# input <- iris
# mSetObj$dataSet$orig = input
# mSetObj$dataSet$norm = input
# facA = "NULL"
# facB = "NULL"
# type = "NULL"
# line_color = "NULL"
# point_color = "NULL"
# xlab = "NULL"
# ylab = "NULL"
# maintitle = 'Title'
# data = "false"

##### SCATTER PLOT ####
#'Scatter Plot'
#'@description Produce scatter plot components based on user data and preferences
#'@usage scatterPlot_setup(mSetObj = NA, facA=NULL, facB=NULL, type = NULL,
#'# line_color = "red", xlab = 'x axis',ylab = 'y axis', maintitle = 'Title')
#'@param mSetObj Input the name of the created mSetObj
#'@param facA list of independent variables
#'@param facB list of dependent variables
#'@param type type of (if any) trend line to be used. Options are "lm", "glm", "gam", "loess", "rlm"!!!!!!!!!!!!!!
#'@param line_color color of line of best fit.
#'@param xlab x axis title. NULL will choose column name
#'@param ylab y axis title, NULL will choose column name
#'@param maintitle graph title
#'@author Leif Wilm\email{lwilm@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
#'

scatterPlot_setup <-
  function(mSetObj = NA,
           facA = "NULL",
           facB = "NULL",
           type = "NULL",
           line_color = "NULL",
           point_color = "NULL",
           xlab = "NULL",
           ylab = "NULL",
           maintitle = 'Title',
           titleTextSize = 12,
           axisTextSize = 12,
           data = "false") {
    #Obtain mSet dataset
    mSetObj <- .get.mSet(mSetObj)
    if (data == "false") {
      input <- mSetObj$dataSet$norm
    } else {
      input <- mSetObj$dataSet$orig
    }
    
    
    #ADD FILTER FOR NUMERIC DATA USING select_if function from dplyr package!
    input <- select_if(input, is.numeric)
    
    #Set independent variable name
    if (facA == "NULL") {
      #Default is first column.
      facA <- colnames(mSetObj$dataSet$norm)[1]
    }
    #Set dependent  variable name
    if (facB == "NULL") {
      #Default is second column.
      facB <- colnames(mSetObj$dataSet$norm)[2]
    }
    
    ##Variable type check
    #if (is.factor(mSetObj$dataSet$norm[,facA] || mSetObj$dataSet$norm[,facB])==TRUE){
    #  #AddErrMsg("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.")
    #  stop("You have chosen 1 or more categorical columns! Try selecting another independent and/or dependent variable. You can also try other regression models such as penalized, logistic, SVM or random forest.") #Error msg
    #}
    
    #Define formula
    formula <- as.formula(paste0(facB, "~", facA))
    
    #Default will pass null to almost every parameter
    if (xlab == "NULL") {
      xlab <- facA
    }
    
    if (ylab == "NULL") {
      ylab <- facB
    }
    
    if (type == "NULL") {
      type <- "lm"
    }
    
    if (line_color == "NULL") {
      line_color <- "black"
    }
    
    if (point_color == "NULL") {
      point_color <- "black"
    }
    
    # save properties to object
    mSetObj$analSet$scatterPlot <-
      list(
        data = input,
        facA = facA,
        facB = facB,
        formula = formula,
        type = type,
        line_color = line_color,
        point_color = point_color,
        xlab = xlab,
        ylab = ylab,
        maintitle = maintitle,
        titleTextSize = titleTextSize,
        axisTextSize = axisTextSize
      )
    return(.set.mSet(mSetObj))
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
# imgName = "test"
# format = "png"
# dpi = 72
# width = NA
plotScatterPlot <-
  function(mSetObj = NA,
           imgName,
           format = "png",
           dpi = 72,
           width = NA) {
    mSetObj <- .get.mSet(mSetObj)
    
    # call upon graph parameters
    input <- mSetObj$analSet$scatterPlot$data
    facA <- mSetObj$analSet$scatterPlot$facA
    facB <- mSetObj$analSet$scatterPlot$facB
    formula <- mSetObj$analSet$scatterPlot$formula
    type <- mSetObj$analSet$scatterPlot$type
    line_color <- mSetObj$analSet$scatterPlot$line_color
    point_color <- mSetObj$analSet$scatterPlot$point_color
    xlab <- mSetObj$analSet$scatterPlot$xlab
    ylab <- mSetObj$analSet$scatterPlot$ylab
    maintitle <- mSetObj$analSet$scatterPlot$maintitle
    axisTextSize <- mSetObj$analSet$scatterPlot$axisTextSize
    titleTextSize <- mSetObj$analSet$scatterPlot$titleTextSize
    
    # Convert to sym to use with aes
    facA <- sym(facA)
    facB <- sym(facB)

    #Set plot dimensions
    if (is.na(width)) {
      w <- 10
    } else if (width == 0) {
      w <- 8
    } else{
      w <- width
    }
    h <- w
    
    
    #Name plot for download
    imgName <- paste(imgName, "dpi", dpi, ".", format, sep="");
    
    #Set that to mSetObj
    mSetObj$imgSet$scatterPlot <- imgName
    
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
    

    #Start plotting with ggplot2
    plot <- ggplot(input, aes(x = !!facA, y = !!facB)) +
      geom_point(color = point_color) +
      labs(title = maintitle, x = xlab, y = ylab) +
      geom_smooth(
        method = type,
        color = line_color,
        se = FALSE,
        fullrange = TRUE
      ) +
      theme_bw() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = axisTextSize, colour = "black"),
        axis.title = element_text(size = axisTextSize),
        plot.title = element_text(face = 'bold', hjust = 0.5, size = titleTextSize)
      )
    show(plot)
    dev.off()
    if(!.on.public.web){
      return(.set.mSet(mSetObj))
    }
  }
