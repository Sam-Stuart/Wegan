library(tidyr)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(vegan)

mSetObj = list()
data("dune")
input <- dune 
mSetObj$dataSet$orig = input
mSetObj$dataSet$norm = input
facA = "NULL"
colors = "NULL"
xlab = "NULL"
ylab = "NULL"
aggregate_function = "NULL"
barLabels = "NULL"
mainTitle = "NULL"
data = "false"
titleTextSize = 12
axisTextSize = 12

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
pieChart_setup <-
  function(mSetObj = NA,
           facA = "NULL",
           colors = "NULL",
           xlab = "NULL",
           ylab = "NULL",
           barLabels = "NULL",
           mainTitle = "NULL",
           aggregate_function = "NULL",
           titleTextSize = 12,
           axisTextSize = 12,
           data = "false") {
    mSetObj <- .get.mSet(mSetObj)
    input <- mSetObj$dataSet$norm
    if (data == "false") {
      input <- mSetObj$dataSet$norm
    } else {
      input <- mSetObj$dataSet$orig
    }
    
    categorical_data <- select_if(input, is.character)
    numerical_data <- select_if(input, is.numeric)
    
    # Create a hash table of aggregate functions
    if (aggregate_function == "NULL")
      aggregate_function = "mean"
    
    # Set selected variable
    if (facA == "NULL")
      #Set it to the first column name
      facA <- colnames(numerical_data)[1]

    if (length(as.matrix(categorical_data)) == 0) {
      sites_abund <- rowSums(input, na.rm = TRUE)
      # Divide each individual abundace by total site abundance
      input <- sweep(input, 1, sites_abund, "/")
      # Get sites variable from rownames
      input_sites <- input %>% mutate(Site=as.numeric(rownames(input)))
      # Get Species as variable and name the values as abundance
      df <- input_sites %>% pivot_longer(cols = -Site,
                                        names_to = "variable", 
                                        values_to = "value") 
      # Set up colorscheme for more than 12 colors
      colourCount = length(unique(df$variable))
      getPalette = colorRampPalette(brewer.pal(12, "Paired"))
    } else {
      # Create aggregate data
      df <-
        aggregate(input[, 1:length(input) - 1],
                  by = categorical_data,
                  FUN = get(aggregate_function))
      df <- melt(df, id = colnames(categorical_data)[1])
    }
  
    #Filter 0 values && only use 1 variable for pie chart
    md.df <- filter(df, value > 0)
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
    mSetObj$analSet$pieGraph <-
      list(
        md.df = md.df,
        dune_like = length(as.matrix(categorical_data)) == 0,
        aggregate_function = aggregate_function,
        facA = facA,
        barColor = colors,
        xlab = xlab,
        ylab = ylab,
        barLabels = barLabels,
        titleTextSize = titleTextSize,
        axisTextSize = axisTextSize,
        colourCount = colourCount,
        getPalette = getPalette,
        mainTitle = mainTitle
      )
    
    return(.set.mSet(mSetObj))
    
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
imgName = "test"
format = "png"
dpi = 72
width = NA
plotPieChart <-
  function(mSetObj = NA,
           imgName,
           format = "png",
           dpi = 72,
           width = NA) {
    mSetObj <- .get.mSet(mSetObj)
    data <- mSetObj$analSet$pieGraph$md.df
    dune_like <- mSetObj$analSet$pieGraph$dune_like
    aggregate_function <-
      mSetObj$analSet$pieGraph$aggregate_function
    facA <- mSetObj$analSet$pieGraph$facA
    colourCount <- mSetObj$analSet$pieGraph$colourCount
    getPalette <- mSetObj$analSet$pieGraph$getPalette
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
    mSetObj$imgSet$pieChart <- imgName
    
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
    if (dune_like) {
      # Create bar plot
      bar <- ggplot(data, aes(x = "", y = value, fill = variable)) +
        geom_bar(stat = "identity", width=2) +
        scale_fill_manual(values = getPalette(colourCount)) +
        theme_classic()
      # convert bar chart to individual pie charts
      plot <- bar +
        coord_polar("y") +
        facet_wrap(~Site, ncol=4) +
        theme_minimal()+
        theme(axis.text.x = element_blank(), legend.position = "bottom")
    } else {
      # iris-like
      data <- data %>%
        arrange(desc(Species)) %>%
        mutate(prop = value / sum(data$value) * 100) %>%
        mutate(ypos = cumsum(prop) - 0.5 * prop)
      print(data)
      
      plot <- ggplot(data, aes(x = "", y = prop, fill = Species)) +
        geom_bar(stat = "identity",
                 width = 1,
                 color = "white") +
        coord_polar("y", start = 0) +
        labs(title = paste(aggregate_function, " ", facA, " of species")) +
        theme_void(
          base_size = 13,
        ) +
        geom_text(aes(y = ypos, label = value), color = "white", size = 6)
    }
   
    show(plot)
    
    dev.off()
    return(.set.mSet(mSetObj))
  }