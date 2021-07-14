#'Perform CIA
#'@description Perform co-inertia analysis between 2 data sets with the same rows
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param type Set type for data in both main data set and constraining data set, drop down options are "Categorical" or "Numeric" (default)
#'@param env_text Input constraining data column names (java uses text box to obtain string), default is use all columns
#'@param envData Environmental/Constraining data set in working directory if uploaded. This constraining data set is compulsory
#'@param metaData  Grouping data in working directory if uploaded, default is nothing uploaded and categorical columns in data set selected if available, otherwise no meta data and only numeric data type available for comparison
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.cia <- function(mSetObj=NA, data=NULL, type="NULL", env_text="NULL") {
 
  library("ade4")
  library("adegraphics")
  library("dplyr")
  library("vegan")
 
  #Extract input from mSetObj
  mSetObj <- .get.mSet(mSetObj)

  ####FOR TESTING####
  #metaData <- "NULL"
  metaData <- .readDataTable("/home/louisa/Wegan/Wegan/MetaboAnalyst/src/main/webapp/resources/rscripts/metaboanalystr/test_data/dune_meta.csv")
  envData <- .readDataTable("/home/louisa/Wegan/Wegan/MetaboAnalyst/src/main/webapp/resources/rscripts/metaboanalystr/test_data/dune_env.csv")
  envData$Moisture <- as.numeric(as.character(envData$Moisture)) #Require more than one numeric envData column for numeric type option

  if (is.null(data)) { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
 
  #constraining data, used to correlate with rows in main data set
  if (is.data.frame(envData)==FALSE) { #No user uplaoded constraining data
    #AddErrMsg("No constraining dataset uploaded! Coinertia analysis requires two datasets for comparison. Please update dataset(s) or try a non-constraining method such as PCA, PCoA, NMDS, CA, Bray-Curtis or DCA.")
    stop("No constraining dataset uploaded! Coinertia analysis requires two datasets for comparison. Please update dataset(s) or try a non-constraining method such as PCA, PCoA, NMDS, CA, Bray-Curtis or DCA.")
  } else {  #User uplaoded constraining data
    envData1 <- envData #User uploaded (like weights in correlation module)
    colnames(envData1) <- gsub(" ", "", colnames(envData1), fixed=FALSE)
    if (nrow(envData1)!=nrow(input)) {
      #AddErrMsg("Your constraining data does not have the same number of rows as your data set! Please check that you constraining data is correct.")
      stop("Your constraining data does not have the same number of rows as your data set! Please check that you constraining data is correct.")
    }
  }
 
  #Text box instructions for selecting predictor variables. Text box should be interactive, meaning any change in text alters the result in real time. Default env_text is second column.
  cat("Identify constraining variables for co-inertia analysis using the column names with commas in between.")
 
  #Set up constraining data using user selected columns
  if (env_text=="NULL") {
    env_text1 <- colnames(envData1) #Default is the all env columns
  } else {
    env_text1 <- env_text #taken from text box by java, fed as string into R code
    env_text1 <- gsub("\n", "", env_text1, fixed=TRUE) #fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
    env_text1 <- gsub(" ", "", env_text1, fixed=TRUE)
    env_text1 <- gsub(",", "+", env_text1, fixed=TRUE)
    env_text1 <- gsub(";", "+", env_text1, fixed=TRUE)
    env_text1 <- gsub(":", "+", env_text1, fixed=TRUE)
    env_text1 <- gsub("*", "+", env_text1, fixed=TRUE)
  }
 
  env_cols <- unlist(strsplit(env_text1, "+", fixed=TRUE))
  env_data <- as.data.frame(envData1[,which(colnames(envData1) %in% env_cols)])
  colnames(env_data) <- env_cols
  
  if (type=="NULL") { #data for comparison is numeric
    type1 <- "Numeric"
    method <- "PCA"
    input.1 <- select_if(input, is.numeric) #Select numeric data from both data sets for comparison
    envData.1 <- select_if(env_data, is.numeric)

    if (ncol(input.1)<2) {
      #AddErrMsg("Main data set has less than 2 numeric columns! Please adjust your data set and rerun analysis.")
      stop("Main data set has less than 2 numeric columns! Please adjust your data set and rerun analysis.")
    }
   
    if (ncol(envData.1)<2) {
      #AddErrMsg("Environmental data set has less than 2 numeric columns. Please adjust your environmental data set and rerun analysis.")
      stop("Environmental data set has less than 2 numeric columns! Please adjust your environmental data set and rerun analysis.")
    }
   
    cat(paste0("You have selected these numeric constraining variables: ", paste(colnames(envData.1), collapse=", "), "."))
    cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
  
    dudi_data <- dudi.pca(input.1, scannf=FALSE) #Create Duality Diagram for both data sets
    dudi_env <- dudi.pca(envData.1, scannf=FALSE)
   
  } else { #data is categorical
    type1 <- "Categorical"
    method <- "CA"
    input.2 <- select_if(input, is.factor)
    envData.2 <- select_if(env_data, is.factor)
   
    if (ncol(input.2)<2) {
      #AddErrMsg("Main data set has less than 2 categorical columns! Please adjust your data set and rerun analysis.")
      stop("Main data set has less than 2 categorical columns! Please adjust your data set and rerun analysis.")
    }
   
    if (ncol(envData.2)<2) {
      #AddErrMsg("Environmental data set has less than 2 categorical columns. Please adjust your environmental data set and rerun analysis.")
      stop("Environmental data set has less than 2 categorical columns! Please adjust your environmental data set and rerun analysis.")
    }
   
    cat(paste0("You have selected these categorical constraining variables: ", paste(colnames(envData.1), collapse=", "), "."))
    cat("If the selection is not what you intended, reenter environmental variable(s) in the text box, using the column names with commas in between.")
  
    dudi_data <- dudi.coa(input.2, scannf=FALSE)
    dudi_env <- dudi.coa(envData.2, scannf=FALSE)
   
  }
 
  if (!identical(dudi_data$lw, dudi_env$lw)) { #If weights calculated for both data sets are not equal, an error is thrown.
    #AddErrMsg("The two data tables that you choose have non-equal row weights. Try changing the data type for data set comparison in the drop down menu above. Try altering the environmental dataset by removing irrelevant columns. Otherwise, try other analyses, such as correlation heatmap or regression modeling, found in the Correlation Module")
    stop("The two data tables that you choose have non-equal row weights. Try changing the data type for data set comparison in the drop down menu above. Try altering the environmental dataset by removing irrelevant columns. Otherwise, try other analyses, such as correlation heatmap or regression modeling, found in the Correlation Module")
  }
 
  cia <- coinertia(dudi_data, dudi_env, scannf=FALSE) #Perform co-inertia analysis
 
  #meta data for input, used to group samples for plotting
  if (is.data.frame(metaData)==FALSE) { #No user uplaoded grouping data
    fac_data <- select_if(input, is.factor)
    count.fac.cols <- ncol(fac_data)
    if (count.fac.cols >= 1) { #If main data set had at least one categorical column, call it grouping data
      metaData1 <- as.data.frame(fac_data)
    } else { #No grouping data detected in main data set
      metaData1 <- "NA" #If main data set had no categorical columns, grouping data is NULL and error is produced
      #AddErrMsg("No groupings columns were detected. If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead ofgrouping data using 1, 2, and 3, use I, II and III.")
      print("No groupings columns were detected! If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead of grouping data using 1, 2, and 3, use I, II and III.")
    }
  } else {  #User uploaded grouping data
    metaData1 <- metaData #User uploaded like weights in correlation module
    if (nrow(metaData1)!=nrow(input)) {
      #AddErrMsg("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
      stop("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
    }
    for(i in 1:ncol(metaData1)) {
      metaData1[,i] <- as.factor(metaData1[,i]) #Make sure all columns are read as factors
    }
  }

  env_row_scores <- cia$mY
  main_row_scores <- cia$mX
  eigenValues_data <- cbind(cia$eig, cia$eig/sum(cia$eig))
  n <- nrow(eigenValues_data)
  eigenValues_data <- as.data.frame(cbind(paste0("CIA Axis ", 1:n), eigenValues_data))
  colnames(eigenValues_data) <- c("Axis", "Eigen_Value", "Variance_Explained")

  #Extract and store results
  mSetObj$analSet$cia$name <- "CIA"
  mSetObj$analSet$cia$cia <- cia
  mSetObj$analSet$cia$type <- type1
  mSetObj$analSet$cia$method <- method
  mSetObj$analSet$cia$summary <- summary(cia)
  mSetObj$analSet$cia$input <- input
  mSetObj$analSet$cia$envData <- env_data
  mSetObj$analSet$cia$metaData <- metaData1
  mSetObj$analSet$cia$eigenValues <- cia$eig
  mSetObj$analSet$cia$screeDataTable <- eigenValues_data
  mSetObj$analSet$cia$scatterEnvData <- env_row_scores
  mSetObj$analSet$cia$scatterMainData <- main_row_scores

  #Download text document containing the summary, called the fileName. Document goes into the working directory and should be accessible to the user as part of the report.
  sink("cia_summary.txt")
  summary(cia)
  cat("\nLegend:\n")
  cat("dudi_data = Duality Diagram Main Data Set\ndudi_env = Duality Diagram Environmental Data Set\n")
  cat("RV = Multivariate R-squared")
  sink()
 
  #Download relevent data
  norm.row.1 <- cia$mX
  colnames(norm.row.1) <- c("Axis 1", "Axis 2")
  norm.row.2 <- cia$mY
  colnames(norm.row.2) <- c("Axis 1", "Axis 2")
  write.csv(norm.row.1, file="coinertia_analysis_row_scores_main_data_set.csv", row.names=row.names(input))
  write.csv(norm.row.2, file="coinertia_analysis_row_scores_env_data_set.csv", row.names=row.names(envData1))
  write.csv(eigenValues_data, file="cia_scree_data.csv", row.names=FALSE)
  
return(.set.mSet(mSetObj))
 
}



#'Produce CIA scatter plot with and without grouping options
#'@description Produce co-inertia analysis scatter plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color pallete selection, options include "viridis" (default), "plasma" and "grey"
#'@param meta_group Boolean, TRUE to group data, FALSE (default) to not group data
#'@param meta_colname Meta data column to use for grouping, Can be user inputted where options are given to java using function meta.columns()
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png"
#'@param dpi Input the dpi as int. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width as int, there are 2 default widths. The first, width=NA, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
Plot.cia.scatter <- function(mSetObj=NA, meta_group=NULL, meta_colname="NULL", color="NULL", imgName, format="png", dpi=72, width=NA) {
 
  library("ade4")
  library("adegraphics")
  library("viridis")
 
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  cia <- mSetObj$analSet$cia$cia
  input <- mSetObj$analSet$cia$input
  metaData <- mSetObj$analSet$cia$metaData

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
 
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$Plot.cia.scatter <- imgName
 
  #Produce
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
 
  if (is.data.frame(metaData)==FALSE) { #No meta (grouping) data
    scatter.var <- s.label(cia$mX, pgrid.draw=FALSE, ppoints.cex=1, plabels.cex=0, main="Co-Inertia Scatter Plot with Groupings\nArrows from Axis 2 Position to Axis 1 Position\n", xlab="\nAxis 1, Constraining Data Set Variance\n", ylab="Axis 2, Main Data Set Variance\n", plot = FALSE)
    scatter.env <- s.label(cia$mY, pgrid.draw=FALSE, ppoints.cex=1, plabels.cex=0, main="Co-Inertia Scatter Plot with Groupings\nArrows from Axis 2 Position to Axis 1 Position\n", xlab="\nAxis 1, Constraining Data Set Variance\n", ylab="Axis 2, Main Data Set Variance\n", plot = FALSE)
    plot_1 <- superpose(scatter.var, scatter.env, plot=FALSE)
    plot_2 <- s.match(scatter.var@data$dfxy, scatter.env@data$dfxy)
    update(plot_2, plabels.box.draw=TRUE, pgrid.draw=FALSE, plabels.cex=.8, plabels.optim=TRUE, plabels=row.names(input), ppoints.cex=0.5, psub.cex=0, plot=TRUE, main="Co-Inertia Scatter Plot\nArrows from Axis 2 Position to Axis 1 Position\n", xlab="\nAxis 1, Constraining Data Set Variance\n", ylab="Axis 2, Main Data Set Variance\n")
  } else { #Yes meta data
    if (is.null(meta_group)) { #default is no meta grouping with color
      scatter.var <- s.label(cia$mX, pgrid.draw=FALSE, ppoints.cex=1, plabels.cex=0, main="Co-Inertia Scatter Plot with Groupings\nArrows from Axis 2 Position to Axis 1 Position\n", xlab="\nAxis 1, Constraining Data Set Variance\n", ylab="Axis 2, Main Data Set Variance\n", plot = FALSE)
      scatter.env <- s.label(cia$mY, pgrid.draw=FALSE, ppoints.cex=1, plabels.cex=0, main="Co-Inertia Scatter Plot with Groupings\nArrows from Axis 2 Position to Axis 1 Position\n", xlab="\nAxis 1, Constraining Data Set Variance\n", ylab="Axis 2, Main Data Set Variance\n", plot = FALSE)
      plot_1 <- superpose(scatter.var, scatter.env, plot=FALSE)
      plot_2 <- s.match(scatter.var@data$dfxy, scatter.env@data$dfxy)
      update(plot_2, plabels.box.draw=TRUE, pgrid.draw=FALSE, plabels.cex=.8, plabels.optim=TRUE, plabels=row.names(input), ppoints.cex=0.5, psub.cex=0, plot=TRUE, main="Co-Inertia Scatter Plot\nArrows from Axis 2 Position to Axis 1 Position\n", xlab="\nAxis 1, Constraining Data Set Variance\n", ylab="Axis 2, Main Data Set Variance\n")
    } else { #grouping by meta data with color
      if (meta_colname=="NULL") { #if no column user selected
        meta_colname1 <- colnames(metaData)[1] #Default is first meta data column
      } else { #User selected, java uses meta.columns function below to offer user options in drop down menu
        meta_colname1 <- meta_colname
      }
     
      n <- length(levels(metaData[,meta_colname1])) #Determine how many different colors are needed based on the levels of theg rouping data column of interest
     
      if (color=="NULL") { #Default colors
        colors <- viridis(n+1) #Assign a color to each level using the viridis pallete (viridis package)
      } else if (color=="plasma") {
        colors <- plasma(n+1) #Assign a color to each level using the plasma pallete (viridis package)
      } else if (color=="grey") {
        colors <- grey.colors(n, start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
      } else {
        colors <- "black"
      }
     
      scatter.var <- s.class(cia$mX, fac = metaData[,meta_colname1], ellipseSize=0, pgrid.draw=FALSE, ppoints.cex=1, starSize = 0, plabels.cex=0, col=colors, main="Co-Inertia Scatter Plot with Groupings\nArrows from Axis 2 Position to Axis 1 Position\n", xlab="\nAxis 1, Constraining Data Set Variance\n", ylab="Axis 2, Main Data Set Variance\n", plot = FALSE)
      scatter.env <- s.class(cia$mY, fac = metaData[,meta_colname1], ellipseSize=0, pgrid.draw=FALSE, ppoints.cex=1, starSize = 0, plabels.cex=0, col=colors, main="Co-Inertia Scatter Plot with Groupings\nArrows from Axis 2 Position to Axis 1 Position\n", xlab="\nAxis 1, Constraining Data Set Variance\n", ylab="Axis 2, Main Data Set Variance\n", plot = FALSE)
      plot_1 <- superpose(scatter.var, scatter.env, plot=FALSE)
      plot_2 <- s.match(scatter.var@stats$means, scatter.env@stats$means, plabels.cex = .9, labels=c(levels(metaData[,meta_colname1])), plabels.boxes.border=colors, plines.lwd = 2, plot = FALSE)
      superpose(plot_1, plot_2, plot=TRUE)
    }
  }
  dev.off()
}




#'Produce CIA loadings plot
#'@description Produce co-inertia analysis loadings plot
#'@param mSetObj Input name of the created mSet Object
#'@param dataSet Select main data set or environmental dataset
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png"
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
Plot.cia.loading <- function(mSetObj=NA, dataSet="NULL", imgName, format="png", dpi=72, width=NA) {
 
  library("ade4")
  library("adegraphics")
 
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  cia <- mSetObj$analSet$cia$cia
 
  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
 
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$Plot.cia.loading <- imgName
 
  #Produce
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
 
  if (dataSet=="NULL") { #Main data set
    s.arrow(cia$c1, plabels.box.draw=FALSE, pgrid.draw=FALSE, plabels.cex=1, psub.cex=0, plot=TRUE, xlab="\nLoadings 1\n", ylab="Loadings 2\n", main="Co-Inertia Analysis Loading Plot\n")
  } else { #env data set
    s.arrow(cia$l1, plabels.box.draw=FALSE, pgrid.draw=FALSE, plabels.cex=1, psub.cex=0, plot=TRUE, xlab="\nLoadings 1\n", ylab="Loadings 2\n", main="Co-Inertia Analysis Loading Plot\n")
  }
  dev.off()
}



#'Produce CIA scree plot
#'@description Produce co-inertia analysis scree plot
#'@param mSetObj Input name of the created mSet Object
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png"
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images,
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
Plot.cia.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
 
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  eigenValues <- mSetObj$analSet$cia$eigenValues
 
  eigenValues_data <- cbind(eigenValues, eigenValues/sum(eigenValues))
  maxVar <- max(eigenValues/sum(eigenValues))
  n <- nrow(eigenValues_data)
  eigenValues_data <- as.data.frame(cbind(1:n, eigenValues_data))
  colnames(eigenValues_data) <- c("Axis", "Eigen_Value", "Variance_Explained")
 
  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
 
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$Plot.cia.scree <- imgName
 
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  plot(x=eigenValues_data$Axis, y=eigenValues_data$Variance_Explained, type="l", xlim=c(1, n), ylim=c(0, maxVar+0.1), xlab="Axis", ylab="Proportion of Variance Explained", main="Co-Inertia Analysis Scree Plot",  xaxt="n", yaxt="n", col="blue", lwd=2)
  points(x=eigenValues_data$Axis, y=eigenValues_data$Variance_Explained, cex=1.1, pch=19, col="blue")
  axis(2, las=2)
  axis(1, at=1:n)
  dev.off()
}




##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Determine number and names ofgrouping data columns for ordination plotting'
#'@description Java will use thegrouping data columns to enable user options for selectinggrouping data for ordination plotting
#'@param mSetObj Input name of the created mSetObject
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

cia.meta.columns <- function(mSetObj=NA) {
 
  mSetObj <- .get.mSet(mSetObj)
 
  metaData <- mSetObj$analSet$cia$metaData
  name.all.meta.cols <- colnames(metaData)
 
  return(name.all.meta.cols)
 
}


#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.cia.get.results <- function(mSetObj=NA){
 
  mSetObj <- .get.mSet(mSetObj)
  ord.cia.result <- c(mSetObj$analSet$cia)
  return(ord.cia.result)
 
}