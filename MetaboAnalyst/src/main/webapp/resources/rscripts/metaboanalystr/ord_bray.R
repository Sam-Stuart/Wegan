#'Perform Polar Ordination
#'@description Perform polar (Bray-Curtis) ordination
#'@param mSetObj Input name of the created mSet Object
#'@param abundance Boolean, Use abundance transformation, default is no change, else relative abundance used (divide by column total)
#'@param distance Set distance metric for building dissimilarity matrix
#'@param binary Boolean, Indicates if data is presence/absence data (default is no)
#'@param data Boolean, Which data set to use, normalized (default) or original
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.bray <- function(mSetObj=NA, abundance="NULL", distance="NULL", data=NULL, binary=NULL) { 
  
  library("natto")
  library("vegan")
  library("dplyr") #For easy data manipulation
  
  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (is.null(data)) {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  #Obtain numeric data for ordination
  num_data <- select_if(input, is.numeric) #Numeric data only for Bray-Curtis ordination
  
  #Transform abundance data
  print("Should you have community species data, you may want to investigate the relative abundance (divide all values by column totals) versus absolute abundance (no change to data).")
  if (abundance=="NULL") {
    abundance1 <- "absolute"
    num_data1 <- num_data #Default abundance is absolute and no change is made to data
  } else {
    abundance1 <- "relative"
    num_data1 <- decostand(num_data, method="total") #Alternative option is relative abundance, each value is divided by the column sum
  }
  
  #Set distance for dissimilarity matrix
  if (distance=="NULL") {
    distance1 <- "euclidean" #Default distance
  } else {
    distance1 <- distance #USer selected from list "euclidean" (default), "manhattan", "canberra", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
  } 
  
  #Generate dissimilarity matrix
  if (is.null(binary)) {
    dist <- vegdist(num_data1, method=distance1, binary=FALSE) #Generate dissimilarity matrix
  } else {
    dist <- vegdist(num_data1, method=distance1, binary=TRUE) #Generate dissimilarity matrix for presence/absence data
  }
  
  #Performed Bray-Curtis Ordination
  polar <- polarord(dist, k = 6)
  metaData = FALSE
  #Get meta data, used to group samples for plotting
  if (is.data.frame(metaData)==FALSE) { #No user uplaoded meta data
    fac_data <- select_if(input, is.factor)
    count.fac.cols <- ncol(fac_data)
    if (count.fac.cols >= 1) { #If main data set had at least one categorical column, call it meta data
      metaData1 <- as.data.frame(fac_data)
    } else {
      metaData1 <- "NA" #If main data set had no categorical columns, meta data is NULL
      #AddErrMsg("No groupings columns were detected. If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead ofgrouping data using 1, 2, and 3, use I, II and III.")
      print("No groupings columns were detected! If this is a mistake, make sure that groupings columns use characters and not numbers. For example, instead of grouping data using 1, 2, and 3, use I, II and III.")
    }
  } else {  #User uploaded meta data
    metaData1 <- metaData #User uploaded like weights in correlation module
    if (nrow(metaData1)!=nrow(input)) {
      #AddErrMsg("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
      stop("Your grouping data does not have the same number of rows as your numerical data! Please check that you grouping data is correct.")
    }
    for(i in 1:ncol(metaData1)) {
      metaData1[,i] <- as.factor(metaData1[,i]) #Make sure all columns are read as factors
    }
  }
  
  #Fit variables
  var_fit <- envfit(polar, num_data1, permutations=999, p.max=NULL)

  #Extract scores
  sample_scores <- polar$points
  var_scores <- var_fit$vectors$arrows
  
  #Other summary elements
  inertia <- polar$inertia
  eigenValues <- polar$eig
  endpoints <- polar$endpoints
  call <- polar$call

  #Store results
  mSetObj$analSet$bray$name <- "Bray-Curtis"
  mSetObj$analSet$bray$bray <- polar
  mSetObj$analSet$bray$distance <- distance1
  mSetObj$analSet$bray$input <- num_data
  mSetObj$analSet$bray$eigenvalues <- eigenValues
  mSetObj$analSet$bray$var_fit <- var_fit
  mSetObj$analSet$bray$metaData <- metaData1
  mSetObj$analSet$bray$var_scores <- var_scores
  
  #Save tables and text files
  eigenValues_data <- cbind(polar$eig, polar$eig/sum(polar$eig))
  eig_rownames <- 1:nrow(eigenValues_data)
  eigenValues_data <- as.data.frame(cbind(eig_rownames, eigenValues_data))
  colnames(eigenValues_data) <- c("Dimension", "Eigen_Value", "Variance_Explained")
  write.csv(eigenValues_data, file="bray_curtis_scree_data.csv", row.names=FALSE)
  write.csv(sample_scores, file="bray_curtis_row_scores.csv", row.names=row.names(input))
  write.csv(var_scores, file="bray_curtis_column_scores.csv", row.names=TRUE)
  write.csv(as.matrix(dist), file=paste0("bray_curtis_", distance1, "_dissimilarity_matrix.csv"), row.names=TRUE)
  
  sink("column_impact_on_bray_curtis.txt") 
  cat("Data columns may significantly impact Bray-Curtis ordination\n")
  print(var_fit)
  sink() 
  
  sink("bray_curtis_summary.txt")
  cat("Bray-Curtis (Polar) Ordination\n")
  cat("\nCall:\n")
  print(call)
  cat(paste0("\nDistance Metric = ", distance1, "\n"))
  cat("\nSites Scores\n")
  print(as.data.frame(sample_scores))
  cat("\nSpecies Scores\n")
  print(var_scores)
  cat("\nInertia\n")
  inertia
  cat("\nEigenvalues\n")
  print(eigenValues)
  cat("\nEndpoints\n")
  print(endpoints)
  cat("\nLengend:\n")
  cat("Site=Rows and Species=Columns\n")
  sink()
  
  return(.set.mSet(mSetObj))
  
}



#'Produce Bray-Curtis 2D ordination plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
#'@description Produce Bray-Curtis ordination plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color #Viridis pallete, options include "viridis" (default), "plasma" and "cividis"
#'@param ellipse Boolean, TRUE to add confidence ellipses, FALSE (default) to not add confidence ellipses
#'@param var_arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
#'@param sampleNames Boolean, TRUE to display data as variable names, FALSE (default) to display data as points
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
#'@param point_options Boolean, TRUE to use points for grouping by color, FALSE (default) uses points all the same color
#'@param meta_col_point Meta data column to use for plotting points, Can be user inputted where options are given to java using function meta.columns()
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
Plot.bray.2D <- function(mSetObj=NA, color="NULL", ellipse="NULL", var_arrows=NULL, sampleNames=NULL, meta_col_color="NULL", point_options=NULL, meta_col_point="NULL", imgName, format="png", dpi=72, width=NA) {

  library("vegan")
  library("viridis") 
  print(var_arrows)
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  bray <- mSetObj$analSet$bray$bray
  metaData <- mSetObj$analSet$bray$metaData
  input <- mSetObj$analSet$bray$input
  var_fit <- mSetObj$analSet$bray$var_fit
  
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
  mSetObj$imgSet$Plot.bray.2D <- imgName
  
  #Produce 2D ordination plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  ordiplot(bray$points, type="n", main="Bray-Curtis Analysis", yaxt="n", xlab="Dim 1", ylab="Dim 2", choices=c(1,2)) #Empty ordination plot 
  axis(2, las=2)
  
  #Plot with and without ellipses/sample labels/metadata options/variable arrows/env data arrows
  if (is.data.frame(metaData)==FALSE) { #If no meta data
    print("NULL metadata")
    #point options
    if (sampleNames=="true") { #If display data as lables
      text(bray$points) #Add text for samples
    } else {
      points(bray$points, pch=19, col="black") #Add text for samples
    }
    
    #arrow options
    if (var_arrows=="true") { #If variable arrows selected
      print("inside the car arrows")
      plot(var_fit, col="darkred", choices=c(1,2), lwd=2, at=c(max(bray[["points"]][,1])/2, max(bray[["points"]][,2])/2))
    }
    
  } else { #If meta data available
    
    #Set up meta data column to use for colors
    if (meta_col_color=="NULL") { 
      meta_col_color_data <- as.factor(metaData[,1]) #Default meta data column for labeling with color is the first
      meta_col_color_name <- colnames(metaData)[1]
    } else {
      meta_col_color_data <- as.factor(metaData[,meta_col_color]) #User imputted meta data column for labeling with colors, options given to java using function meta.columns() below
      meta_col_color_name <- meta_col_color
    }
    
    #Set up meta data column to use for points
    if (meta_col_point=="NULL") { 
      meta_col_point_data <- as.factor(metaData[,1]) #Default meta data column for labeling with points is the first
      meta_col_point_name <- colnames(metaData)[1]
    } else {
      meta_col_point_data <- as.factor(metaData[,meta_col_point]) #User imputted meta data column for labeling with points, options given to java using function meta.columns() below
      meta_col_point_name <- meta_col_point #User defined
    }
    
    #Color options
    n <- length(levels(meta_col_color_data)) #Determine how many different colors are needed based on the levels of the meta data
    if (color=="NULL") {
      colors <- viridis(n)#Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1)#Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assign a grey color to each level (grDevices package- automatically installed)
    } else { 
      color <- "none"
    }
    
    #Assign colors to points
    if (color=="none") {
      cols <- "black" #color none means black points
    } else {
      cols <- colors[meta_col_color_data] #Color pallete applied for groupings of user's choice
    }
    
    #point options
    pch_options <- c(19, 17, 15, 18, 1, 2, 0, 5, 6, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14)
    
    if (is.null(point_options)) { #No grouping by points (default)
      pchs <- 19 #No points option gets solid circle
    } else {
      pchs <- pch_options[meta_col_point_data] #Otherwise use points for grouping by color
    }
    
    if (sampleNames=="true") { #If display data as lables
      with(metaData, text(bray$points, col=cols, bg=cols)) # Text for samples
    } else { #display data as points
      if (is.null(point_options)) { #No grouping by points (default)
        with(metaData, points(bray$points, col=cols, pch=pchs, bg=cols)) 
        with(metaData, legend("bottomright", legend=levels(meta_col_point_data), col="black", pch=unique(pchs), pt.bg="black", title=meta_col_point_name))
      } else { #No point options
        with(metaData, points(bray$points, col=cols, pch=pchs, bg=cols)) 
      }
    }
    
    #arrow options
    if (var_arrows=="true") { #If variable arrows selected
      plot(var_fit, col="darkred", lwd=2, at=c(max(bray[["points"]][,1])/2, max(bray[["points"]][,2])/2)) #Set color, linewidth and place origin in the middle of the plot
    }
    
    #Ellipse option
    if (ellipse=="true") { #if ellipses selected
      with(metaData, ordiellipse(bray, meta_col_color_data, kind="sd", draw="polygon", border=colors, lwd=2)) # Include standard deviation ellipses that are the same color as the text.
    }
    
    #Legend
    with(metaData, legend("topright", legend=levels(meta_col_color_data), col=colors, pch=19, title=meta_col_color_name)) # Include legend for colors in figure   
  }
  print("at the end")
  dev.off()
  
  return(.set.mSet(mSetObj))
  
}


#'Produce Bray-Curtis 3D ordination plot
#'@description Rotate Bray-Curtis analysis
#'@param mSetObj Input name of the created mSet Object
#'@param color Input color name, options include "viridis" (default), "plasma" and "grey"
#'@param var_arrows Boolean, TRUE to produce variable arrows, FALSE (default) to produce ordination plot without variable arrows
#'@param meta_col_color Meta data column to use for plotting colors, Can be user inputted where options are given to java using function meta.columns()
#'@param imgName Input the image name
#'@param format Select the image format, only option is a json object that will be used by javascript to produce an interactive 3D plot
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
Plot.bray.3D <- function(mSetObj=NA, color="NULL", var_arrows=NULL, meta_col_color="NULL", imgName, format="json"){
  
  library("viridis")
  library("RJSONIO")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  input <- mSetObj$analSet$bray$input
  bray <- mSetObj$analSet$bray$bray
  metaData <- mSetObj$analSet$bray$metaData
  var_scores <- mSetObj$analSet$bray$var_scores

  #Create list to hold plot items
  bray3D_plot <- list()
  
  #Samples (rows)
  bray3D_plot$score$axis <- paste("Dim", c(1, 2, 3), sep="")
  sampleCoords <- data.frame(t(as.matrix(bray$points[,1:3])))
  colnames(sampleCoords) <- NULL
  bray3D_plot$score$xyz <- sampleCoords
  bray3D_plot$score$name <- rownames(input)
  
  if (is.data.frame(metaData)==FALSE) { #If no meta data
    bray3D_plot$score$color <- "NA"
  } else { #If meta data
    
    #Set up meta data column to use for colors
    if (meta_col_color=="NULL") { 
      meta_col_color_data <- as.factor(metaData[,1]) #Default meta data column for labeling with color is the first
      meta_col_color_name <- colnames(metaData)[1]
    } else {
      meta_col_color_data <- as.factor(metaData[,meta_col_color]) #User imputted meta data column for labeling with colors, options given to java using function meta.columns() below
      meta_col_color_name <- meta_col_color
    }

    #Color options
    n <- length(levels(meta_col_color_data)) #Determine how many different colors are needed based on the levels of the meta data
    if (color=="NULL") {
      color <- "viridis" #Default
      colors <- viridis(n) #Assign a color to each level using the viridis pallete (viridis package)
    } else if (color=="plasma") {
      colors <- plasma(n+1) #Assign a color to each level using the plasma pallete (viridis package)
    } else if (color=="grey") {
      colors <- grey.colors(n, start=0.1, end=0.75) #Assing a grey color to each level (grDevices package- automatically installed)
    } else { 
      color <- "none"
    }
    
    #Assign colors
    if (color=="none") {
      cols <- "black"
    } else {
      cols <- colors[meta_col_color_data]
    }
    bray3D_plot$score$colors <- col2rgb(cols)
    cols <- unique(GetColorSchema(mSetObj));
    rgbcols <- col2rgb(cols);
    cols <- apply(rgbcols, 2, function(x){paste("rgb(", paste(x, collapse=","), ")", sep="")})
    bray3D_plot$score$colors <- cols;

  }
  
  #Variables (columns)
  if (is.null(var_arrows)==FALSE) { #produce ordination plot with variable arrows
  variableCoords <- data.frame(t(as.matrix(var.scores)))
  colnames(variableCoords) <- NULL
  nmds3D_plot$scoreVar$axis <- paste("Dim", c(1, 2, 3), sep="")
  nmds3D_plot$scoreVar$xyzVar <- variableCoords
  nmds3D_plot$scoreVar$nameVar <- colnames(input)
  nmds3D_plot$scoreVar$colorVar <- col2rgb("darkred")
  } 

  
  #Metaboanalyst included this code block. Unsure what it does.
  if(mSetObj$dataSet$type.cls.lbl=="integer"){
    cls <- as.character(sort(as.factor(as.numeric(levels(mSetObj$dataSet$cls))[mSetObj$dataSet$cls])))
  }else{
    cls <- as.character(mSetObj$dataSet$cls)
  }
  if(all.numeric(cls)){
    cls <- paste("Group", cls)
  }
  bray3D_plot$score$facA <- cls
  print("before json")
  print(bray3D_plot)
  imgName=paste(imgName, ".", format, sep="")
  json.obj <- RJSONIO::toJSON(bray3D_plot, .na='null')
  sink(imgName)
  cat(json.obj)
  print("after json")
  sink()
  
  if(!.on.public.web){
    return(.set.mSet(mSetObj))
  }
}




#'Produce Bray-Curtis scree plot
#'@description Produce Bray-Curtis scree plot
#'@param mSetObj Input name of the created mSet Object
#'@param dim Number of dimensions for x-axis
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
Plot.bray.scree <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  eigenvalues <- mSetObj$analSet$bray$eigenvalues

  #Prepare eigenvalue data for plotting
  eigenvalues_data <- as.data.frame(cbind(1:6, eigenvalues/sum(eigenvalues))) #Divide each eigenvalue with the sum of all eigenvalues in order to obtain the variance explained by each dimension

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
  mSetObj$imgSet$Plot.bray.scree <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  plot(x=eigenvalues_data$V1, y=eigenvalues_data$V2, type="l", xlim=c(1, 6), ylim=c(0, 1), xlab="Dimension", ylab="Proportion of Variance Explained", main="Bray-Curtis Analysis Scree Plot", yaxt="n", xaxt="n", col="blue", lwd=2)
  points(x=eigenvalues_data$V1, y=eigenvalues_data$V2, cex=1.1, pch=19, col="blue")
  axis(2, las=2)
  axis(1, at=1:6)
  dev.off()
}



##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Determine number and names of meta data columns for ordination plotting'
#'@description Java will use the meta data columns to enable user options for selecting meta data for ordination plotting
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

meta.columns <- function(mSetObj=NA) {
  
  mSetObj <- .get.mSet(mSetObj)
  
  metaData <- mSetObj$analSet$bray$metaData
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
ord.bray.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.bray.result <- c(mSetObj$analSet$bray)
  return(ord.bray.result)
  
}