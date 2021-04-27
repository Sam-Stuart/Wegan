#'Perform ANOSIM
#'@description Perform Analysis of Similarities
#'@param mSetObj Input name of the created mSet Object
#'@param distance Input distance as one of "euclidean" (default), "manhattan", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
#'@param facA  Set column of env data to use for plotting, default is first categorical column
#'@param data Which data set to use, normalized (default) or original
#'@param binary Boolean, is data absence/presence data (default is no)
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.anosim <- function(mSetObj=NA, data="NULL", facA="NULL", distance="NULL", binary="NULL") {

  library("vegan")
  library("dplyr")
  library("viridis")

  #Obtain mSet dataset
  mSetObj <- .get.mSet(mSetObj)
  if (data=="NULL") {
    input <- mSetObj$dataSet$norm
  } else {
    input <- mSetObj$dataSet$orig
  }
  
  envData1 <- envData #User uploaded, compulsary
  
  #Obtain numeric data for anosim
  num_data <- select_if(input, is.numeric) 
  if (ncol(num_data)<1) {
    #AddErrMsg("Your main data set does not contain any categorical data! Please choose a different main data set.")
    stop("Your main data set does not contain any numeric data! Please uplaod a different main data set.")
  }
  
  #Obtain env factor data for anosim
  fac_data <- select_if(envData1, is.factor)
  if (ncol(fac_data)<1) {
    #AddErrMsg("Your environmental data set does not contain any categorical data! Please choose a different environmental data set.")
    stop("Your environmental data set does not contain any categorical data! Please uplaod a different environmental data set.")
  }
  
  #Set env factor column for analysis
  if (facA=="NULL") {
    facA1 <- colnames(fac_data)[1]# Default is to choose the first categorical column
  } else {
    facA1 <- facA #User selected, java uses function factor.columns() to provide options in drop down menu (only factor columns are available)
  }
  
  #Set distance measure for creation of dissimilarity matrix
  if (distance=="NULL") {
    distance1 <- "euclidean" #Default distance
  } else {
    distance1 <- distance #USer selected from list "euclidean", "bray", "manhattan", "canberra", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
  } 
  
  #Generate dissimilarity matrix
  if (binary=="NULL") {
    dist <- vegdist(num_data, method=distance1, binary=FALSE) #Generate dissimilarity matrix
  } else {
    dist <- vegdist(num_data, method=distance1, binary=TRUE) #Generate dissimilarity matrix for presence/absence data
  }
  
  #Perform analysis of similarity
  ano <- anosim(dist, fac_data[,facA1], permutations = 999, distance=distance1) #Run analysis
  
  #Store results in mSetObj$analSet$rda
  mSetObj$analSet$anosim$name <- "ANOSIM"
  mSetObj$analSet$anosim$anosim <- ano
  mSetObj$analSet$anosim$num_data <- num_data
  mSetObj$analSet$anosim$fac_data <- fac_data
  mSetObj$analSet$anosim$facA <- facA1
  mSetObj$analSet$anosim$distance <- ano$dissimilarity
  mSetObj$analSet$anosim$dist.matrix <- as.matrix(dist)
  
  #Download relevent data
  write.csv(as.matrix(dist), file=paste0("anosim_", distance1, "_dissimilarity_matrix.csv"), row.names=TRUE)

  sink(paste0("anosim_", distance1, "_summary.txt"))
  summary(ano)
  sink()
  
  return(.set.mSet(mSetObj))

}




#'Produce ANOSIM plot
#'@description Produce ANOSIM plot, where "Between" is difference between groups vs within groups, thickness is sample size, and the greater the height, the greater the difference between samples
#'@param mSetObj Input name of the created mSet Object
#'@param color Set the color pallette, one of viridis (default), plasma, grey, none
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
Plot.anosim <- function(mSetObj=NA, color=NULL, imgName, format="png", dpi=72, width=NA) {
  
  library("vegan")
  library("viridis")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  ano <- mSetObj$analSet$anosim$ano
  facA <- mSetObj$analSet$anosim$facA
  fac_data <- mSetObj$analSet$anosim$fac_data

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
  mSetObj$imgSet$Plot.anosim <- imgName
  
  #Scree plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  
  #Color options
  n <- length(levels(fac_data[,facA])) #Determine how many different colors are needed based on the levels of the meta data
  if (is.null(color)) { #viridis is default
    colors <- viridis(n+1)#Assign a color to each level using the viridis pallete (viridis package)
    plot(ano, main="Analysis of Similarities", xlab=paste0(facA), ylab="Dissimilarity Rank Between Samples", yaxt="n", col=colors)
  } else if (color=="plasma") {
    colors <- plasma(n+2)#Assign a color to each level using the plasma pallete (viridis package)
    plot(ano, main="Analysis of Similarities", xlab=paste0(facA), ylab="Dissimilarity Rank Value", yaxt="n", col=colors)
  } else if (color=="grey") {
    colors <- grey.colors(n+1, start=0.2, end=1) #Assing a grey color to each level (grDevices package- automatically installed)
    plot(ano, main="Analysis of Similarities", xlab=paste0(facA), ylab="Dissimilarity Rank Value", yaxt="n", col=colors)
  } else { #no color
    plot(ano, main="Analysis of Similarities", xlab=paste0(facA), ylab="Dissimilarity Rank Value", yaxt="n")
  }
  axis(2, las=2)

  dev.off()
  
  return(.set.mSet(mSetObj))
  
}




##############################################
##############################################
########## Utilities for web-server ##########
##############################################
##############################################

#'Determine number and names of factor columns for Analysis of Similarities'
#'@description Java will use the results to enable user options for selecting environment data column
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
factor.columns <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  
  library("dplyr")
  
  data <- select_if(envData, is.factor)
  count.all.fac.cols <- ncol(data)
  name.all.fac.cols <- colnames(data)
  
  fac.col.results <- list(
    count=count.all.fac.cols,
    names=name.all.fac.cols
  )
  
  return(fac.col.results)
  
}

#'Obtain results'
#'@description Java will use the stored results as needed for the results page
#'@param mSetObj Input name of the created mSetObject 
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.anosim.get.results <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  ord.anosim.result <- c(mSetObj$analSet$anosim)
  return(ord.anosim.result)
  
}