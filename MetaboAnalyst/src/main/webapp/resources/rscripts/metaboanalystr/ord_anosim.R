#'Perform ANOSIM
#'@description Perform Analysis of Similarities
#'@param mSetObj Input name of the created mSet Object
#'@param distance Input distance as one of "euclidean" (default), "manhattan", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
#'@param env_col  Set column of env data to use for plotting, default is first categorical column
#'@param data Which data set to use, normalized (default) or original
#'@param binary Boolean, is data absence/presence data (default is no)
#'@author Louisa Normington\email{normingt@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export
ord.anosim <- function(mSetObj=NA, data="NULL", distance="NULL", binary="false", env_col="NULL") {

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
  
  input <- input[order(as.numeric(row.names(input))),] #Order rows
  envData <- mSetObj$dataSet$origEnv #Compulsory
  
  #Obtain numeric data for anosim
  num_data <- select_if(input, is.numeric) 
  if (ncol(num_data)<1) {
    #AddErrMsg("Your main data set does not contain any numeric variables! Please choose a different main data set.")
    stop("Your main data set does not contain any numeric variables! Please upload a different main data set.")
  }
  
  #Obtain env categorical data for anosim
  char_data <- select_if(envData, is.character)
  if (ncol(char_data)<1) {
    #AddErrMsg("Your constraining data set does not contain any categorical variables! Please choose a different constraining data set.")
    stop("Your constraining data set does not contain any categorical variables! Please uplaod a different constraining data set.")
  }
  
  #Set env column for analysis
  if (env_col=="NULL") {
    env_col1 <- colnames(char_data)[1]# Default is to choose the first categorical column
  } else {
    env_col1 <- env_col #User selected, java uses function anosim.env.columns() to provide options in drop down menu (only factor columns are available)
  }
  
  #Set distance measure for creation of dissimilarity matrix
  if (distance=="NULL") {
    distance1 <- "euclidean" #Default distance
  } else {
    distance1 <- distance #USer selected from list "euclidean", "bray", "manhattan", "canberra", "kulczynski", "jaccard", "gower", "altGower", "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao", "mahalanobis"
  } 
  
  #Generate dissimilarity matrix
  if (binary=="false") {
    dist <- vegdist(num_data, method=distance1, binary=FALSE) #Generate dissimilarity matrix
  } else {
    dist <- vegdist(num_data, method=distance1, binary=TRUE) #Generate dissimilarity matrix for presence/absence data
  }
  
  #Perform analysis of similarity
  ano <- anosim(dist, char_data[,env_col1], permutations = 999, distance=distance1) #Run analysis
  
  #Store results in mSetObj$analSet$rda
  mSetObj$analSet$anosim$name <- "ANOSIM"
  mSetObj$analSet$anosim$anosim <- ano
  mSetObj$analSet$anosim$num_data <- num_data
  mSetObj$analSet$anosim$char_data <- char_data
  mSetObj$analSet$anosim$envData <- envData
  mSetObj$analSet$anosim$env_col <- env_col1
  mSetObj$analSet$anosim$distance <- ano$dissimilarity
  mSetObj$analSet$anosim$dist.matrix <- as.matrix(dist)
  
  #Download relevent data
  write.csv(as.matrix(dist), file="anosim_dissimilarity_matrix.csv", row.names=TRUE)

  sink("anosim_summary.txt")
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
Plot.anosim <- function(mSetObj=NA, color="NULL", imgName, format="png", dpi=72, width=NA) {
  
  library("vegan")
  library("viridis")
  
  #Extract necessary objects from mSetObj
  mSetObj <- .get.mSet(mSetObj)
  ano <- mSetObj$analSet$anosim$ano
  env_col <- mSetObj$analSet$anosim$env_col
  char_data <- mSetObj$analSet$anosim$char_data

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
  
  #ANOSIM plot
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1)) 
  
  #Color options
  n <- length(levels(as.factor(char_data[,env_col]))) #Determine how many different colors are needed based on the levels of the meta data
  if (color=="NULL") { #viridis is default
    colors <- viridis(n+1)#Assign a color to each level using the viridis pallete (viridis package)
    plot(ano, main="Analysis of Similarities", xlab=paste0(env_col), ylab="Dissimilarity Rank Between Samples", yaxt="n", col=colors)
  } else if (color=="plasma") {
    colors <- plasma(n+2)#Assign a color to each level using the plasma pallete (viridis package)
    plot(ano, main="Analysis of Similarities", xlab=paste0(env_col), ylab="Dissimilarity Rank Value", yaxt="n", col=colors)
  } else if (color=="grey") {
    colors <- grey.colors(n+1, start=0.2, end=1) #Assing a grey color to each level (grDevices package- automatically installed)
    plot(ano, main="Analysis of Similarities", xlab=paste0(env_col), ylab="Dissimilarity Rank Value", yaxt="n", col=colors)
  } else { #no color
    plot(ano, main="Analysis of Similarities", xlab=paste0(env_col), ylab="Dissimilarity Rank Value", yaxt="n")
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
anosim.env.columns <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj)
  envData <- mSetObj$analSet$anosim$envData

  library("dplyr")
  
  data <- select_if(envData, is.character)
  data_colnames <- colnames(data)
  
  return(data_colnames)
  
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