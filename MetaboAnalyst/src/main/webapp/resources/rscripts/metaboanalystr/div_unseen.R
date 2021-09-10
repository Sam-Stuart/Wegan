
#'Perform unseen species analysis 
#'@description Perform unseen species analysis 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param permutations Input number of permutations with method = "random", default is 100
#'@param pool Input a vector giving a classification for pooling the sites in the species data, default is the first row of metaData
#'@param smallsample Input small sample correction (N-1)/N, where N is the number of sites within the pool. 
#'@param index Input selected index of extrapolated richness, drop down options are "jack1"(default), "jack2", "chao", "boot","Species"
#'@param parallel Input number of parallel processes or a predefined socket cluster, default is -1
#'@param minsize Input the smallest number of sampling units reported, default is 3.
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export

sp_pool <- function(mSetObj = NA, data = "false", pool = "", smallsample = "false", 
                         index = "NULL", permutations = "", minsize = "", parallel = "") {
  print("start model")
  options(errors = traceback)   
  
  library("plyr")
  library("dplyr")
  library("vegan")
  
  mSetObj <- .get.mSet(mSetObj)
  #Extract input from mSetObj
  
  data(dune)
  data(dune.env)

  mSetObj$dataSet$norm <- dune
  mSetObj$dataSet$origMeta <- dune.env
  metaData <- mSetObj$dataSet$origMeta
  print(metaData)

  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  print(input)
  
  if (smallsample == "false") {
    smallsample1 = FALSE
  } else {
    smallsample1 = TRUE
  }
  print(smallsample1)
  
  if (index == "NULL") {
    index1 = "jack1"
  } else {
    index1 = index
  }
  print(index1)
  
  if (permutations == "") {
    permutations1 = 100
  } else {
    permutation1 <- as.numeric(permutations)
  }
  print(permutations1)
  
  if (minsize == "") {
    minsize1 = 3
  } else if (as.numeric(minsize) > count(input)) {
    cat ("minsize has to be smaller than the total row number of the uploaded dataset.")
  } else {
    minsize1 = as.numeric(minsize)
  }
  print(minsize1)
  
  if (parallel == "") {
    parallel1 = 1
  } else {
    parallel1 = parallel
  }
  print(parallel1)
  
  #if (pool == "") {
  #  pool1 <- metaData[,1]
  #} else {
  #  pool1 <- as.numeric(pool)
  #}
  #print(pool1)
  
  #if (display == "NULL") {
  #  display1 = "jack1"
  #} else {
  #  display1 = display
  #}
  
  #input.2 <- select_if(input, is.numeric)
  cat("Input dataset has to be pure numeric data")
  
  sp1_all <- specpool(input.2, smallsample = smallsample1)
  #print(sp1_all)
  if (pool == "") {
    sp1_in <- specpool(input.2, pool = metaData[,2], smallsample = smallsample1)
  } else {
    pool1 <- as.numeric(pool)
    sp1_in <- specpool(input.2, pool = metaData[,pool1], smallsample = smallsample1)
  }
  #print(pool1)
  print(sp1_in)
  est <- estimateR(input.2)
  sp2 <- specpool2vect(sp1_in, index = index1)
  print(sp2)
  pool.ac <- poolaccum(input.2, permutations = permutations1, minsize = minsize1)
  est.ac <- estaccumR(input.2, permutations = permutations1, parallel = parallel1)
  sum.sp_all <- summary(sp1_all, alpha = 0.05)
  sum.sp_in <- summary(sp1_in, alpha = 0.05)
  
  mSetObj$analset$sp1_all <- sp1_all
  mSetObj$analset$sp1_in <- sp1_in
  mSetObj$analset$est <- est
  mSetObj$analset$sp2 <- sp2
  mSetObj$analset$pool.ac <- pool.ac
  mSetObj$analset$est.ac <- est.ac
  mSetObj$analset$sum.sp_all <- sum.sp_all
  mSetObj$analset$sum.sp_in <- sum.sp_in
  mSetObj$analset$input <- input
  mSetObj$analset$env.data <- metaData
  
  write.csv(mSetObj$analset$sp1_all, "Incidence-based estimates_freq_all sites.csv")
  write.csv(mSetObj$analset$sp1_in, "Incidence-based estimates_freq_selected variable.csv")
  write.csv(mSetObj$analset$est, "Abundance-based estimates_counts.csv")
  write.csv(mSetObj$analset$sp2, "Pooled values.csv")
  write.csv(mSetObj$analset$pool.ac$means, "Extrapolated richness indices.csv")
  write.csv(mSetObj$analset$est.ac$means, "Number of species for random ordering of sampling units.csv")
  write.csv(mSetObj$analset$sum.sp_all, "Quantile envelopes of permutations_all sites.csv")
  write.csv(mSetObj$analset$sum.sp_in, "Quantile envelopes of permutations_selected variable.csv")
  
  return(.set.mSet(mSetObj)) 
  
  }


#'Produce a boxplot between the extrapolated species richness and selected groups
#'@description Produce a boxplot between the pooled values and selected groups
#'@param mSetObj Input name of the created mSet Object
#'@param plot_data Input the y-axis values, drop down options are pooled values using index "jack1" (default),"jack2", "chao", "boot" or "Species"
#'@param fac_data Input the row number in metaData, default is 1
#'@param box.color Input box color of boxplot, options are "skyblue" (default), "green", "turquoise", "steelblue", "peach", "wheat"  
#'@param border.col options include "blue" (default), "green", "turquoise" & "steelblue", "peach" & "wheat"
#'@param xlab Input x axis title, default is "Treatment"
#'@param ylab Input y axis title, default is "Estimate"
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

pool_boxplot <- function(mSetObj=NA, plot_data = "NULL", fac_data = "", box.color = "NULL", xlab = "", ylab = "",
                      border.col = "NULL", imgName, format="png", dpi=72, width=NA) {
  library(plyr)
  library(vegan)
  library(dplyr)
  
  mSetObj <- .get.mSet(mSetObj)
  
  if (plot_data == "NULL") {
    plot_data1 <- mSetObj$analset$sp2
  } else if (plot_data == "S") {
    plot_data1 <- mSetObj$analset$est.ac$S
  } else if (plot_data == "chao") {
    plot_data1 <- mSetObj$analset$est.ac$chao
  } else if (plot_data == "ace") {
    plot_data1 <- mSetObj$analset$est.ac$ace
  }
  print(plot_data1)
  metaData <- mSetObj$analset$env.data
  
  #metaData1 <- select_if(metaData, is.factor)
  if (fac_data == "") {
    treat = metaData[,1]
    treat <- factor(treat, levels = unique(treat))
  } else {
    fac_data1 <- as.numeric(fac_data)
    treat = metaData[ ,fac_data1]
    treat <- factor(treat, levels = unique(treat))
  }
  print(treat)
  #treat <- as.character(treat)
  
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
  mSetObj$imgSet$pool.plot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  #abline(0, 1)
    
  if(box.color == "NULL") { 
    box.color1 = "skyblue" #default fill palette is grayscale
  } else if (box.color == "green") { #manual user entry. Selection of this option causes text box to appear
    box.color1 = "palegreen"
  } else if (box.color == "turquoise") {
    box.color1 = "turquoise"
  } else if (box.color == "steelblue") {
    box.color1 = "lightsteelblue1"
  } else if (box.color == "peach") {
    box.color1 = "peachpuff"
  } else if (box.color == "wheat") {
    box.color1 = "wheat1"
  } 
  print(box.color1)
  
  if(border.col=="NULL") { 
    border.col1 = "blue" #default fill palette is grayscale
  } else if (border.col == "green") {
    border.col1 = "green"
  } else if (border.col == "turquoise") {
    border.col1 = "turquoise4"
  } else if (border.col == "steelblue") {
    border.col1 = "lightsteelblue4"
  } else if (border.col1 == "peach") {
    border.col1 = "tan2"  
  } else if (border.col1 == "wheat") {
    border.col1 <- "wheat3" 
  }
  print(border.col1)
  
  #pars <- expand.grid(col = box.color1, stringsAsFactors = FALSE)
  
  if (xlab == "") {
    xlab1 = "Treatment"
  } else {
    xlab1 = xlab
  }
  
  if (ylab == "") {
    ylab1 = "Estimate"
  } else {
    ylab1 = ylab
  }
  print(plot_data1)

  #windows(height = h, width = w)
  #ylim = c(0, max(plot_data1))
  boxplot(plot_data1 ~ treat, ann = T, yaxt = "n", col = box.color1, border = border.col1, xlab = xlab1, ylab = ylab1)
  axis(2, las = 2)
  title("boxplot of extrapolated species richness and selected groups")
  
  dev.off()
  
  return(.set.mSet(mSetObj))
}


#'Produce a series of line graphs between the matrices of permutation results for each richness estimator and sample size
#'@description Produce a series of line graphs between the matrices of permutation results for each richness estimator and sample size
#'@param mSetObj Input name of the created mSet Object
#'@param color Input box color of lines and CI, options are "black/gray" (default), "green", "red", "royalblue", "wheat", "darkslategray"  
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2)
#'@export

rich_est_curve <- function(mSetObj=NA, color="NULL", imgName, format="png", dpi=72, width=NA) {

  library(plyr)
  library(dplyr)
  library(vegan)
  
  mSetObj <- .get.mSet(mSetObj)
  
  line_data <- mSetObj$analset$pool.ac
  print(line_data) 

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w/2
  
  if (color == "NULL") { 
    color1 = c("black", "gray") #default fill palette is grayscale
  } else if (color == "green") { #manual user entry. Selection of this option causes text box to appear
    color1 <- c("forestgreen", "lightgreen")
  } else if (color == "red") { 
    color1 <- c("indianred1", "lightpink")
  } else if (color == "royalblue") { 
    color1 <- c("slateblue4", "lightsteelblue1")
  } else if (color == "wheat") { 
    color1 <- c("sienna1", "wheat1")
  } else if (color == "darkslategray") { 
    color1 <- c("darkorchid4", "plum")
  } 
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$pool.plot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  #abline(0, 1)
  
  pars <- expand.grid(col = color1, stringsAsFactors = FALSE)
  
  #windows(height = h, width = w)
  plot.new()
  title("matrices of permutation results for each richness estimator")
  par(mfrow = c(2,3))
  S <- line_data$S
  S1 <- rowMeans(S)
  N <- line_data$N
  plot(S1 ~ N, ann = F, axes = F)
  lines(S1 ~ line_data$N, color = color1) 
  axis(2, lwd = 2, ylab = "Richness")
  axis(1, xlab = "S")
  #plot(line_data, lwd = 2, col = color1)
  
  
  dev.off()
  
  return(.set.mSet(mSetObj))
}



