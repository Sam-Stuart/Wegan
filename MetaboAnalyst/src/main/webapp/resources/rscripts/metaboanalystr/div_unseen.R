
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
  
  sp1_all <- specpool(input.2)
  sp1_all_small <- specpool(input.2, smallsample = smallsample1)
  if (pool == "") {
    pool1 <- metaData[,2]
    sp1_in <- specpool(input.2, pool = pool1)
    sp1_in_small <- specpool(input.2, pool = pool1, smallsample = smallsample1)
  } else {
    pool1 <- metaData[, as.numeric(pool)]
    #pool2 <- metaData[,pool1]
    sp1_in <- specpool(input.2, pool = pool2)
    sp1_in_small <- specpool(input.2, pool = pool2, smallsample = smallsample1)
    print(pool2)
  }
  #spec <- with(metaData, sp1_in)
  #sp1_in <- specpool(input.2, pool = pool1, smallsample = smallsample1)
  est <- estimateR(input.2)
  sp2 <- specpool2vect(sp1_in, index = index1)
  pool.ac <- poolaccum(input.2, permutations = permutations1, minsize = minsize1)
  est.ac <- estaccumR(input.2, permutations = permutations1, parallel = parallel1)
  sum.sp_all <- summary(sp1_all, alpha = 0.05)
  sum.sp_in <- summary(sp1_in, alpha = 0.05)
  
  rare <- specnumber(input.2)
  
  mSetObj$analset$sp1_all <- sp1_all
  mSetObj$analset$sp1_all_small <- sp1_all_small
  mSetObj$analset$sp1_in <- sp1_in
  mSetObj$analset$sp1_in_small <- sp1_in_small
  #mSetObj$analset$spec <- spec
  mSetObj$analset$est <- est
  mSetObj$analset$sp2 <- sp2
  mSetObj$analset$pool.ac <- pool.ac
  mSetObj$analset$est.ac <- est.ac
  mSetObj$analset$sum.sp_all <- sum.sp_all
  mSetObj$analset$sum.sp_in <- sum.sp_in
  mSetObj$analset$input <- input
  mSetObj$analset$env.data <- metaData
  mSetObj$analset$rare <- rare
  mSetObj$analset$fac_data <- pool1
  
  write.csv(mSetObj$analset$sp1_all, "Incidence-based estimates_freq_all sites.csv")
  write.csv(mSetObj$analset$sp1_all_small, "Incidence-based estimates_freq_all sites_smallsample.csv")
  write.csv(mSetObj$analset$sp1_in, "Incidence-based estimates_freq_selected variable.csv")
  write.csv(mSetObj$analset$sp1_in_small, "Incidence-based estimates_freq_selected variable_smallsample.csv")
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

pool_boxplot <- function(mSetObj=NA, plot_data = "NULL", box.color = "NULL", xlab = "", ylab = "",
                      border.col = "NULL", imgName, format="png", dpi=72, width=NA) {
  #library(plyr)
  #library(vegan)
  #library(dplyr)
  
  mSetObj <- .get.mSet(mSetObj)
  
  if (plot_data == "NULL") {
    plot_data1 <- mSetObj$analset$rare/specpool2vect(mSetObj$analset$sp1_in)
  } else if (plot_data == "S") {
    plot_data1 <- mSetObj$analset$est.ac$S
  } else if (plot_data == "chao") {
    plot_data1 <- mSetObj$analset$est.ac$chao
  } else if (plot_data == "ace") {
    plot_data1 <- mSetObj$analset$est.ac$ace
  }
  print(plot_data1)
  
  treat <- factor(mSetObj$analset$fac_data)
  #print(treat)
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
  print(xlab1)
  
  if (ylab == "") {
    ylab1 = "Estimate"
  } else {
    ylab1 = ylab
  }
  print(ylab1)

  #print(plot_data1)
  #print(treat)
  print("ready for boxplot")

  #windows(height = h, width = w)
  #ylim = c(0, max(plot_data1))
  boxplot(plot_data1 ~ treat, ann = T, yaxt = "n", col = box.color1, border = border.col1, xlab = xlab1, ylab = ylab1)
  axis(2, las = 2)
  title("boxplot of extrapolated species richness and selected groups")
  print("finish boxplot")  

  dev.off()
  return(.set.mSet(mSetObj))
}


#'Produce a series of line graphs between the matrices of permutation results for each richness estimator and sample size
#'@description Produce a series of line graphs between the matrices of permutation results for each richness estimator and sample size
#'@param mSetObj Input name of the created mSet Object
#'@param color Input lines of min & max, options are "azure2" (default), "green", "red", "gray", "wheat", "orange"  
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

  #library(plyr)
  #library(vegan)
  #library(dplyr)
  #library(lattice)
  #install.packages("tidyverse")
  #library(tidyverse)
  library(ggplot2)
  #library("viridis")
  #library("stringr")
  #library(lettice)
  #library(reshape2)
  
  mSetObj <- .get.mSet(mSetObj)
  
  plot_data <- mSetObj$analset$pool.ac
  print(plot_data)  

  Smean <- as.data.frame(rowMeans(plot_data$S))
  Smean <- cbind(plot_data$N, Smean, "S")
  colnames(Smean) <- c("Size", "Richness", "Index")
  x.max <- as.numeric(max(as.numeric(Smean$Size)))
  y.max <- as.numeric(max(Smean$Richness))
  min.data <- data.frame()
  max.data <- data.frame()
  min.list <- list()  
  max.list <- list()
  c <- nrow(plot_data$S)
  print(Smean)
  print(c)  

  for (i in 1:c) {
    a <- plot_data$S[i,]
    b <- summary(a)
    min.S <- as.numeric(b[1])
    max.S <- as.numeric(b[6])
    min1 <- data.frame(min.S)
    max1 <- data.frame(max.S)
    min.list[[i]] <- min1
    max.list[[i]] <- max1
    min.data <- rbind(min.data, min.list[[i]])
    max.data <- rbind(max.data, max.list[[i]])
  }
  min.data <- cbind(min.data, max.data)
  colnames(min.data) <- c("min", "max")
  Sgg <- cbind(Smean, min.data) 
  print(Sgg)
  
  Chmean <- as.data.frame(rowMeans(plot_data$chao))
  Chmean <- cbind(plot_data$N, Chmean, "chao")
  colnames(Chmean) <- c("Size", "Richness", "Index")
  x.max.ch <- as.numeric(max(as.numeric(Chmean$Size)))
  y.max.ch <- as.numeric(max(Chmean$Richness))
  min.data.ch <- data.frame()
  max.data.ch <- data.frame()
  min.list.ch <- list()  
  max.list.ch <- list()
  ch <- nrow(plot_data$chao)
  print(Chmean)
  print(ch)

  for (i in 1:ch) {
    a.ch <- plot_data$chao[i,]
    b.ch <- summary(a.ch)
    min.ch <- as.numeric(b.ch[1])
    max.ch <- as.numeric(b.ch[6])
    min.ch1 <- data.frame(min.ch)
    max.ch1 <- data.frame(max.ch)
    min.list.ch[[i]] <- min.ch1
    max.list.ch[[i]] <- max.ch1
    min.data.ch <- rbind(min.data.ch, min.list.ch[[i]])
    max.data.ch <- rbind(max.data.ch, max.list.ch[[i]])
  }
  min.data.ch <- cbind(min.data.ch, max.data.ch)
  colnames(min.data.ch) <- c("min", "max")
  chgg <- cbind(Chmean, min.data.ch)
  print(chgg)

  
  j1mean <- as.data.frame(rowMeans(plot_data$jack1))
  j1mean <- cbind(plot_data$N, j1mean, "jack1")
  colnames(j1mean) <- c("Size", "Richness", "Index")
  x.max.j1 <- as.numeric(max(as.numeric(j1mean$Size)))
  y.max.j1 <- as.numeric(max(j1mean$Richness))
  min.data.j1 <- data.frame()
  max.data.j1 <- data.frame()
  min.list.j1 <- list()  
  max.list.j1 <- list()
  j1 <- nrow(plot_data$jack1)
  print(j1mean)
  print(j1)

  for (i in 1:j1) {
    a.j1 <- plot_data$jack1[i,]
    b.j1 <- summary(a.j1)
    min.j1 <- as.numeric(b.j1[1])
    max.j1 <- as.numeric(b.j1[6])
    min.j11 <- data.frame(min.j1)
    max.j11 <- data.frame(max.j1)
    min.list.j1[[i]] <- min.j11
    max.list.j1[[i]] <- max.j11
    min.data.j1 <- rbind(min.data.j1, min.list.j1[[i]])
    max.data.j1 <- rbind(max.data.j1, max.list.j1[[i]])
  }
  min.data.j1 <- cbind(min.data.j1, max.data.j1)
  colnames(min.data.j1) <- c("min", "max")
  j1gg <- cbind(j1mean, min.data.j1)
  print(j1gg)

  
  j2mean <- as.data.frame(rowMeans(plot_data$jack2))
  j2mean <- cbind(plot_data$N, j2mean, "jack2")
  colnames(j2mean) <- c("Size", "Richness", "Index")
  x.max.j2 <- as.numeric(max(as.numeric(j2mean$Size)))
  y.max.j2 <- as.numeric(max(j2mean$Richness))
  min.data.j2 <- data.frame()
  max.data.j2 <- data.frame()
  min.list.j2 <- list()  
  max.list.j2 <- list()
  j2 <- nrow(plot_data$jack2)
  print(j2mean)
  print(j2)

  for (i in 1:j2) {
    a.j2 <- plot_data$jack2[i,]
    b.j2 <- summary(a.j2)
    min.j2 <- as.numeric(b.j2[1])
    max.j2 <- as.numeric(b.j2[6])
    min.j21 <- data.frame(min.j2)
    max.j21 <- data.frame(max.j2)
    min.list.j2[[i]] <- min.j21
    max.list.j2[[i]] <- max.j21
    min.data.j2 <- rbind(min.data.j2, min.list.j2[[i]])
    max.data.j2 <- rbind(max.data.j2, max.list.j2[[i]])
  }
  min.data.j2 <- cbind(min.data.j2, max.data.j2)
  colnames(min.data.j2) <- c("min", "max")
  j2gg <- cbind(j2mean, min.data.j2)
  print(j2gg)

  
  bmean <- as.data.frame(rowMeans(plot_data$boot))
  bmean <- cbind(plot_data$N, bmean, "boot")
  colnames(bmean) <- c("Size", "Richness", "Index")
  x.max.b <- as.numeric(max(as.numeric(bmean$Size)))
  y.max.b <- as.numeric(max(bmean$Richness))
  min.data.b <- data.frame()
  max.data.b <- data.frame()
  min.list.b <- list()  
  max.list.b <- list()
  bt <- nrow(plot_data$boot)
  print(bmean)
  print(bt)

  for (i in 1:bt) {
    a.b <- plot_data$boot[i,]
    b.b <- summary(a.b)
    min.b <- as.numeric(b.b[1])
    max.b <- as.numeric(b.b[6])
    min.b1 <- data.frame(min.b)
    max.b1 <- data.frame(max.b)
    min.list.b[[i]] <- min.b1
    max.list.b[[i]] <- max.b1
    min.data.b <- rbind(min.data.b, min.list.b[[i]])
    max.data.b <- rbind(max.data.b, max.list.b[[i]])
  }
  min.data.b<- cbind(min.data.b, max.data.b)
  colnames(min.data.b) <- c("min", "max")
  bgg <- cbind(bmean, min.data.b)
  print(bgg)  

  plot_dataA <- rbind(Sgg, chgg, j1gg, j2gg, bgg)
  print(plot_dataA)

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
  
  if (color == "NULL") { 
    color1 <- c("azure2") #default fill palette is grayscale
  } else if (color == "green") { #manual user entry. Selection of this option causes text box to appear
    color1 <- c("darkseagreen")
  } else if (color == "red") { 
    color1 <- c("bisque")
  } else if (color == "gray") { 
    color1 <- c("darkslategray2")
  } else if (color == "wheat") { 
    color1 <- c("cornsilk2")
  } else if (color == "orange") { 
    color1 <- c("coral")
  } 
  print(color1)
 
  #pars <- expand.grid(col = color1, stringsAsFactors = FALSE)
   
  print("ready for ggplot") 
  p <- ggplot(plot_dataA, aes(x = Size, y = Richness)) +
    geom_line(aes(color = Index)) +
    facet_grid(Index ~ ., scales = "free_y") +
    geom_line(aes(x = Size, y = min, color = color1), linetype = "dotdash") +
    geom_line(aes(x = Size, y = max, color = color1), linetype = "dotdash") + 
    xlim(0,20)
  
  # Suppress the legend since color isn't actually providing any information
  #  opts(legend.position = "none")
  png(imgName)
  print(p)

  #print("after ggplot")

  #ggsave(mSetObj$imgSet$pool.plot)
  #ggsave("whatever.png")
  
  dev.off()
  
  return(.set.mSet(mSetObj))
}


