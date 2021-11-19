#'Perform species accumulation curve 
#'@description Perform species accumulation curve 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param permutations Input number of permutations with method = "random", default is 100
#'@param conditioned Boolean for estimation of standard deviation is conditional on the empirical dataset for the exact SAC, drop down options are "TRUE" or "FALSE" (default). 
#'@param gamma Input method for estimating the total extrapolated number of species in the survey area, drop down options are "jack1" (default), "jack2", "chao", "boot" or "Species". 
#'@param models Input nonlinear regression model, drop down options are "arrhenius" (default), "gleason", "gitay" or "lomolino".
#'@param object Input either a community data set or fitted specaccum model, drop down options are "exact" (default), "random", "collector", "coleman", "rarefaction" or "input_data".
#'@param interval input a character string indicating if prediction intervals or a confidence interval on the mean responses are to be calculated, drop down options are "none" (Default), "confidence", "prediction".
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export

AccumulationModel <- function(mSetObj = NA, data = "false", permutations = "", conditioned = "false", gamma = "NULL",
                            models = "NULL", object = "NULL", interval = "NULL") {
  print("start model")
  options(errors = traceback)   

  #library("ade4")
  #library("adegraphics")
  #library(plyr)
  #library(dplyr)
  library(vegan)

  mSetObj <- .get.mSet(mSetObj)
  
  #Extract input from mSetObj
  
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  
  cat("Input dataset has to be numeric data")
  input.2 <- select_if(input, is.numeric)
  print("Get input.2")
  

  if (gamma == "NULL") {
    gamma1 <- "jack1"
  } else {
    gamma1 <- gamma
  }
  print(gamma1)


  if (permutations == "" ) {
    permutations1 <- 100
  } else {
    permutations1 <- as.numeric(permutations)
  }
  print(permutations1)
  

  if (conditioned == "false") {
    conditioned1 = FALSE
  } else {
    conditioned1 = TRUE
  }
  print(conditioned1)  


  if (models == "NULL") {
    models1 = "arrhenius"
  } else if (models == "gleason") {
    models1 = "gleason"
  } else if (models == "gitay") {
    models1 = "gitay"
  } else if (models == "lomolino") {
    models1 = "lomolino"
  }
  print(models1)

  if (interval == "NULL") {
    interval1 = "none"
  } else if (interval == "confidence") (
    interval1 = "confidence"
  ) else if (interval == "prediction"){
    interval1 = "prediction"
  }
  print(interval1)
  
  sp.exact <- specaccum(input.2, method = "exact", gamma = gamma1, conditioned = conditioned1)
  sp.collector <- specaccum(input.2, method = "collector", gamma = gamma1)
  sp.random <- specaccum(input.2, method = "random", gamma = gamma1) #, permutations = permutations1
  print(sp.random)
  sp.coleman <- specaccum(input.2, method = "coleman", gamma = gamma1)
  sp.rarefaction <- specaccum(input.2, method = "rarefaction", gamma = gamma1, permutations = permutations1)
  

  if (object == "NULL") {
    object1 = sp.exact
  } else if (object == "random") {
    object1 = sp.random
  } else if (object == "coleman"){
    object1 = sp.coleman
  } else if (object == "collector") {
    object1 = sp.collector
  } else if (object == "rarefaction") {
    object1 = sp.rarefaction
  } else if (object == "input_data") {
    object1 = input.2
  }
  print(object1)

  mods <- fitspecaccum(object1, model = models1)
  if (object != "Input_data") {
     pred1 <- predict(object1, interval = interval1)
     mSetObj$analset$pred <- pred1
  }
  print(pred1)
  
  sp.exact_freq <- as.data.frame(sp.exact$freq)
  sp.exact_freq$Species <- rownames(sp.exact_freq)
  sp.exact_freq1 <- cbind(sp.exact_freq$Species, sp.exact_freq$`sp.exact$freq`)
  colnames(sp.exact_freq1) <- c("Species", "Frequence")
  sp.exact_curvedata <- as.data.frame(cbind(sp.exact$sites, sp.exact$richness, sp.exact$sd))
  colnames(sp.exact_curvedata) <- c("Sites", "Richness", "Sd")
  mSetObj$analset$sp.exact <- sp.exact
  mSetObj$analset$sp.exact_freq <- as.data.frame(sp.exact_freq1)
  mSetObj$analset$sp.exact_curvedata <- as.data.frame(sp.exact_curvedata)
  
  #sp.collector_perm <- as.list.data.frame(sp.collector$perm)
  #colnames(sp.collector_perm) <- c("perm")
  sp.collector_sites <- sp.collector$sites
  sp.collector_Richness <- sp.collector$richness
  sp.collector_curvedata <- as.data.frame(cbind(sp.collector_sites, sp.collector_Richness))
  colnames(sp.collector_curvedata) <- c("Sites", "Richness")
  mSetObj$analset$sp.collector <- sp.collector
  mSetObj$analset$sp.collector_curvedata <- sp.collector_curvedata

  sp.coleman_freq <- as.data.frame(sp.coleman$freq)
  sp.coleman_freq$Species <- rownames(sp.coleman_freq)
  sp.coleman_freq1 <- cbind(sp.coleman_freq$Species, sp.coleman_freq$`sp.coleman$freq`)
  colnames(sp.coleman_freq1) <- c("Species", "Frequence")
  sp.coleman_curvedata <- as.data.frame(cbind(sp.coleman$sites, sp.coleman$richness, sp.coleman$sd))
  colnames(sp.coleman_curvedata) <- c("Sites", "Richness", "Sd")
  mSetObj$analset$sp.coleman <- sp.coleman
  mSetObj$analset$sp.coleman_freq <- as.data.frame(sp.coleman_freq1)
  mSetObj$analset$sp.coleman_curvedata <- as.data.frame(sp.coleman_curvedata)
  
  #sp.random_perm <- as.list.data.frame(sp.random$perm)
  #colnames(sp.random_perm) <- c("perm")
  sp.random_sites <- sp.random$sites
  sp.random_Richness <- sp.random$richness
  sp.random_sd <- sp.random$sd
  sp.random_curvedata <- as.data.frame(cbind(sp.random_sites, sp.random_Richness, sp.random_sd))
  mSetObj$analset$sp.random <- sp.random
  mSetObj$analset$sp.random_curvedata <- sp.random_curvedata
  
  sp.rarefaction_freq <- as.data.frame(sp.rarefaction$freq)
  sp.rarefaction_freq$Species <- rownames(sp.rarefaction_freq)
  sp.rarefaction_freq1 <- cbind(sp.rarefaction_freq$Species, sp.rarefaction_freq$`sp.rarefaction$freq`)
  colnames(sp.rarefaction_freq1) <- c("Species", "Frequence")
  sp.rarefaction_curvedata <- as.data.frame(cbind(sp.rarefaction$sites, sp.rarefaction$individuals, sp.rarefaction$richness, sp.rarefaction$sd))
  colnames(sp.rarefaction_curvedata) <- c("Sites", "Individuals", "Richness", "Sd")
  mSetObj$analset$sp.rarefaction <- sp.rarefaction
  mSetObj$analset$sp.rarefaction_freq <- as.data.frame(sp.rarefaction_freq1)
  mSetObj$analset$sp.rarefaction_curvedata <- as.data.frame(sp.rarefaction_curvedata)
  
  mods_freq <- as.data.frame(mods$freq)
  mods_freq$Species <- rownames(mods_freq)
  mods_freq1 <- cbind(mods_freq$Species, mods_freq$`mods$freq`)
  colnames(mods_freq1) <- c("Species", "Frequence")
  mods_curvedata <- as.data.frame(cbind(mods$sites, mods$richness, mods$sd))
  colnames(mods_curvedata) <- c("Sites", "Richness", "Sd")
  mods_fitted <- mods$fitted
  mods_residuals <- as.data.frame(mods$residuals)
  mSetObj$analset$mods <- mods
  mSetObj$analset$mods_freq <- as.data.frame(mods_freq1)
  mSetObj$analset$mods_curvedata <- as.data.frame(mods_curvedata)
  mSetObj$analset$mods_SSmodel <- mods$SSmodel
  mSetObj$analset$mods_residuals <- mods_residuals
  mSetObj$analset$mods_fitted <- mods_fitted
  #colnames(mSetObj$analset$mods_fitted) <- c("No", "fitted")
  
  mSetObj$analset$input <- input.2
  
  write.csv(mSetObj$analset$sp.exact_freq, "Exact Species Accumulation Curve_Frequency.csv")
  write.csv(mSetObj$analset$sp.exact_curvedata, "Exact Species Accumulation Curve_Data.csv")
  #write.csv(mSetObj$analset$sp.collector_freq, "Collector Species Accumulation Curve_Frequency.csv")
  write.csv(mSetObj$analset$sp.collector_curvedata, "Collector Species Accumulation Curve_Data.csv")
  write.csv(mSetObj$analset$sp.random_freq, "Random Species Accumulation Curve_Frequency.csv")
  write.csv(mSetObj$analset$sp.random_curvedata, "Random Species Accumulation Curve_Data.csv")
  #write.csv(mSetObj$analset$sp.coleman_freq, "Coleman Species Accumulation Curve_Frequency.csv")
  write.csv(mSetObj$analset$sp.coleman_curvedata, "Coleman Species Accumulation Curve_Data.csv")
  write.csv(mSetObj$analset$sp.rarefaction_freq, "Rarefaction Species Accumulation Curve_Frequency.csv")
  write.csv(mSetObj$analset$sp.rarefaction_curvedata, "Rarefaction Species Accumulation Curve_Data.csv")
  #write.csv(mSetObj$analset$mods_freq, "Nonlinear Selfstarting Species Accumulation Curve_Frequency.csv")
  write.csv(mSetObj$analset$mods_curvedata, "Nonlinear Selfstarting Species Accumulation Curve_Data.csv")
  write.csv(mSetObj$analset$mods_residuals, "Nonlinear Selfstarting Species Accumulation Curve_Residuals.csv")
  write.csv(mSetObj$analset$mods_fitted, "Nonlinear Selfstarting Species Accumulation Curve_Fitted.csv")
  
  return(.set.mSet(mSetObj)) 
}


#'Produce species accumulation curves for each row of the input data. 
#'@description Produce species accumulation curves with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param type Input specaccum species accumulation curves, drop down options are "exact" (default), "random", "collector", "coleman" or "rarefaction". 
#'@param ci.type Input type of confidence intervals, drop down options are "polygon" (default), "bar" or "line".
#'@param color Input color name for fitted specaccum model, drop down options are "black" (default), "slateblue", "steelblue", "royalblue", "navyblue", "darkslategray". 
#'@param ci.color Input color name for filling "polygon" (default, see 'type'), drop down options are "gray88" (default), "honeydew", "lightcyan", "ivory", "azure"  
#'@param box.color Input box color for random species accumulation curve boxplot, drop down options are "skyblue" (default), "green", "turquoise", "lightsteelblue", "peach", "wheat". 
#'@param line.color Input line color for nonlinear self-starting species accumulation model, drop down options are "red" (default), "coral", "brown", "salmon", "tomato", "sienna".
#'@param pred.color Input line color for specaccum model prediction corresponding to newdata, drop down options are "purple" (default), "violetred", "orchid", "maroon", "hotpink", "deeppink". 
#'@param pch Input point shapes, textbox default is "+"
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
AccumCurve <- function(mSetObj=NA, type = "NULL", color = "NULL", ci.color="NULL", ci.type="NULL", box.color = "NULL", 
                           line.color = "NULL", pred.color = "NULL", pch = "", imgName, format="png", dpi=72, width=NA) {
  
  #library(vegan)
  #library(plyr)  
  #library(dplyr)
  
  mSetObj <- .get.mSet(mSetObj)
  
  if (type == "NULL") {
    plot_data <- mSetObj$analset$sp.exact
    type1 = "exact method"
  } else if (type == "collector") {
    plot_data <- mSetObj$analset$sp.collector
    type1 = "collector method"
  } else if (type == "random") {
    plot_data <- mSetObj$analset$sp.random
    type1 = "random method"
  } else if (type == "coleman") {
    plot_data <- mSetObj$analset$sp.coleman
    type1 = "coleman method"
  } else if (type == "rarefaction") {
    plot_data <- mSetObj$analset$sp.rarefaction
    type1 = "rarefaction method"
  }
  
  box_data <- mSetObj$analset$sp.random
  line_data <- mSetObj$analset$mods
  pred_data <- mSetObj$analset$pred
  SSmodel <- mSetObj$analset$mods_SSmodel

  n <- as.numeric(max(plot_data$sites)) 

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  
  if(ci.type == "NULL") {
    ci.type1 = "polygon"
  } else if (ci.type == "bar") {
    ci.type1 = "bar"
  } else if (ci.type == "line") {
    ci.type1 = "line"
  }
  

  if(box.color == "NULL") { 
    box.color1 = "skyblue" #default fill palette is grayscale
  } else if (box.color == "green") { #manual user entry. Selection of this option causes text box to appear
    box.color1 = "palegreen"
  } else if (box.color == "turquoise") {
    box.color1 = "turquoise"
  } else if (box.color == "lightsteelblue1") {
    box.color1 = "lightsteelblue1"
  } else if (box.color == "peach") {
    box.color1 = "peachpuff"
  } else if (box.color == "wheat") {
    box.color1 = "wheat1"
  } 
  
  
  if (ci.color == "NULL") { 
    ci.color1 = "gray88" #default fill palette is grayscale
  } else if (ci.color == "honeydew") { #manual user entry. Selection of this option causes text box to appear
    ci.color1 <- "honeydew3"
  } else if (ci.color == "lightcyan") { 
    ci.color1 <- "lightcyan3"
  } else if (ci.color == "thistle") { 
    ci.color1 <- "thistle"
  } else if (ci.color == "ivory") { 
    ci.color1 <- "ivory3"
  } else if (ci.color == "azure") { 
    ci.color1 <- "azure3"
  } 
  
  
  if (color == "NULL") { 
    color1 = "black" #default fill palette is grayscale
  } else if (color == "slateblue") { #manual user entry. Selection of this option causes text box to appear
    color1 <- "slateblue4"
  } else if (color == "steelblue") { 
    color1 <- "steelblue4"
  } else if (color == "royalblue") { 
    color1 <- "royalblue4"
  } else if (color == "navyblue") { 
    color1 <- "navyblue"
  } else if (color == "darkslategray") { 
    color1 <- "darkslategray"
  } 
  
  
  if (line.color == "NULL") { 
    line.color1 = "red" #default fill palette is grayscale
  } else if (line.color == "coral") { #manual user entry. Selection of this option causes text box to appear
    line.color1 <- "coral3"
  } else if (line.color == "brown") { 
    line.color1 <- "brown4"
  } else if (line.color == "salmon") { 
    line.color1 <- "salmon1"
  } else if (line.color == "tomato") { 
    line.color1 <- "tomato2"
  } else if (line.color == "sienna") { 
    line.color1 <- "sienna1"
  }
  
  
  if (pred.color == "NULL") { 
    pred.color1 = "purple3" 
  } else if (pred.color == "violetred") { 
    pred.color1 <- "violetred1"
  } else if (pred.color == "orchid") { 
    pred.color1 <- "orchid3"
  } else if (pred.color == "maroon") { 
    pred.color1 <- "maroon3"
  } else if (pred.color == "hotpink") { 
    pred.color1 <- "hotpink1"
  } else if (pred.color == "deeppink") { 
    pred.color1 <- "deeppink1"
  } 
  
  
  if (pch == "") {
    pch1 = 19
  } else {
    pch1 = as.numeric(pch)
  }
  
  
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$pool.plot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  #abline(0, 1)
  
  #pars <- expand.grid(col = color1, stringsAsFactors = FALSE)
  
  #windows(height = h, width = w)
  
  plot(plot_data, ci.type = ci.type1, col = color1, lwd = 3, ci.lty = 0, 
       ci.col = ci.color1, lty = 1, xaxt = "n", yaxt = "n", ylab = "Number of species", xlab = "Sites", cex.axis = 1, cex.lab = 1.2)
  legend("bottomright", legend = c(type1, "nonlinear selfstarting", "prediction"), col = c(color1, line.color1, pred.color1),
         lty = 1:3, cex = 1.2, lwd = 2, box.lty = 1) 
  axis(1, labels = T, at = 0:n)
  axis(2, las = 2)
  boxplot(box_data, col = box.color1, add = TRUE, pch = pch1)
  lines(line_data, col = line.color1, lwd = 3, lty = 2)
  lines(pred_data, col = pred.color1, lwd = 3, lty = 3)
   
  title(main = "Species Accumulation Model")

  dev.off() 
  
  return(.set.mSet(mSetObj))
}


