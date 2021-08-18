#'Perform species abundance distribution
#'@description Perform rarefaction technique 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param community Input for abundance model, textbox with default as the whole dataset, or a row number (each row should represent a plot/site/sample unit)
#'@param tiesplit Boolean for split frequencies, drop down options are "TRUE" or "FALSE" (default)
#'@param truncate Boolean for truncation point for log-Normal model, in log2 units, default value is -1
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export

AbundanceModel <- function(mSetObj = NA, data = "false", community = "", tiesplit = "false", truncate = ""){
  print("start model")
  options(errors = traceback)                          
  #library("ade4")
  #library("adegraphics")
  library("plyr")
  library("dplyr")
  library("vegan")


  mSetObj <- .get.mSet(mSetObj)
  
  print("setup normal")
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }

  input.2 <- select_if(input, is.numeric)
  print(input.2)  

  if (tiesplit == "false") {
    tiesplit1 = FALSE
  } else {
    tiesplit1 = TRUE
  }
  print(tiesplit1)  

  if(community == "") {
    input.c <- input.2
  } else {
    community1 <- as.numeric(community)
    input.c <- input.2[community1, ]
  }
  print(input.c)

  if(truncate == "")  {
    truncate1 = "-1"
  } else {
    truncate1 = truncate
  }
  truncate1 <- as.numeric(truncate1)
  print(truncate1)  

  
  
  output.fisher <- fisherfit(input.c)
  output.fit <- prestonfit(input.c, tiesplit = tiesplit1)
  output.distr <- prestondistr(input.c, truncate = truncate1)
  output.ve.fit <- veiledspec(output.fit)
  output.ve.distr <- veiledspec(output.distr)
  print(output.fisher)
  print(output.fisher$fisher)
    

  mSetObj$analset$result$fisher$name <- "Species Abundance Model"
  mSetObj$analset$result$fisher$type <- "Fisher's logseries"
  mSetObj$analset$result$fisher$estimate <- output.fisher$estimate
  mSetObj$analset$result$fisher$No.of.species <- sum(output.fisher$fisher)
  mSetObj$analset$result$fisher$freq <- output.fisher$fisher
  mSetObj$analset$result$fisher$freq <- as.list.data.frame(output.fisher$fisher)
  mSetObj$analset$result$fisher$freq <- as.data.frame(mSetObj$analset$result$fisher$freq)
  colnames(mSetObj$analset$result$fisher$freq) <- c("freq")
  freq.row <- rownames(mSetObj$analset$result$fisher$freq)
  mSetObj$analset$result$fisher$freq <- cbind(freq.row, mSetObj$analset$result$fisher$freq)
  mSetObj$analset$result$fisher$freq.row <- as.numeric(mSetObj$analset$result$fisher$freq.row)
  mSetObj$analset$result$fisher$n <- nrow(mSetObj$analset$result$fisher$freq)
  mSetObj$analset$result$fisher$output <- output.fisher
  mSetObj$analset$result$fisher$com.result <- rbind(mSetObj$analset$result$fisher$name, 
                                                    mSetObj$analset$result$fisher$type,
                                                    mSetObj$analset$result$fisher$estimate,
                                                    mSetObj$analset$result$fisher$No.of.species)
  rownames(mSetObj$analset$result$fisher$com.result) <- c("Analysis", "Model Type", "Fisher_alpha Estimate", "No. of Sp.")
  

  mSetObj$analset$result$lognormal_fit$name <- "Species Abundance Model"
  mSetObj$analset$result$lognormal_fit$type <- "Poisson fit to octaves"
  mSetObj$analset$result$lognormal_fit$fitted <- output.fit$fitted
  mSetObj$analset$result$lognormal_fit$freq <- output.fit$freq
  mSetObj$analset$result$lognormal_fit$coefficients <- output.fit$coefficients
  mSetObj$analset$result$lognormal_fit$output <- output.fit
  mSetObj$analset$result$lognormal_fit$output.r <- output.ve.fit
  mSetObj$analset$result$lognormal_fit$frequencies <- cbind(mSetObj$analset$result$lognormal_fit$freq,
                                                            mSetObj$analset$result$lognormal_fit$fitted)
  colnames(mSetObj$analset$result$lognormal_fit$frequencies) <- c("Observed", "Fitted")
  mSetObj$analset$result$lognormal_fit$coeffi <- as.data.frame(mSetObj$analset$result$lognormal_fit$coefficients)
  colnames(mSetObj$analset$result$lognormal_fit$coeffi) <- c("Coefficients")
  mSetObj$analset$result$lognormal_fit$output.r <- as.data.frame(mSetObj$analset$result$lognormal_fit$output.r)
  colnames(mSetObj$analset$result$lognormal_fit$output.r) <- c("Richness")
  
  
  mSetObj$analset$result$log_likelihood$name <- "Species Abundance Model"
  mSetObj$analset$result$log_likelihood$type <- "Maximization of log-likelihood"
  mSetObj$analset$result$log_likelihood$fitted <- output.distr$fitted
  mSetObj$analset$result$log_likelihood$freq <- output.distr$freq
  mSetObj$analset$result$log_likelihood$coefficients <- output.distr$coefficients
  mSetObj$analset$result$log_likelihood$output <- output.distr
  mSetObj$analset$result$log_likelihood$output.r <- output.ve.distr
  mSetObj$analset$result$log_likelihood$frequencies <- cbind(mSetObj$analset$result$log_likelihood$freq,
                                                            mSetObj$analset$result$log_likelihood$fitted)
  colnames(mSetObj$analset$result$log_likelihood$frequencies) <- c("Observed", "Fitted")
  mSetObj$analset$result$log_likelihood$coeffi <- as.data.frame(mSetObj$analset$result$log_likelihood$coefficients)
  colnames(mSetObj$analset$result$log_likelihood$coeffi) <- c("Coefficients")
  mSetObj$analset$result$log_likelihood$output.r <- as.data.frame(mSetObj$analset$result$log_likelihood$output.r)
  colnames(mSetObj$analset$result$log_likelihood$output.r) <- c("Richness")
  
  
  write.csv(mSetObj$analset$result$fisher$freq, "Fisher frequency table.csv")
  write.csv(mSetObj$analset$result$fisher$com.result, "Fisher log series model output.csv")
  write.csv(mSetObj$analset$result$lognormal_fit$coeffi, "Poisson coefficients.csv")
  write.csv(mSetObj$analset$result$lognormal_fit$frequencies, "Poisson frequencies by Octave.csv")
  write.csv(mSetObj$analset$result$lognormal_fit$output.r, "Poisson_Total extrapolated richness.csv")
  write.csv(mSetObj$analset$result$log_likelihood$coeffi, "Max_likelihood coefficients.csv")
  write.csv(mSetObj$analset$result$log_likelihood$frequencies, "Max_likelihood frequencies by Octave.csv")
  write.csv(mSetObj$analset$result$log_likelihood$output.r, "Max_likelihood_Total extrapolated richness.csv")
    
  return(.set.mSet(mSetObj)) 
}


#'Perform Fisher log series distribution histogram plot
#'@description Fisher log series distribution histogram plot 
#'@param mSetObj Input name of the created mSet Object
#'@param bar.color Input bar color of the histogram graph. drop down options are skyblue (default), gray, turquoise, slateblue, seagreen, wheat
#'@param line.color.addFit Input for, drop down options are red (default), coral, brwon, salmon, tomato, sienna
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export
AbundanceFisherPlot <- function(mSetObj = NA, bar.color = "NULL", line.color.addFit = "NULL", 
                           imgName, format="png", dpi=72, width=NA) {
  
  library(vegan)
  
  mSetObj <- .get.mSet(mSetObj)
  
  plot_data.fisher <- mSetObj$analset$result$fisher$output
  n <- as.numeric(mSetObj$analset$result$fisher$n)
  print(n)
  
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
  mSetObj$imgSet$Abundance.plot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  if(bar.color == "NULL") { 
    bar.color1 = "skyblue" #default fill palette is grayscale
  } else if (bar.color == "gray") { #manual user entry. Selection of this option causes text box to appear
    bar.color1 = "gray"
  } else if (bar.color == "turquoise") {
    bar.color1 = "turquoise4"
  } else if (bar.color == "slateblue") {
    bar.color1 = "slateblue4"
  } else if (bar.color == "seagreen") {
    bar.color1 = "darkseagreen1"
  } else if (bar.color == "wheat") {
    bar.color1 = "wheat1"
  } 
  
  if (line.color.addFit == "NULL") { 
    line.color.addFit1 = "red" 
  } else if (line.color.addFit == "coral") { 
    line.color.addFit1 <- "coral3"
  } else if (line.color.addFit == "brown") { 
    line.color.addFit1 <- "brown4"
  } else if (line.color.addFit == "salmon") { 
    line.color.addFit1 <- "salmon1"
  } else if (line.color.addFit == "tomato") { 
    line.color.addFit1 <- "tomato2"
  } else if (line.color.addFit == "sienna") { 
    line.color.addFit1 <- "sienna1"
  }
  
  plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
       line.col = line.color.addFit1, bar.col = bar.color1, xlab = "Frequency", ylab = "Number of species")
  axis(1, labels = T, at = 1:n)
  axis(2, las = 2)
  title("Fisher's Log-series Species Abundance Distribution ")
  dev.off()
  
  return(.set.mSet(mSetObj))
}



#'Perform Preston lognormal distribution histogram plot
#'@description lognormal distribution histogram plot 
#'@param mSetObj Input name of the created mSet Object
#'@param bar.color Input bar color of the histogram graph. drop down options are skyblue (default), gray, turquoise, slateblue, seagreen, wheat
#'@param line.color.addPoi Input for Preston’s lognormal model trend line (traditional binning), drop down options are green (default), olive, springgreen, yellowgreen, gold, orange
#'@param line.color.addPoi Input for Preston’s lognormal model trend line (without binning), drop down options are purple (default), violetred, orchid, maroon, hotpink, deeppink
#'@param imgName Input the image name
#'@param format Select the image format, "png" or "pdf", default is "png" 
#'@param dpi Input the dpi. If the image format is "pdf", users need not define the dpi. For "png" images, 
#'the default dpi is 72. It is suggested that for high-resolution images, select a dpi of 300.  
#'@param width Input the width, there are 2 default widths. The first, width=NULL, is 10.5.
#'The second default is width=0, where the width is 7.2. Otherwise users can input their own width
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export
AbundancePrestPlot <- function(mSetObj=NA, bar.color="NULL", line.color.addPoi = "NULL", line.color.addMax = "NULL", imgName, format="png", dpi=72, width=NA) {
  
  library(vegan)
  
  mSetObj <- .get.mSet(mSetObj)
  
  plot_data.fit <- mSetObj$analset$result$lognormal_fit$output
  plot_data.distr <- mSetObj$analset$result$log_likelihood$output
  
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
  mSetObj$imgSet$Abundance.plot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  #abline(0, 1)
  
  n = 1
  if(bar.color == "NULL") { 
    bar.color1 = "skyblue" #default fill palette is grayscale
  } else if (bar.color == "gray") { #manual user entry. Selection of this option causes text box to appear
    bar.color1 = "gray"
  } else if (bar.color == "turquoise") {
    bar.color1 = "turquoise4"
  } else if (bar.color == "slateblue") {
    bar.color1 = "slateblue4"
  } else if (bar.color == "seagreen") {
    bar.color1 = "darkseagreen1"
  } else if (bar.color == "wheat") {
    bar.color1 = rainbow(n)
  } 
  
  
  if (line.color.addPoi == "NULL") { 
    line.color.addPoi1 = "green" #default fill palette is grayscale
  } else if (line.color.addPoi == "olive") { #manual user entry. Selection of this option causes text box to appear
    line.color.addPoi1 <- "olivedrab3"
  } else if (line.color.addPoi == "springgreen") { 
    line.color.addPoi1 <- "springgreen1"
  } else if (line.color.addPoi == "yellowgreen") { 
    line.color.addPoi1 <- "yellowgreen"
  } else if (line.color.addPoi == "gold") { 
    line.color.addPoi1 <- "goldenrod1"
  } else if (line.color.addPoi == "orange") { 
    line.color.addPoi1 <- "orange"
  } 
  
  if (line.color.addMax == "NULL") { 
    line.color.addMax1 = "purple3" #default fill palette is grayscale
  } else if (line.color.addMax == "violetred") { #manual user entry. Selection of this option causes text box to appear
    line.color.addMax1 <- "violetred1"
  } else if (line.color.addMax == "orchid") { 
    line.color.addMax1 <- "orchid3"
  } else if (line.color.addMax == "maroon") { 
    line.color.addMax1 <- "maroon3"
  } else if (line.color.addMax == "hotpink") { 
    line.color.addMax1 <- "hotpink1"
  } else if (line.color.addMax == "deeppink") { 
    line.color.addMax1 <- "deeppink1"
  } 

  plot(plot_data.distr, ann = T, pch = 17, lty = 1, lwd = 2, cex = 2,
       line.col = line.color.addPoi1, bar.col = bar.color1,  ylab = "Species", xlab = "Frequencies")
  axis(1, labels = F, xlab = "Frequencies")
  axis(2, labels = F, ylab = "Number of species")    
  lines(plot_data.fit, line.col = line.color.addMax1, lty = 2)
  legend("topright", legend = c("Traditional binning", "Without binning"), col = c(line.color.addPoi1, line.color.addMax1), 
         lty = c(1,2), cex = 1, box.lty = 0)
  title("Preston's Lognormal Species Abundance Distribution ")


  dev.off()
  
  return(.set.mSet(mSetObj))
}


