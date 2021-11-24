#'Perform Fisher species abundance distribution
#'@description Perform rarefaction technique 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param communityFisher Input for abundance model, textbox with default as the whole dataset, or a row number (each row should represent a plot/site/sample unit)
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export

AbundanceFisherModel <- function(mSetObj = NA, data = "false", communityFisher = ""){
  print("start model")
  options(errors = traceback)                          
  #library("ade4")
  #library("adegraphics")
  #library("plyr")
  #library("dplyr")
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

  if(communityFisher == "") {
    input.c <- input.2
  } else {
    community1 <- as.numeric(communityFisher)
    input.c <- input.2[community1, ]
  }
  #print(input.c)


  #mod.radfit_all <- radfit(input.2)

  #print(community1)  

  output.fisher <- fisherfit(input.c)

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
   
  write.csv(mSetObj$analset$result$fisher$freq, "Fisher frequency table.csv")
  write.csv(mSetObj$analset$result$fisher$com.result, "Fisher log series model output.csv")
  
  return(.set.mSet(mSetObj)) 
}



#'Perform Preston species abundance distribution
#'@description Perform rarefaction technique 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param communityPres Input for abundance model, textbox with default as the whole dataset, or a row number (each row should represent a plot/site/sample unit)
#'@param tiesplit Boolean for split frequencies, drop down options are "TRUE" or "FALSE" (default)
#'@param truncate Boolean for truncation point for log-Normal model, in log2 units, default value is -1
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export
AbundancePrestonModel <- function(mSetObj = NA, data = "false", communityPres = "", tiesplit = "false", truncate = ""){
  print("start model")
  options(errors = traceback)                          
  #library("ade4")
  #library("adegraphics")
  #library("plyr")
  #library("dplyr")
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

  if(communityPres == "") {
    input.c <- input.2
  } else {
    community1 <- as.numeric(communityPres)
    input.c <- input.2[community1, ]
  }
  #print(input.c)

  if(truncate == "")  {
    truncate1 = "-1"
  } else {
    truncate1 = truncate
  }
  truncate1 <- as.numeric(truncate1)
  print(truncate1)  

  #mod.radfit_all <- radfit(input.2)
  
  
  output.fit <- prestonfit(input.c, tiesplit = tiesplit1)
  output.distr <- prestondistr(input.c, truncate = truncate1)
  output.ve.fit <- veiledspec(output.fit)
  output.ve.distr <- veiledspec(output.distr)
  #print(AIC(output.fisher))
  #print(AIC(output.fit))
  #print(AIC(output.distr))
  #print(BIC(output.fisher))
  #print(BIC(output.fit))
  #print(BIC(output.distr))

  #mSetObj$analset$result$mod.radfit <- mod.radfit
  #mSetObj$analset$result$mod.radfit_all <- mod.radfit_all
  

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
  

  write.csv(mSetObj$analset$result$lognormal_fit$coeffi, "Poisson coefficients.csv")
  write.csv(mSetObj$analset$result$lognormal_fit$frequencies, "Poisson frequencies by Octave.csv")
  write.csv(mSetObj$analset$result$lognormal_fit$output.r, "Poisson_Total extrapolated richness.csv")
  write.csv(mSetObj$analset$result$log_likelihood$coeffi, "Max_likelihood coefficients.csv")
  write.csv(mSetObj$analset$result$log_likelihood$frequencies, "Max_likelihood frequencies by Octave.csv")
  write.csv(mSetObj$analset$result$log_likelihood$output.r, "Max_likelihood_Total extrapolated richness.csv")
   
  return(.set.mSet(mSetObj)) 
}



#'Perform rank species abundance distribution
#'@description Perform rarefaction technique 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param communityRank Input for abundance model, textbox with default as the whole dataset, or a row number (each row should represent a plot/site/sample unit)
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export
AbundanceRankModel <- function(mSetObj = NA, data = "false", communityRank = ""){
  print("start model")
  options(errors = traceback)                          
  #library("ade4")
  #library("adegraphics")
  #library("plyr")
  #library("dplyr")
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

  mod.radfit_all <- radfit(input.2)
  
  null.data <- data.frame()
  prep.data <- data.frame()
  log.data <- data.frame()
  zipf.data <- data.frame()
  man.data <- data.frame()
  null.list <- list()  
  prep.list <- list()
  log.list <- list()
  zipf.list <- list()
  man.list <- list()
  null.aic.data <- data.frame()
  prep.aic.data <- data.frame()
  log.aic.data <- data.frame()
  zipf.aic.data <- data.frame()
  man.aic.data <- data.frame()
  null.aic.list <- list()  
  prep.aic.list <- list()
  log.aic.list <- list()
  zipf.aic.list <- list()
  man.aic.list <- list()
  prep.alpha.data <- data.frame()
  prep.alpha.list <- list()
  log.mu.data <- data.frame()
  log.signal.data <- data.frame()
  log.mu.list <- list()
  log.signal.list <- list()
  zipf.gamma.data <- data.frame()
  zipf.gamma.list <- list()
  man.beta.data <- data.frame()
  man.beta.list <- list()
  man.gamma.data <- data.frame()
  man.gamma.list <- list()
  
  c <- as.numeric(nrow(input.2))
  print(c)  

  for (i in 1:c) {
    null <- rad.null(input.2[i,])$deviance
    prep <- rad.preempt(input.2[i,])$deviance
    log <- rad.lognormal(input.2[i,])$deviance
    zipf <- rad.zipf(input.2[i,])$deviance
    man <- rad.zipfbrot(input.2[i,])$deviance
    null.aic <- rad.null(input.2[i,])$aic
    prep.aic <- rad.preempt(input.2[i,])$aic
    log.aic <- rad.lognormal(input.2[i,])$aic
    zipf.aic <- rad.zipf(input.2[i,])$aic
    man.aic <- rad.zipfbrot(input.2[i,])$aic
    prep.alpha <- rad.preempt(input.2[i,])$coefficients
    log.mu <- rad.lognormal(input.2[i,])$coefficients[1]
    log.signal <- rad.lognormal(input.2[i,])$coefficients[2]
    zipf.gamma <- rad.zipf(input.2[i,])$coefficients[2]
    man.beta <- rad.zipfbrot(input.2[i,])$coefficients[2]
    man.gamma <- rad.zipfbrot(input.2[i,])$coefficients[3]
    null.de <- data.frame(null)
    prep.de <- data.frame(prep)
    log.de <- data.frame(log)
    zipf.de <- data.frame(zipf)
    man.de <- data.frame(man)
    null.aicA <- data.frame(null.aic)
    prep.aicA <- data.frame(prep.aic)
    log.aicA <- data.frame(log.aic)
    zipf.aicA <- data.frame(zipf.aic)
    man.aicA <- data.frame(man.aic)
    prep.alphaA <- data.frame(prep.alpha)
    log.muA <- data.frame(log.mu)
    log.signalA <- data.frame(log.signal)
    zipf.gammaA <- data.frame(zipf.gamma)
    man.betaA <- data.frame(man.beta)
    man.gammaA <- data.frame(man.gamma)
    null.list[[i]] <- null.de
    prep.list[[i]] <- prep.de
    log.list[[i]] <- log.de
    zipf.list[[i]] <- zipf.de
    man.list[[i]] <- man.de
    null.aic.list[[i]] <- null.aicA
    prep.aic.list[[i]] <- prep.aicA
    log.aic.list[[i]] <- log.aicA
    zipf.aic.list[[i]] <- zipf.aicA
    man.aic.list[[i]] <- man.aicA
    prep.alpha.list[[i]] <- prep.alphaA
    log.mu.list[[i]] <- log.muA
    log.signal.list[[i]] <- log.signalA
    zipf.gamma.list[[i]] <- zipf.gammaA
    man.beta.list[[i]] <- man.betaA
    man.gamma.list[[i]] <- man.gammaA
    null.data <- rbind(null.data, null.list[[i]])
    prep.data <- rbind(prep.data, prep.list[[i]])
    log.data <- rbind(log.data, log.list[[i]])
    zipf.data <- rbind(zipf.data, zipf.list[[i]])
    man.data <- rbind(man.data, man.list[[i]])
    null.aic.data <- rbind(null.aic.data, null.aic.list[[i]])
    prep.aic.data <- rbind(prep.aic.data, prep.aic.list[[i]])
    log.aic.data <- rbind(log.aic.data, log.aic.list[[i]])
    zipf.aic.data <- rbind(zipf.aic.data, zipf.aic.list[[i]])
    man.aic.data <- rbind(man.aic.data, man.aic.list[[i]])
    prep.alpha.data <- rbind(prep.alpha.data, prep.alpha.list[[i]])
    log.mu.data <- rbind(log.mu.data, log.mu.list[[i]])
    log.signal.data <- rbind(log.signal.data, log.signal.list[[i]])
    zipf.gamma.data <- rbind(zipf.gamma.data, zipf.gamma.list[[i]])
    man.beta.data <- rbind(man.beta.data, man.beta.list[[i]])
    man.gamma.data <- rbind(man.gamma.data, man.gamma.list[[i]])
  }
  
  null.data.set <- cbind(null.data, null.aic.data)
  colnames(null.data.set) <- c("Deviance", "AIC")
  prep.data.set <- cbind(prep.data, prep.aic.data, prep.alpha.data)
  rownames(prep.data.set) <- c(1:c)
  colnames(prep.data.set) <- c("Deviance", "AIC", "Alpha")
  log.data.set <- cbind(log.data, log.aic.data, log.mu.data, log.signal.data)
  rownames(log.data.set) <- c(1:c)
  colnames(log.data.set) <- c("Deviance", "AIC", "log.mu", "log.signal")
  zipf.data.set <- cbind(zipf.data, zipf.aic.data, zipf.gamma.data)
  rownames(zipf.data.set) <- c(1:c)
  colnames(zipf.data.set) <- c("Deviance", "AIC", "Gamma")
  man.data.set <- cbind(man.data, man.aic.data, man.beta.data, man.gamma.data)
  rownames(man.data.set) <- c(1:c)
  colnames(man.data.set) <- c("Deviance", "AIC", "Beta", "Gamma")
  print(man.data.set)  

  if (communityRank == "") {
    mod.radfit <- radfit(input.2[1,])
    mod.null <- rad.null(input.2[1,])
    mod.pree <- rad.preempt(input.2[1,])
    mod.log <- rad.lognormal(input.2[1,])
    mod.zipf <- rad.zipf(input.2[1,])
    mod.man <- rad.zipfbrot(input.2[1,])
  } else {
    community1 <- as.numeric(communityRank)
    input.c <- input.2[community1, ]
    mod.radfit <- radfit(input.c)
    mod.null <- rad.null(input.c)
    mod.pree <- rad.preempt(input.c)
    mod.log <- rad.lognormal(input.c)
    mod.zipf <- rad.zipf(input.c)
    mod.man <- rad.zipfbrot(input.c)
  }
  #print(community)  

  mSetObj$analset$result$mod.radfit <- mod.radfit
  mSetObj$analset$result$mod.radfit_all <- mod.radfit_all
  mSetObj$analset$result$mod.null <- mod.null
  mSetObj$analset$result$mod.pree <- mod.pree
  mSetObj$analset$result$mod.log <- mod.log
  mSetObj$analset$result$mod.zipf <- mod.zipf
  mSetObj$analset$result$mod.man <- mod.man
  mSetObj$analset$result$null.data.set <- null.data.set
  mSetObj$analset$result$prep.data.set <- prep.data.set
  mSetObj$analset$result$log.data.set <- log.data.set
  mSetObj$analset$result$zipf.data.set <- zipf.data.set
  mSetObj$analset$result$man.data.set <- man.data.set
  

  write.csv(mSetObj$analset$result$null.data.set, "Perimeters of brokenstick models of species abundance.csv")
  write.csv(mSetObj$analset$result$prep.data.set, "Perimeters of preemption models of species abundance.csv")
  write.csv(mSetObj$analset$result$log.data.set, "Perimeters of log-Normal models of species abundance.csv")
  write.csv(mSetObj$analset$result$zipf.data.set, "Perimeters of Zipf models of species abundance.csv")
  write.csv(mSetObj$analset$result$man.data.set, "Perimeters of Zipf-Mandelbrot models of species abundance.csv")
  
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
  
  #library(vegan)
  
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

  m <- max(plot_data.fisher$fisher)
  if (m/10000 >= 5) {  
     per <- 10000
  } else if (m/1000 >= 5) {
     per <- 1000 
  } else if (m/100 >= 5) {
     per <- 100 
  } else if (m/100 < 5 & m/50 >=5) {
     per <- 50 
  } else {
     per <- 10
  } 
  
  
  plot(plot_data.fisher, lty = 1, lwd = 2, cex = 2, yaxt = "n", xaxt = "n",
       line.col = line.color.addFit1, bar.col = bar.color1, xlab = "Abundance Octave", ylab = "Number of Species",
       cex.lab = 1.2, cex.axis = 1)
  axis(1, labels = T, at = 0:n, cex.lab = 1.2, cex.axis = 1)
  axis(2, las = 2, at = seq(0, m, per), cex.lab = 1.2, cex.axis = 1)
  title(main = "Fisher's Logseries Species Abundance Distribution", cex.lab = 1.5)
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

  #library(plyr)
  #library(dplyr)
  #library(vegan)
  
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
  
  #n = 1
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
  print(bar.color1)
  
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
  print(line.color.addPoi1)
  
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
  print(line.color.addMax1)

  a <- as.data.frame(mSetObj$analset$result$log_likelihood$frequencies)
  print(a)
  
  
  #m1 <- round(max(a$Observed) + 10, digits = -1)
  
  n <- length(a$Observed)
  print(n)
  b <- a%>%
    select(Fitted)%>%
    mutate(row = seq(0.5, n, by = 1))
  print(b)
  m <- round(max(a$Fitted) + 10, digits = -1)
  
  c <- as.data.frame(mSetObj$analset$result$lognormal_fit$frequencies)
  d <- c%>%
    select(Fitted)%>%
    mutate(row <- seq(0.5, n, by = 1))
  colnames(d) <- c("Fitted", "row") 
  print(d)
  eec <- 0.5 + d$row
  eem <- max(d$row)

  m1 <- max(d$Fitted)

  if (m >= m1) {
     y_m <- m
  } else {
     y_m <- m1
  }
  y_m <- as.numeric(y_m)
  print(y_m)

  if (y_m/10000 >= 5) {  
     per <- 10000
  } else if (y_m/1000 >= 5) {
     per <- 1000 
  } else if (y_m/100 >= 5) {
     per <- 100 
  } else if (y_m/100 < 5 & y_m/50 >=5) {
     per <- 50 
  } else {
     per <- 10
  } 

  y_m1 <- y_m +per  

  #windows(height = h, width = w)
  barplot(a$Observed, space = 0, col = bar.color1, ylim = c(0,y_m1), 
          names.arg = eec, xlab = "Abundance Octave", ylab = "Number of Species", yaxt = "n", cex.lab = 1.2, cex.axis = 1)
  axis(1, at = 0:eem, label = F)
  axis(2, at = seq(0, y_m1, per), las = 2, ylab = "Number of Species", cex.lab = 1.2, cex.axis = 1)
  box()
  #axis(3, cex.axis = 1)
  #axis(4, cex.axis = 1)
  points(x = b$row, y = b$Fitted, pch = 1, col = line.color.addPoi1)
  lines(x = b$row, y = b$Fitted, lwd = 2, col = line.color.addPoi1)
  points(x = d$row, y = d$Fitted, pch = 1, col = line.color.addMax1)
  lines(x = d$row, y = d$Fitted, lwd = 2, col = line.color.addMax1)
  legend("topright", legend=c("Traditional binning", "Without binning"),
         col=c(line.color.addPoi1, line.color.addMax1), lty=1, cex=1.2, lwd = 2,
         bty = "n")
  title(main = "Species Abundance Distribution Maximization of Log-likelihood", cex.lab = 1.5)

  dev.off()
  print("really?")
  return(.set.mSet(mSetObj))  
}

#'Perform rank abundance or dominance/diversity models
#'@description rank abundance distribution graph 
#'@param mSetObj Input name of the created mSet Object
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
AbundanceRankPlot <- function(mSetObj=NA, imgName, format="png", dpi=72, width=NA) {
  
  #library(plyr)
  #library(vegan)
  #library(dplyr)
  library(ggplot2)
  library(svglite)
  print("ggplot2")
  
  mSetObj <- .get.mSet(mSetObj)
  
  mod.radfit <- mSetObj$analset$result$mod.radfit
  print(mod.radfit)
  

  null.data <- cbind(mod.radfit$models$Null$y, mod.radfit$models$Null$fitted.values)
  null.c <- as.numeric(nrow(null.data))  
  null.c.data <- cbind("Null", null.data, c(1:null.c))
  colnames(null.c.data) <- c("Index", "yvalue", "Fitted", "Rank")

  prep.data <- cbind(mod.radfit$models$Preemption$y, mod.radfit$models$Preemption$fitted.values)
  prep.c <- as.numeric(nrow(prep.data)) 
  prep.c.data <- cbind("Prep", prep.data, c(1:prep.c))
  colnames(prep.c.data) <- c("Index", "yvalue", "Fitted", "Rank")
  
  log.data <- cbind(mod.radfit$models$Lognormal$y, mod.radfit$models$Lognormal$fitted.values)
  log.c <- as.numeric(nrow(log.data))  
  log.c.data <- cbind("Lognormal", log.data, c(1:log.c))
  colnames(log.c.data) <- c("Index", "yvalue", "Fitted", "Rank")

  zipf.data <- cbind(mod.radfit$models$Zipf$y, mod.radfit$models$Zipf$fitted.values)
  zipf.c <- as.numeric(nrow(zipf.data))  
  zipf.c.data <- cbind("Zipf", zipf.data, c(1:zipf.c))
  colnames(zipf.c.data) <- c("Index", "yvalue", "Fitted", "Rank")
  
  man.data <- cbind(mod.radfit$models$Mandelbrot$y, mod.radfit$models$Mandelbrot$fitted.values)
  man.c <- as.numeric(nrow(man.data))  
  man.c.data <- cbind("Mandelbrot", man.data, c(1:man.c))
  colnames(man.c.data) <- c("Index", "yvalue", "Fitted", "Rank")
  
  total <- as.data.frame(rbind(null.c.data, prep.c.data, log.c.data, zipf.c.data, man.c.data))
  total
  print(total)
  rownames(total) <- c(1:nrow(total))
  total$Index <- as.factor(total$Index)
  total$yvalue <- as.numeric(total$yvalue)
  total$Fitted <- as.numeric(total$Fitted)
  total$Rank <- as.numeric(total$Rank)

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
  mSetObj$imgSet$AbundanceRankPlot <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  #plot(yvalue ~ Rank, data = total)

  #windows ( w = 10, h = 10)
  gg <- ggplot(total, aes(x = Rank, y = log(yvalue), group = 1)) +
    geom_point(aes(color = Index)) +
    facet_grid(Index ~ ., scales = "free_y") +
    geom_line(aes(x = Rank, y = log(Fitted), color = Index)) 
  #print(gg + labs(y="Abundance (log)", x = "Rank"))
  
  if (format == "png") {
     png(imgName)
  } else if (format == "tiff") {
     tiff(imgName)
  } else if (format == "svg") {
     ggsave(file = "imgName.svg", plot = gg + labs(y="Abundance (log)", x = "Rank"))
  } else if (format == "pdf") {
     ggsave(file = "imgName.pdf") 
  } 

  print(gg + labs(y="Abundance (log)", x = "Rank"))
  #print(gg)

  dev.off()
  
  return(.set.mSet(mSetObj))

}