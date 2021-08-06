#'Perform species abundance distribution
#'@description Perform rarefaction technique 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param type Set type for different methods for abundance models, drop down options are "Fisher's logseries" (default), "Split or Keep" or "Maximize"
#'@param community Input for abundance model, drop down options are "Whole Dataset" (default), or "colSums": form row and column sums and means for numeric arrays or data frames
#'@param row_num Boolean for the beginning (first) of selected rows, numeric data
#'@param row_num1 Boolean for the end (last) of selected rows, numeric data 
#'@param col_num Boolean for the beginning (first) of selected columns, numeric data
#'@param col_num1 Boolean for the end (last) of selected columns, numeric data 
#'@param tiesplit Input split frequencies, drop down options are "TRUE" or "FALSE" (default)
#'@param truncate Boolean for truncation point for log-Normal model, in log2 units, default value is -1
#'@param MARGIN Input the index is computed, drop down options are "Rows" (default), "Columns" or "Columns & Rows".
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export

#data("BCI")

#mSetObj=list()
#mSetObj$dataSet$norm=BCI
#mSetObj$dataSet$orig=BCI
#data = "false"
#type = "NULL"
#community = ""
#row_num = ""
#row_num1 = ""
#col_num = ""
#col_num1 = ""
#tiesplit = "false"
#truncate = ""
#margin = "NULL" 

AbundanceModel <- function(mSetObj = NA, data = "false", community = "", tiesplit = "false", truncate = ""){
                            
  #library("ade4")
  #library("adegraphics")
  library("plyr")
  library("dplyr")
  library("vegan")
  
  #Extract input from mSetObj
  
  #FOR TESTING
  #metaData <- "NULL"
  #metaData <- .readDataTable("/home/louisa/Wegan/Wegan/MetaboAnalyst/src/main/webapp/resources/rscripts/metaboanalystr/test_data/dune_meta.csv")
  #envData <- .readDataTable("/home/louisa/Wegan/Wegan/MetaboAnalyst/src/main/webapp/resources/rscripts/metaboanalystr/test_data/dune_env.csv")
  #envData$Moisture <- as.numeric(as.character(envData$Moisture)) #Require more than one numeric envData column for numeric type option
  
  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  
#  if (margin == "NULL") {
#    if (row_num == "") {
#      input.2 <- select_if(input, is.numeric)
#    } else { 
#      if (row_num1 == "") {
#      row_num <- as.numeric(row_num)
#      input.2 <- select_if(input[row_num, ], is.numeric)
#      } else {
#      row_num <- as.numeric(row_num)
#      row_num1 <- as.numeric(row_num1)
#      input.2 <- select_if(input[row_num:row_num1, ], is.numeric)
#     }
#   }
#  } else if (margin == "Columns") {
#    if (col_num == "") {
#      input.2 <- select_if(input, is.numeric)
#    } else { 
#      if (col_num1 == "") {
#        col_num <- as.numeric(col_num)
#        input.2 <- select_if(input[, col_num], is.numeric)
#      } else {
#        col_num <- as.numeric(col_num)
#        col_num1 <- as.numeric(col_num1)
#        input.2 <- select_if(input[, col_num:col_num1], is.numeric)
#      }
#    }
#  } else if (margin == "Columns & Rows") {
#      col_num <- as.numeric(col_num)
#      col_num1 <- as.numeric(col_num1)
#      row_num <- as.numeric(row_num)
#      row_num1 <- as.numeric(row_num1)
#      input.2 <- select_if(input[row_num:row_num1, col_num:col_num1], is.numeric)
#  }
  
  if (tiesplit == "false") {
    tiesplit1 = FALSE
  } else {
    tiesplit = TRUE
  }
  
  if(truncate == "")  {
    truncate1 = -1
  } else {
    truncate1 = as.numeric(truncate)
  }
  
  if(community == "") {
    input.c <- input.2
  } else {
    community1 <- as.numeric(community)
    input.c <- input.2[community1, ]
  }
  
  output.fisher <- fisherfit(input.2)
  output.fit <- prestonfit(input.2, tiesplit = tiesplit1)
  output.distr <- prestondistr(input.2, truncate = truncate1)
  output.ve.fit <- veiledspec(output.fit)
  output.ve.distr <- veiledspec(output.distr)
  
  
  mSetObj$analset$result$fisher$name <- "Species Abundance Model"
  mSetObj$analset$result$fisher$type <- "Fisher's logseries"
  mSetObj$analset$result$fisher$estimate <- output.fisher$estimate
  mSetObj$analset$result$fisher$No.of.species <- sum(output.fisher$fisher)
  mSetObj$analset$result$fisher$freq <- output.fisher$fisher
  mSetObj$analset$result$fisher$output <- output.fisher
  #mSetObj$analset$result$fisher$freq <- 
  
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
  
  
  #write.csv(mSetObj$analset$result$fisher$freq, "Fisher frequency table.csv")
  write.csv(mSetObj$analset$result$fisher$com.result, "Fisher log series model output.csv")
  write.csv(mSetObj$analset$result$lognormal_fit$coeffi, "Poisson coefficients.csv")
  write.csv(mSetObj$analset$result$lognormal_fit$frequencies, "Poisson frequencies by Octave.csv")
  write.csv(mSetObj$analset$result$lognormal_fit$output.r, "Poisson_Total extrapolated richness.csv")
  write.csv(mSetObj$analset$result$log_likelihood$coeffi, "Max_likelihood coefficients.csv")
  write.csv(mSetObj$analset$result$log_likelihood$frequencies, "Max_likelihood frequencies by Octave.csv")
  write.csv(mSetObj$analset$result$log_likelihood$output.r, "Max_likelihood_Total extrapolated richness.csv")
  
  
#  if (type == "NULL") {
#      cat("Fisher's logseries")
#      output <- fisherfit(input.2)
#  } else if (type == "Split or Keep") {
#    if (community == "NULL") {
#        cat("Split the ties or keep all limit cases in the lower octave")
#        output <- prestonfit(input.2, tiesplit = tiesplit1)
#    } else {
#        cat("Split the ties or keep all limit cases in the lower octave")
#        output <- prestonfit(colSums(input.2), tiesplit = tiesplit1)
#    }
#  } else if (type == "Maximize") {
#    if (community == "NULL") {
#      cat("Maximizes truncated log-normal likelihood without binning data")
#      output <- prestondistr(input.2)
#    } else {
#      cat("Maximizes truncated log-normal likelihood without binning data")
#      output <- prestondistr(colSums(input.2))
#    }
#  }
  
#  if (type != "NULL") {
#  output.r <- veiledspec(output)
#  }
  
#  if (type == "NULL") {
#    mSetObj$analset$result$name <- "Species Abundance Model"
#    mSetObj$analset$result$type <- "Fisher's logseries"
#    mSetObj$analset$result$estimate <- output$estimate
#    mSetObj$analset$result$No.of.species <- sum(output$fisher)
#    mSetObj$analset$result$freq <- output$fisher
#    mSetObj$analset$result$output <- output
#  } else {
#    mSetObj$analset$result$name <- "Species Abundance Model"
#    mSetObj$analset$result$type <- output$method
#    mSetObj$analset$result$fitted <- output$fitted
#    mSetObj$analset$result$freq <- output$freq
#    mSetObj$analset$result$coefficients <- output$coefficients
#    mSetObj$analset$result$output <- output
#    mSetObj$analset$result$output.r <- output.r
#  }

#  write.csv(mSetObj$analset$result$output.r, "Extrapolated richness.csv")
#  write.csv(mSetObj$analset$result$coefficients, "Coefficients.csv")
  
  return(.set.mSet(mSetObj)) 
  
}

#basebar = "Maximized likelihood"
#add.line = "Both"
#bar.color = "slateblue"
#color_text = "NULL"
#line.color.addFit = "coral"
#line.color_text = "NULL"
#imgName = "NULL"
#format = "png"
#dpi = 72
#width = NA

AbundanceFisherPlot <- function(mSetObj = NA, bar.color = "NULL", line.color.addFit = "NULL", 
                           imgName, format="png", dpi=72, width=NA) {
  
  library(vegan)
  
  mSetObj <- .get.mSet(mSetObj)
  
  plot_data.fisher <- mSetObj$analset$result$fisher$output
  
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
  
  
#  if(color=="NULL") { 
#    color1 = "blue" #default fill palette is grayscale
#  } else if (color=="manual") { #manual user entry. Selection of this option causes text box to appear
#    color1 <- "manual"
#    color_text1 <- color_text #colors entered by user as string with commas between colors
#    color_text1 <- gsub("\n", "", color_text1, fixed=TRUE) #Prepare colors list, fixed=TRUE means we are dealing with one string, versus a vector of strings (fixed=FALSE)
#    color_text1 <- gsub(";", ",", color_text1, fixed=TRUE)
#    color_text1 <- gsub(" ", "", color_text1, fixed=TRUE)
#    color_text1 <- unlist(strsplit(color_text1, split=","))
#  } else { #User selected color palette
#    color1 <- color #user selects color palette from drop down menu (options are grayscale, viridis, plasma)
#  }
  
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
    line.color.addFit1 = "red" #default fill palette is grayscale
  } else if (line.color.addFit == "coral") { #manual user entry. Selection of this option causes text box to appear
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
  

  #pars <- expand.grid(col = color, stringsAsFactors = FALSE)
  
  windows(height = h, width = w)
  plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
       line.col = line.color.addFit1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
  axis(1, labels = T)
  axis(2, las = 2)
  
#  if (basebar == "NULL") {
#    if (add.line == "NULL") {
#      plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#         line.col = line.color.addFit1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#      axis(1, labels = T)
#      axis(2, las = 2)
#    } else if (add.line == "Poisson fit") {
#      plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#           line.col = line.color.addFit1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#      axis(1, labels = T)
#      axis(2, las = 2)
#      lines(plot_data.fit, line.col = line.color.addPoi1)
#    } else if (add.line == "Maximized likelihood") {
#      plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#           line.col = line.color1.addFit1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#      axis(1, labels = T)
#      axis(2, las = 2)
#      lines(plot_data.distr, line.col = line.color.addMax1)
#    } else if (add.line == "Both") {
#      plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#           line.col = line.color.addFit1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#      axis(1, labels = T)
#      axis(2, las = 2)
#      lines(plot_data.fit, line.col = line.color.addPoi1)
#      lines(plot_data.distr, line.col = line.color.addMax1)
#    }
#  } else if (basebar == "Possion fit") {
#    if (add.line == "NULL") {
#      plot(plot_data.fit, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#           line.col = line.color.addPoi1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#      axis(1, labels = T)
#      axis(2, las = 2)
#    } else if (add.line == "Maximized likelihood") {
#      plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#           line.col = line.color.addPoi1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#      axis(1, labels = T)
#      axis(2, las = 2)
#      lines(plot_data.distr, line.col = line.color.addMax1)
#    } else if (add.line == "Fisher alpha") {
#      plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#           line.col = line.color.addPoi1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#      axis(1, labels = T)
#      axis(2, las = 2)
#      lines(plot_data.fisher, line.col = line.color.addFit1)
#    } else if (add.line == "Both") {
#      plot(plot_data.fisher, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#           line.col = line.color.addPoi1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#      axis(1, labels = T)
#      axis(2, las = 2)
#      lines(plot_data.fisher, line.col = line.color.addFit1)
#      lines(plot_data.distr, line.col = line.color.addMax1)
#    }
# } else if (basebar == "Maximized likelihood") {
#      if (add.line == "NULL") {
#        plot(plot_data.distr, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#             line.col = line.color.addMax1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#        axis(1, labels = T)
#        axis(2, las = 2)
#      } else if (add.line == "Possion fit") {
#        plot(plot_data.distr, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#             line.col = line.color.addMax1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#        axis(1, labels = T)
#        axis(2, las = 2)
#        lines(plot_data.fit, line.col = line.color.addPoi1)
#      } else if (add.line == "Fisher alpha") {
#        plot(plot_data.distr, ann = T, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n",
#             line.col = line.color.addMax1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#        axis(1, labels = T)
#        axis(2, las = 2)
#        lines(plot_data.fisher, line.col = line.color.addFit1)
#      } else if (add.line == "Both") {
#        plot(plot_data.distr, #ann = T, axes = F, pch = 17, 
#             lty = 1, lwd = 2, cex = 2, xant = "n", yant = "n", labels = F,
#             line.col = line.color.addMax1, bar.col = bar.color1, xlab = "Frequency", ylab = "Species")
#        axis(1, labels = F)
#        axis(2, las = 2)
#        lines(plot_data.fisher, line.col = line.color.addFit1)
#        lines(plot_data.fit, line.col = line.color.addPoi1)
#    }
# }
  
  dev.off()
  
  return(.set.mSet(mSetObj))
}



#bar.color = "wheat"
#color_text = "NULL"
#line.color = "NULL"
#line.color_text = "NULL"
#line.color.addPoi = "olive"
#line.color.addMax = "hotpink"
#imgName = "NULL"
#format = "png"
#dpi = 72
#width = NA

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
  
  
  #pars <- expand.grid(col = color1, stringsAsFactors = FALSE)
  #xant = "n", yant = "n",
  windows(height = h, width = w)
  #if (color1 == "NULL") {
  plot(plot_data.distr, ann = F, pch = 17, lty = 1, lwd = 2, cex = 2, yaxt = "n", xaxt = "n", labels = F,
       line.col = line.color.addPoi1, bar.col = bar.color1,  ylab = "Species")
  #axis(1, labels = T, xlab = "Frequency",)
  #axis(2, las = 2)    
  lines(plot_data.fit, line.col = line.color.addMax1)
#  #} else {
# #  plot(plot_data.distr, ann = F, axes = F, pch = 17, lty = 1, lwd = 2, cex = 2, bar.col = bar.color1)
#  #  lines(plot_data.fit, line.col = line.color)
#  #}
#  
  dev.off()
  
  return(.set.mSet(mSetObj))
}


