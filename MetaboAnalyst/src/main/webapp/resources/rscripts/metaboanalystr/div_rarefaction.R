#'Perform Rarefaction
#'@description Perform rarefaction technique 
#'@param mSetObj Input name of the created mSet Object
#'@param data Boolean for which data set to use, normalized (default) or original
#'@param type Set type for different types of rarefied community data, drop down options are "Species richness" (default), "Random rarefaction" or "Probability"
#'@param sample Input subsample size for rarefying community, either a single value or a vector.default is not apply subsample
#'@param se Input estimate standard errors, drop down options are "TRUE" or "FALSE" (default). 
#'@param margin Input the index is computed, drop down options are "1" (default) or "2" 
#'@author Shiyang Zhao\email{shiyang1@ualberta.ca}
#'University of Alberta, Canada
#'License: GNU GPL (>= 2) ######
#'@export


Rarefaction_div <- function(mSetObj = NA, data = "false", type = "NULL", sample = "", se = "false", margin = "NULL") {
  
  options(errors = traceback)
  #print("inside faction R file")

  #library("ade4")
  #library("adegraphics")
  #install.packages("plyr")
  library("plyr")
  #print("library plyr")
  library("dplyr")
  library("vegan")
  
  print("setup mSetObj")


  #Extract input from mSetObj
  mSetObj <- .get.mSet(mSetObj)

  metaData <- mSetObj$dataSet$origMeta
  #envData <- mSetObj$dataSet$origEnv
 
  print("BEFORE DATA TESTS")
  print(metaData)
  #print(envData)

  if (data == "false") { #normalized data as input
    input <- mSetObj$dataSet$norm
  } else { #original data as input
    input <- mSetObj$dataSet$orig
  }
  #input <- as.data.frame(input)
  
  print("margin setup")
  # margin = 1, data in rows; margin = 2, data in columns
  if (margin == "NULL") { 
    margin1 <- "1"
  } else if (margin == "2") {
    margin1 <- "2"  
  }
  margin1 <- as.numeric(margin1)
  print(margin1)  

  cat("after if margin statement")
  #sample <- as.numeric(sample)

  print("before se")
  if (se == "false") {
    se1 = FALSE
  } else {
    se1 = TRUE
  } 

  cat("Rarefaction technique works on numeric data only")
  input.2 <- select_if(input, is.numeric)

  print("before real analysis")
  if (type == "NULL") {
    type1 <- "Rarefaction"
    if (sample == "") {
    #the minimum sample count achieved over the selected data columns
      sample1 <- as.numeric(min(rowSums(input))) 
    } else {
      sample1 = as.numeric(sample)
    }
    Srare <- rarefy(input.2, sample = sample1, se = se1, MARGIN = margin1)
    cat ("Expected species richness in random subsamples of size 'samples' from the community")
    mSetObj$analset$result$name <- "Rarefaction"
    mSetObj$analset$result$type <- type1
    mSetObj$analset$result$sample_size <- sample1
    mSetObj$analset$result$margin <- margin1
    mSetObj$analset$result$Output <- Srare
    mSetObj$analset$Srare <- mSetObj$analset$result$Output
  } else if (type == "Random rarefaction") {
    if (sample == "") {
    #the minimum sample count achieved over the selected data columns
      sample1 <- as.numeric(min(rowSums(input))) 
    } else {
      sample1 = as.numeric(sample)
    }
    Srare <- rrarefy(input.2, sample = sample1)
    cat("Generates one randomly rarefied community data frame or vector of given 'sample' size")
    mSetObj$analset$result$name <- "Rarefaction"
    mSetObj$analset$result$type <- type
    #mSetObj$analset$result$sample_size <- sample1
    mSetObj$analset$result$margin <- margin1
    mSetObj$analset$result$Output <- Srare 
    Srare.frame <- as.data.frame(cbind(mSetObj$analset$result$name, mSetObj$analset$result$type, 
                                 mSetObj$analset$result$sample <- sample1, mSetObj$analset$result$Output <- Srare))
    colnames(Srare.frame)[1:3] <- c("name", "type", "Sample_size")
    mSetObj$analset$Srare <- Srare.frame 
  } else if (type == "Probability") {
    if (sample == "") {
    #the minimum sample count achieved over the selected data columns
      sample1 <- as.numeric(min(rowSums(input)))
    } else {
        sample1 = as.numeric(sample)
    }
    Srare <- drarefy(input.2, sample = sample1)
    cat("Returns probabilities that species occur in a rarefied community of size 'sample'") 
    mSetObj$analset$result$name <- "Rarefaction"
    mSetObj$analset$result$type <- type
    mSetObj$analset$result$sample <- sample1
    mSetObj$analset$result$margin <- margin1
    mSetObj$analset$result$Output <- Srare 
    Srare.frame <- as.data.frame(cbind(mSetObj$analset$result$name, mSetObj$analset$result$type, 
                                       mSetObj$analset$result$sample <- sample1, mSetObj$analset$result$Output <- Srare))
    mSetObj$analset$Srare <- Srare.frame
    colnames(mSetObj$analset$Srare)[1:3] <- c("name", "type", "Sample_size")
  }

  print(sample)
  print("transfer data")
  print(Srare)
  #mSetObj$analset$result$name <- "Rarefaction"
  #mSetObj$analset$result$type <- type
  #mSetObj$analset$result$sample <- sample1
  #mSetObj$analset$result$Output <- Srare
  
  mSetObj$analset$input <- input.2
  print(input.2)
  
  write.csv(mSetObj$analset$result, "Rarefaction.csv")
  
  return(.set.mSet(mSetObj)) 

}


#'Produce a rarefaction curve for each row of the input data. 
#'@description Produce rarefaction curve plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color options include ("darkred", "forestgreen", "hotpink", "blue") (default), "plasma" and "grayscale"
#'@param color_text Input name of the user selected colors' names. 
#'@param step step size for sample sizes in rarefaction curves, user input the number of step.
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

RarefactionCurve <- function(mSetObj=NA, step = "", color="NULL", imgName, format="png", dpi=72, width=NA) {
  
  library(vegan)
  
  mSetObj <- .get.mSet(mSetObj)
  
  print("Get ready for plotting")
 
  sample1 <- mSetObj$analset$result$sample_size
  input.p <- mSetObj$analset$input
  type <- mSetObj$analset$result$type
  
  #print("get richness value")
  #S <- specnumber(input.2)
  
  print("width and height")
  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  print("image name")
  #Name plot for download
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$Plot.RarefactionCurve <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  
  #n <- length(input.p[1,])
  if (color == "NULL") { 
    color1 <- c("grey27", "green", "lightpink", "lightcoral", "midnightblue",
                "mediumpurple", "maroon", "lightyellow", "turquoise3", "tan3", 
                "springgreen", "thistle1", "sienna3", "orange", "dimgray",
                "burlywood", "darksalmon", "deeppink", "goldenrod", "forestgreen")
  } else if (color == "red") {
     color1 = "red"
  } else if (color == "plasma") {
     color1 <- c("yellow", "pink")
  } else if (color == "rainbow") { 
     color1 <- c("orange", "brown")
  }
  print(color1)  

  pars <- expand.grid(col = color1, stringsAsFactors = FALSE)
  
  print("set up step")
  if (step == "") {
    step1 = 1
    step1 <- as.numeric(step1)
  } else {
    step1 <- as.numeric(step) 
  }  
  print(step)
  
  print("actually plotting")
  print(input.p)
  #windows(width = w, height = h)
  rarecurve(input.p, step = step1, sample = sample1, col = color1, label = T, yaxt = "n")
  axis(2, las = 2, labels = T)
  title = "Rarefaction Curve Plot" 
  
  dev.off()
 
  return(.set.mSet(mSetObj))
}


#'Produce a plot for rarefied species richness and un-rarefied richness 
#'@description Produce rarefaction curve plot with user selected options
#'@param mSetObj Input name of the created mSet Object
#'@param color options include ("darkred", "forestgreen", "hotpink", "blue") (default), "plasma" and "grayscale"
#'@param color_text Input name of the user selected colors' names. 
#'@param step step size for sample sizes in rarefaction curves, user input the number of step.
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

RarefactionPlot <- function(mSetObj = NA, colorb = "NULL", imgName, format = "png", dpi = 72, width = NA) {
  
  library(vegan)
  
  cat("Linear plot only works for univariate analysis")
  mSetObj <- .get.mSet(mSetObj)
  #type.p <- mSetObj$analset$Srare$type
  type.p <- mSetObj$analset$result$type
  plot_data <- mSetObj$analset$result$Output
  input.p <- mSetObj$analset$input
  margin.p <- as.numeric(mSetObj$analset$result$margin)
  print("gather plot data") 
  print(margin.p) 
  

  #if (MARGIN2 == "2") {
  #  plot_data <- mSetObj$analset$result$Output[1,]
  #} else {
  #  plot_data <- mSetObj$analset$result$Output
  #}
  
  if (type.p == "Rarefaction") {
    S <- specnumber(input.p, MARGIN = margin.p)
  }  else {
    S <- specnumber(input.p)
  }
  print(type.p)

  #Set plot dimensions
  if(is.na(width)){
    w <- 10.5
  } else if(width==0){
    w <- 7.2
  } else{
    w <- width
  }
  h <- w
  
  print("Name plot for download")
  imgName <- paste(imgName, "dpi", dpi, ".", format, sep="")
  mSetObj$imgSet$Plot.Rarefaction <- imgName
  
  Cairo::Cairo(file=imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg="white")
  par(xpd=FALSE, mar=c(5.1, 4.1, 4.1, 2.1))
  #abline(0, 1)
  
  if(colorb == "NULL") { 
    colorb1 <- "black" #default fill palette is grayscale
  } else if (colorb == "gray") {
     colorb1 <- "gray" 
  } else if (colorb == "blue") {
     colorb1 <- "blue"
  } else if (colorb == "red") { #manual user entry. Selection of this option causes text box to appear
     colorb1 <- "red"
  } 
  print(colorb1)

  pars <- expand.grid(col = colorb1, stringsAsFactors = FALSE)
  
  #if (is.na(step)) {
  #  cat("step has to be numeric data and can't be blank")
  #} else {
  #  step1 = as.numeric(step) 
  #}  
  
  #windows(width = w, height = h)
  minnx = round(as.numeric(min(S)-5), digits = -1)
  if (minnx >= 0) {
     minnx1 <- minnx
  } else {
     minnx1 = 0
  } 
  print(minnx1)
  maxnx = round(as.numeric(max(S)+5), digits = -1)
  minmy = round(as.numeric(min(plot_data)-10), digits = -1)
  if (minmy >= 0) {
     minmy1 <- minmy
  } else {
     minmy1 = 0
  }
  print(minmy1)
  maxmy = round(as.numeric(max(plot_data)+10), digits = -1)
  #windows(width = w, height = h)
  plot(plot_data ~ S, ann = F, axes = F, xlim = c(minnx1, maxnx), ylim = c(minmy1, maxmy),
       col = colorb1, xlab = "Observed No. of Species", ylab = "Rarefied No. of Species")
  axis(1, labels = T, at = seq(minnx1, maxnx, by = 5))
  axis(2, las = 2, at = seq(minmy1, maxmy, by = 5) )
  #abline(0,1)
  title = "Rarefaction Linear Plot"

  dev.off()
  
  return(.set.mSet(mSetObj))
}

